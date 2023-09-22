module TheBlunt

open System

type [<Struct>] Parser<'value> = Parser of (ParserInput -> ParserResult<'value>)

and [<Struct>] ParserInput =
    { index: int
      text: Str }

and Str = ReadOnlyMemory<char>

and [<Struct>] ParserResult<'out> =
    | POk of ok: ParserResultValue<'out>
    | PError of error: ParseError

and [<Struct>] ParserResultValue<'out> =
    { index: int
      value: 'out }

and [<Struct>] ParseError =
    { index: int
      message: string }

type [<Struct>] DocPos =
    { index: int
      line: int
      column: int }

type CanParse = CanParse

type [<Struct>] ForControl<'v> =
    | Store of value:'v
    | Break
    // | Fwd of offset:int
    // | Next

open System.Runtime.CompilerServices

[<Extension>]
type StringExtensions =
    [<Extension>]
    static member eq(s: Str, compareWith: string) =
        s.Span.SequenceEqual(compareWith.AsSpan())
    [<Extension>]
    static member eq(s: Str, compareWith: Str) =
        s.Span.SequenceEqual(compareWith.Span)
    [<Extension>]
    static member eq(s: string, compareWith: Str) =
        s.AsSpan().SequenceEqual(compareWith.Span)
    [<Extension>]
    static member eq(s: string, compareWith: string) =
        String.Equals(s, compareWith)

module Str =
    // TODO: Use Span
    let equalsAt(this: Str, index: int, compareWith: Str) =
        index + compareWith.Length <= this.Length
        && this.Slice(index, compareWith.Length).eq(compareWith)
    let empty = "".AsMemory()

module Cursor =
    let canGoto (idx: int) (value: Str) = 
        idx >= 0 && idx <= value.Length
    let isAtEnd (idx: int) (value: Str) = 
        idx = value.Length
    let isAtEndOrBeyond (idx: int) (value: Str) = 
        idx >= value.Length
    let hasRest (idx: int) (value: Str) = 
        not (isAtEnd idx value) && canGoto idx value
    let goto (idx: int) f (value: Str) =
        if canGoto idx value
        then f idx
        else
            let msg = $"Index {idx} is out of range of string of length {value.Length}."
            PError  { index = idx; message = msg }

// TODO: Perf: The parser combinators could track that, instead of computing it from scratch.
module DocPos =
    let create (index: int) (input: Str) =
        if index < 0 || index > input.Length then
            failwithf "Index %d is out of range of input string of length %d." index input.Length
        
        let lineStart, columnStart = 1, 1
        let rec findLineAndColumn (currIdx: int) (line: int) (column: int) =
            match currIdx = index with
            | true -> { index = index; line = line; column = column }
            | false ->
                let line, column =
                    if Str.equalsAt(input, currIdx, "\n".AsMemory()) 
                    then line + 1, columnStart
                    else line, column + 1
                findLineAndColumn (currIdx + 1) line column
        findLineAndColumn 0 lineStart columnStart

    let ofInput (pi: ParserInput) = create pi.index pi.text

let inline mkParser parser = Parser parser

let inline getParser (Parser parser) = parser

let hasConsumed lastIdx currIdx = lastIdx > currIdx

let inline bind ([<InlineIfLambda>] f: 'a -> _ Parser) (parser: _ Parser) =
    mkParser <| fun inp ->
        match getParser parser inp with
        | PError error -> PError error
        | POk pRes ->
            let fParser = getParser (f pRes.value)
            fParser { inp with index = pRes.index }

let return' value =
    mkParser <| fun inp -> 
        POk { index = inp.index; value = value }

let map proj (p: _ Parser) =
    mkParser <| fun inp ->
        match getParser p inp with
        | PError error -> PError error
        | POk pRes -> POk { index = pRes.index; value = proj pRes.value }

let pignore (p: _ Parser) =
    p |> map (fun _ -> ())

let inline run (text: string) (parser: _ Parser) =
    let text = text.AsMemory()
    match getParser parser { index = 0; text = text } with
    | POk res -> Ok res.value
    | PError error ->
        let docPos = DocPos.create error.index text
        Error {| pos = docPos; message = error.message |}

let pstr (s: string) =
    mkParser <| fun inp ->
        if Str.equalsAt(inp.text, inp.index, s.AsMemory())
        then POk { index = inp.index + s.Length; value = s }
        else PError { index = inp.index; message = $"Expected: '{s}'" }

let (~%) value = pstr value

type ParserBuilder() =
    let reducer a b = [ yield! a; yield! b ]
    member inline _.Bind(p, [<InlineIfLambda>] f) = bind f p
    member _.Return(value) = return' value
    member _.ReturnFrom(value) = value
    member _.Yield(value) = return' [value]
    member _.YieldFrom(p: _ Parser) =
        mkParser <| fun inp ->
            let pRes = getParser p inp
            match pRes with
            | PError err -> PError err
            | POk pRes -> POk { index = pRes.index; value = [pRes.value] }
    member _.Zero() = return' []
    member _.Delay(f) = f
    member _.Run(f) = mkParser <| fun inp -> getParser (f ()) inp

    member _.Combine(p1, fp2) = 
        mkParser <| fun inp ->
            let p2 = fp2 ()
            match getParser p1 inp with
            | POk p1Res ->
                match getParser p2 { inp with index = p1Res.index } with
                | POk p2Res ->
                    POk
                        { index = p2Res.index
                          value = reducer p1Res.value p2Res.value }
                | PError error -> PError error
            | PError error -> PError error

    // TODO: resolve whileCond / whileCanParse redundancy?
    member _.While(guard: unit -> bool, body) =
        mkParser <| fun inp ->
            let rec iter currResults currIdx =
                match guard () with
                | true ->
                    match getParser (body ()) { inp with index = currIdx } with
                    | PError error -> PError error
                    | POk res ->
                        if hasConsumed res.index currIdx
                        then iter (reducer currResults res.value) res.index
                        else POk { index = currIdx; value = currResults }
                | false -> 
                    POk { index = currIdx; value = currResults }
            iter [] inp.index

    member _.While(guard: unit -> CanParse, body) =
        mkParser <| fun inp ->
            let rec iter currResults currIdx =
                match getParser (body ()) { inp with index = currIdx } with
                | PError _ -> 
                    POk { index = currIdx; value = currResults }
                | POk res ->
                    if hasConsumed res.index currIdx
                    then iter (reducer currResults res.value) res.index
                    else POk { index = currIdx; value = currResults }
            iter [] inp.index

    member this.For(sequence: _ seq, body) =
        let enum = sequence.GetEnumerator()
        this.While(
            (fun _ -> enum.MoveNext()),
            (fun () -> body enum.Current))

    member _.For(loopParser: 'a Parser, body: 'a -> Parser<ForControl<'b> list>) : Parser<'b list> =
        mkParser <| fun inp ->
            let rec iter currResults currIdx =
                match getParser loopParser { inp with index = currIdx } with
                | PError err -> PError err
                | POk loopRes ->
                    let bodyP = body loopRes.value
                    match getParser bodyP { inp with index = loopRes.index } with
                    | PError err -> PError err
                    | POk bodyRes ->
                        let mutable shouldBreak = false
                        let mutable currResults = currResults
                        for command in bodyRes.value do
                            match command with
                            | Store v -> currResults <- reducer currResults [v]
                            | Break -> shouldBreak <- true
                        if shouldBreak || (inp.text |> Cursor.isAtEndOrBeyond bodyRes.index)
                        then POk { index = bodyRes.index; value = currResults }
                        else iter currResults bodyRes.index
            iter [] inp.index

let parse = ParserBuilder()

// TODO: sepBy
// TODO: skipN
// TODO: Or
// TODO: Choose

//let puntil (until: _ Parser) =
//    parse {
//    }

let panyChar =
    mkParser <| fun inp ->
        if inp.text |> Cursor.isAtEnd inp.index
        then POk { index = inp.index; value = "" }
        else POk { index = inp.index + 1; value = inp.text.Slice(inp.index, 1).ToString() }

let pend =
    mkParser <| fun inp ->
        if inp.index = inp.text.Length - 1
        then POk { index = inp.index + 1; value = () }
        else PError { index = inp.index; message = "End of input." }

let pblank = pstr " "
// TODO: blankN

/// Parse at least n or more blanks.
let pblanks n =
    parse {
        for x in 1 .. n do
            yield! pstr " "
        while CanParse do
            yield! pstr " "
    }

// TODO: passable reduce function / Zero for builder, etc.
// Name: Imparsible


module Tests =
    let r = pblanks 1 |> run "       xxx"
    let r = pblanks 1 |> run "   xxx"
    let r = pblanks 1 |> run " xxx"
    let r = pblanks 1 |> run "xxx"
    let r = pblanks 0 |> run "xxx"

    let r =
        parse {
            for x in panyChar do
                if x.eq "a" || x.eq "b" || x.eq "c" then
                    yield Store x
                elif x.eq "X" then
                    yield Break
        }
        |> run "abcdeaXabb"
