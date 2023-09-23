module TheBlunt

open System

type ParserFunction<'value> = Cursor -> ParserResult<'value>

and [<Struct>] Cursor =
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

#if USE_INLINEIFLAMBDA

type Parser<'value> = ParserFunction<'value>

[<AutoOpen>]
module ParserHandling =
    let inline mkParser (parser: _ Parser) = parser
    let inline getParser (parser: _ Parser) = parser

module Inline =
    type IfLambdaAttribute = FSharp.Core.InlineIfLambdaAttribute

#else

type Parser<'value> = Parser of ParserFunction<'value>

[<AutoOpen>]
module ParserHandling =
    let inline mkParser parserFunction = Parser parserFunction
    let inline getParser (parser: _ Parser) = let (Parser p) = parser in p

module Inline =
    type IfLambdaAttribute() = inherit System.Attribute()

#endif

type [<Struct>] DocPos =
    { index: int
      line: int
      column: int }

type CanParse = CanParse

type [<Struct>] ForControl<'item> =
    | Item of item:'item
    | Append of value:'item
    | Break
    // | Fwd of offset:int
    // | Next

open System.Runtime.CompilerServices

[<Extension>]
type StringExtensions =
    [<Extension>] static member Equals(s: Str, compareWith: string)
        = s.Span.SequenceEqual(compareWith.AsSpan())
    [<Extension>] static member Equals(s: Str, compareWith: Str) 
        = s.Span.SequenceEqual(compareWith.Span)
    [<Extension>] static member Equals(s: string, compareWith: Str) 
        = s.AsSpan().SequenceEqual(compareWith.Span)
    [<Extension>] static member Equals(s: string, compareWith: string) 
        = String.Equals(s, compareWith)
    [<Extension>] static member EqualsAt(this: Str, other: Str, idx: int) 
        = idx + other.Length <= this.Length && this.Slice(idx, other.Length).Equals(other)

module Str =
    let empty = "".AsMemory()

type Cursor with
    static member Create(text, idx) = { index = idx; text = text }
    member c.CanGoto(idx: int) =
        // TODO: Should be: Only forward
        idx >= 0 && idx <= c.text.Length
    member c.IsAtEnd =
        c.index = c.text.Length
    member c.HasRest = c.index < c.text.Length
    // TODO: This should be a "pgoto" parser
    member c.Goto(idx: int, f) =
        // TODO: this propably would be a fatal, most propably an unexpected error
        if c.CanGoto(idx)
        then f idx
        else
            let msg = $"Index {idx} is out of range of string of length {c.text.Length}."
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
                    if input.EqualsAt("\n".AsMemory(), currIdx)
                    then line + 1, columnStart
                    else line, column + 1
                findLineAndColumn (currIdx + 1) line column
        findLineAndColumn 0 lineStart columnStart

    let ofInput (pi: Cursor) = create pi.index pi.text

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
        if inp.text.EqualsAt(s.AsMemory(), inp.index)
        then POk { index = inp.index + s.Length; value = s }
        else PError { index = inp.index; message = $"Expected: '{s}'" }

let (~%) value = pstr value

type ParserBuilder() =
    let combineItem a b = [ yield! a; yield! b ]
    let appendItem a b = [ yield! a; yield! b ]

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
                          value = combineItem p1Res.value p2Res.value }
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
                        then iter (combineItem currResults res.value) res.index
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
                    then iter (combineItem currResults res.value) res.index
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
                            | Item item -> currResults <- combineItem currResults [item]
                            | Append x -> failwith "TODO"
                            | Break -> shouldBreak <- true
                        if shouldBreak || inp.IsAtEnd
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
        if inp.IsAtEnd
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

// let pSepByStr (p: _ Parser) (sep: _ Parser) =
//     parse {
//         for x
//     }

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
                if x = "a" || x = "b" || x = "c" then
                    yield Item x
                elif x = "X" then
                    yield Break
        }
        |> run "abcdeaXabb"
