module TheBlunt

type [<Struct>] Parser<'value> = Parser of (ParserInput -> ParserResult<'value>)

and [<Struct>] ParserInput =
    { index: int
      text: string }

and [<Struct>] ParserResult<'out> =
    | POk of ok:ParserResultValue<'out>
    | PError of error:ParseError

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

type ForControl<'v> =
    | Store of 'v
    | Goto of int
    | Next
    | Break

open System.Runtime.CompilerServices

[<Extension>]
type StringExtensions =
    [<Extension>]
    static member EqualsAt(value: string, index: int, compareWith: string) =
        // TODO: Use Span
        index + compareWith.Length <= value.Length
        && value.Substring(index, compareWith.Length) = compareWith

// TODO: Perf: The parser combinators could track that, instead of computing it from scratch.
module DocPos =
    let create (index: int) (input: string) =
        if index < 0 || index > input.Length then
            failwithf "Index %d is out of range of input string of length %d." index input.Length

        let lineStart, columnStart = 1, 1
        let rec findLineAndColumn (currIdx: int) (line: int) (column: int) =
            match currIdx = index with
            | true -> { index = index; line = line; column = column }
            | false ->
                let line, column =
                    if input.EqualsAt(currIdx, "\n") 
                    then line + 1, columnStart
                    else line, column + 1
                findLineAndColumn (currIdx + 1) line column
        findLineAndColumn 0 lineStart columnStart

    let ofInput (pi: ParserInput) = create pi.index pi.text

let inline mkParser parser = Parser parser

let inline getParser (Parser parser) = parser

module BuilderBricks =
    let hasConsumed lastIdx currIdx = lastIdx > currIdx

    let inline bind ([<InlineIfLambda>] f: 'a -> _ Parser) (parser: _ Parser) =
        mkParser <| fun inp ->
            match getParser parser inp with
            | POk pRes ->
                let fParser = getParser (f pRes.value)
                fParser { inp with index = pRes.index }
            | PError error -> PError error

    let return' value =
        mkParser <| fun inp -> 
            POk { index = inp.index; value = value }

    let zero = return' ()

    let combine (p1: _ Parser) (p2: _ Parser) reduction =
        mkParser <| fun inp ->
            match getParser p1 inp with
            | POk p1Res ->
                match getParser p2 { inp with index = p1Res.index } with
                | POk p2Res ->
                    POk
                        { index = p2Res.index
                          value = reduction p1Res.value p2Res.value }
                | PError error -> PError error
            | PError error -> PError error

    // TODO: resolve whileCond / whileCanParse redundancy?

    let whileCond (guard: unit -> bool) body idElem reducer =
        mkParser <| fun inp ->
            let rec iter currResults currIdx =
                match guard () with
                | true ->
                    match getParser (body ()) { inp with index = currIdx } with
                    | PError error -> PError error
                    | POk res ->
                        if hasConsumed res.index currIdx
                        then iter (reducer res.value currResults) res.index
                        else POk { index = currIdx; value = currResults }
                | false -> 
                    POk { index = currIdx; value = currResults }
            iter idElem inp.index

    // TODO: We really need that when we have a forParser?
    let whileCanParse (_: unit -> CanParse) body idElem reducer =
        mkParser <| fun inp ->
            let rec iter currResults currIdx =
                match getParser (body ()) { inp with index = currIdx } with
                | PError _ -> 
                    POk { index = currIdx; value = currResults }
                | POk res ->
                    if hasConsumed res.index currIdx
                    then printfn "JA"; iter (reducer res.value currResults) res.index
                    else printfn "NO"; POk { index = currIdx; value = currResults }
            iter idElem inp.index

    let inline forSeq (sequence: _ seq) body idElem reducer =
        let enum = sequence.GetEnumerator()
        whileCond (fun _ -> enum.MoveNext()) (fun () -> body enum.Current) idElem reducer

    let inline forParser (parser: _ Parser) body idElem reducer =
        mkParser <| fun inp ->
            let rec iter currResults currIdx =
                match getParser parser { inp with index = currIdx } with
                | PError err -> PError err
                | POk pRes ->
                    let bodyP = body pRes.value
                    match getParser bodyP { inp with index = pRes.index } with
                    | PError err -> PError err
                    | POk res ->
                        match hasConsumed res.index currIdx with
                        | false -> POk { index = currIdx; value = currResults }
                        | true ->
                            match res.value with
                            | Store v ->
                                if hasConsumed pRes.index  currIdx
                                then iter (reducer v currResults) pRes.index
                                else POk { index = currIdx; value = currResults }
                            | Goto idx ->
                                if idx <= 0
                                then failwith "Backward moving is not allowed"
                                else iter currResults (pRes.index + idx)
                            | Next -> iter currResults pRes.index
                            | Break -> POk { index = currIdx; value = currResults }
            iter idElem inp.index

let inline run (text: string) (parser: _ Parser) =
    let inp = { index = 0; text = text }
    match getParser parser inp with
    | POk res -> Ok res.value
    | PError error ->
        let docPos = DocPos.create error.index text
        Error {| pos = docPos; message = error.message |}

let pstr (s: string) =
    mkParser <| fun inp ->
        if inp.text.EqualsAt(inp.index, s)
        then POk { index = inp.index + s.Length; value = s }
        else PError { index = inp.index; message = $"Expected: '{s}'" }

let (~%) value = pstr value

module Reducers =
    let consReducer a b = a :: b
    let inline plusReducer a b = a + b

type ParserBuilder() =
    let reducer a b = a :: b
    let reducerId = []
    member inline _.Bind(p, [<InlineIfLambda>] f) = BuilderBricks.bind f p
    member _.Return(value) = BuilderBricks.return' value
    member _.ReturnFrom(value) = value
    member _.Yield(value) = BuilderBricks.return' value
    member _.YieldFrom(value) = value
    member _.Zero() = BuilderBricks.zero
    member _.Delay(f) = f
    member _.Run(f) = f ()
    member _.Combine(p1, fp2) = 
        BuilderBricks.combine p1 (fp2 ()) reducer
    member _.While(guard: unit -> bool, body) =
        BuilderBricks.whileCond guard body reducerId reducer
    member _.While(guard: unit -> CanParse, body) =
        BuilderBricks.whileCanParse guard body reducerId reducer
    member _.For(sequence: _ seq, body) =
        BuilderBricks.forSeq sequence body reducerId reducer
    member _.For(parser: _ Parser, body) =
        BuilderBricks.forParser parser body reducerId reducer

let parse = ParserBuilder()

let map proj (p: _ Parser) =
    parse {
        let! x = p
        return proj x
    }

let pignore (p: _ Parser) =
    parse {
        let! _ = p
        return ()
    }

let pblank = pstr " "

// TODO: sepBy
// TODO: skipN
// TODO: Or
// TODO: Choose

//let puntil (until: _ Parser) =
//    parse {
//    }

let anyChar =
    mkParser <| fun inp ->
        if inp.index < inp.text.Length
        then POk { index = inp.index + 1; value = string inp.text[inp.index] }
        else POk { index = inp.index; value = "" }

/// Parse zero or more blanks.
let pblanks =
    parse {
        while CanParse do
            yield! % " "
    }

/// Parse one or more blanks.
let pblanks1 =
    parse {
        let! b = % " "
        yield b
        while CanParse do
            let! x = % " "
            yield x
    }

module Tests =
    let res1 = pblanks1 |> run "   xxx"
    let res2 = pblanks1 |> run " xxx"
    let res3 = pblanks1 |> run "xxx"

    let res4 =
        parse {
            for x in anyChar do
                if x = "a"
                then yield (Store x)
                else yield Next
        }
        |> run "ababababab"
