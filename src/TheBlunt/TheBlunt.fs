module TheBlunt

type Parser<'value> = Parser of (ParserInput -> ParserResult<'value>)

and ParserInput =
    { index: int
      text: string }

and ParserResult<'out> =
    | POk of ParserResultValue<'out>
    | PError of ParseError

and ParserResultValue<'out> =
    { index: int
      value: 'out }

and [<Struct>] ParseError =
    { index: int
      message: string }

[<Struct>]
type DocPos =
    { index: int
      line: int
      column: int }

type CanParse = CanParse

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
    let inline bind ([<InlineIfLambda>] f: 'a -> _ Parser) (parser: _ Parser) =
        mkParser <| fun inp ->
            match (getParser parser) inp with
            | POk pRes ->
                let fParser = getParser (f pRes.value)
                let fResult = fParser { inp with index = pRes.index }
                fResult
            | PError error -> PError error

    let return' value =
        mkParser <| fun inp -> 
            POk { index = inp.index; value = value }

    let zero = return' ()

    let combine (p1: _ Parser) (p2: _ Parser) reduction =
        mkParser <| fun inp ->
            match (getParser p1) inp with
            | POk p1Res ->
                let p2Res = (getParser p2) { inp with index = p1Res.index }
                match p2Res with
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
                    let pBody = getParser (body ())
                    match pBody { inp with index = currIdx } with
                    | PError error -> PError error
                    | POk res ->
                        let hasConsumed = res.index > currIdx
                        if hasConsumed 
                        then POk { index = currIdx; value = currResults }
                        else iter (reducer res.value currResults) res.index
                | false -> 
                    POk { index = currIdx; value = currResults }
            iter idElem inp.index

    let whileCanParse (_: unit -> CanParse) body idElem reducer =
        mkParser
        <| fun inp ->
            let rec iter currResults currIdx =
                let pBody = getParser (body ())
                match pBody { inp with index = currIdx } with
                | PError _ -> POk { index = currIdx; value = currResults }
                | POk res ->
                    let hasConsumed = res.index > currIdx
                    match hasConsumed with
                    | false -> POk { index = currIdx; value = currResults }
                    | true -> iter (reducer res.value currResults) res.index
            iter idElem inp.index

    let inline forSeq (sequence: _ seq) body idElem reducer =
        let enum = sequence.GetEnumerator()
        whileCond (fun _ -> enum.MoveNext()) (fun () -> body enum.Current) idElem reducer

    // let inline forParser (p: _ Parser) body idElem reducer =
    //     let enum = sequence.GetEnumerator()
    //     whileCond (fun _ -> enum.MoveNext()) (fun () -> body enum.Current) idElem reducer

let inline run (text: string) (parser: _ Parser) =
    let inp = { index = 0; text = text }
    match (getParser parser) inp with
    | POk res -> Ok res.value
    | PError error ->
        let docPos = DocPos.create error.index text
        Error {| pos = docPos; message = error.message |}

let pstr (s: string) =
    mkParser
    <| fun inp ->
        match inp.text.EqualsAt(inp.index, s) with
        | true -> POk { index = inp.index + s.Length; value = s }
        | false -> PError { index = inp.index; message = $"Expected: '{s}'" }

let (~%) value = pstr value

module Reducers =
    let consReducer a b = a :: b
    let inline plusReducer a b = a + b

type ParserBuilder() =
    member inline _.Bind(p, [<InlineIfLambda>] f) = BuilderBricks.bind f p
    member _.Return(value) = BuilderBricks.return' value
    member _.ReturnFrom(value) = value
    member _.Yield(value) = BuilderBricks.return' value
    member _.YieldFrom(value) = value
    member _.Zero() = BuilderBricks.zero
    member _.Delay(f) = f
    member _.Run(f) = f ()
    member _.Combine(p1, fp2) = BuilderBricks.combine p1 (fp2 ()) Reducers.consReducer
    member _.While(guard: unit -> bool, body) =
        BuilderBricks.whileCond guard body [] Reducers.consReducer
    member _.While(guard: unit -> CanParse, body) =
        BuilderBricks.whileCanParse guard body [] Reducers.consReducer
    member _.For(sequence: _ seq, body) = BuilderBricks.forSeq sequence body [] Reducers.consReducer

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

//let puntil (until: _ Parser) =
//    parse {
//    }

/// Parse zero or more blanks.
let pblanks =
    joinParse {
        while CanParse do
            yield! % " "
    }

/// Parse one or more blanks.
let pblanks1 =
    joinParse {
        let! b = % " "
        yield b
        while CanParse do
            let! x = % " "
            yield x
    }


module Tests =
    // Ok "   "
    let res1 = pblanks1 |> run "   xxx"

    // Ok " "
    let res2 = pblanks1 |> run " xxx"

    // Error { message = "Expected: ' '" pos = { index = 0; line = 1; column = 1 } }
    let res3 = pblanks1 |> run "xxx"
