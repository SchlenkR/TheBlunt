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
            | true ->
                { index = index
                  line = line
                  column = column }
            | false ->
                let line, column =
                    if input.EqualsAt(currIdx, "\n") then
                        line + 1, columnStart
                    else
                        line, column + 1
                findLineAndColumn (currIdx + 1) line column

        findLineAndColumn 0 lineStart columnStart

    let ofInput (pi: ParserInput) = create pi.index pi.text

let inline mkParser parser = Parser parser
let inline getParser (Parser parser) = parser

let inline bind ([<InlineIfLambda>] f: 'a -> _ Parser) (parser: _ Parser) =
    mkParser
    <| fun inp ->
        match (getParser parser) inp with
        | POk pRes ->
            let fParser = getParser (f pRes.value)
            let fResult = fParser { inp with index = pRes.index }
            fResult
        | PError error -> PError error

let return' value =
    mkParser
    <| fun inp ->
        POk
            { index = inp.index
              value = value }

let zero = return' ()

let combine reduction (p1: _ Parser) (p2: _ Parser) =
    mkParser
    <| fun inp ->
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

let whileCond idElem reducer (guard: unit -> bool) body =
    mkParser
    <| fun inp ->
        let rec iter currResults currIdx =
            match guard () with
            | true ->
                let pBody = getParser (body ())
                match pBody { inp with index = currIdx } with
                | PError error -> PError error
                | POk res ->
                    let hasConsumed = res.index > currIdx
                    if hasConsumed then
                        POk
                            { index = currIdx
                              value = currResults }
                    else
                        iter (reducer res.value currResults) res.index
            | false ->
                POk
                    { index = currIdx
                      value = currResults }
        iter idElem inp.index

type CanParse = CanParse

let whileCanParse idElem reducer (_: unit -> CanParse) body =
    mkParser
    <| fun inp ->
        let rec iter currResults currIdx =
            let pBody = getParser (body ())
            match pBody { inp with index = currIdx } with
            | PError _ ->
                POk
                    { index = currIdx
                      value = currResults }
            | POk res ->
                let hasConsumed = res.index > currIdx
                match hasConsumed with
                | false ->
                    POk
                        { index = currIdx
                          value = currResults }
                | true -> iter (reducer res.value currResults) res.index
        iter idElem inp.index

let inline run (text: string) (parser: _ Parser) =
    let inp =
        { index = 0
          text = text }

    match (getParser parser) inp with
    | POk res -> Ok res.value
    | PError error ->
        let docPos = DocPos.create error.index text
        Error
            {| pos = docPos
               message = error.message |}

let pstr (s: string) =
    mkParser
    <| fun inp ->
        match inp.text.EqualsAt(inp.index, s) with
        | true ->
            POk
                { index = inp.index + s.Length
                  value = s }
        | false ->
            PError
                { index = inp.index
                  message = $"Expected: '{s}'" }

let (~%) value = pstr value

type ParserBuilderBase() =
    member inline _.Bind(p, [<InlineIfLambda>] f) = bind f p
    member _.Return(value) = return' value
    member _.ReturnFrom(value) = value
    member _.Yield(value) = return' value
    member _.YieldFrom(value) = value
    member _.Zero() = zero
    member _.Delay(f) = f
    member _.Run(f) = f ()

module Reducers =
    let consReducer a b = a :: b
    let inline plusReducer a b = a + b

type ParserBuilder() =
    inherit ParserBuilderBase()
    member _.Combine(p1, fp2) = combine Reducers.consReducer p1 (fp2 ())
    member _.While(guard: unit -> bool, body) = whileCond [] Reducers.consReducer guard body
    member _.While(guard: unit -> CanParse, body) = whileCanParse [] Reducers.consReducer guard body
    member _.For(sequence: _ seq, body) =
        let enum = sequence.GetEnumerator()
        whileCond [] Reducers.consReducer (fun _ -> enum.MoveNext()) (fun () -> body enum.Current)

type ParserReduceBuilder() =
    inherit ParserBuilderBase()
    member inline _.Combine(p1, fp2) = combine Reducers.plusReducer p1 (fp2 ())
    member inline _.While(guard: unit -> bool, body) = whileCond "" Reducers.plusReducer guard body
    member inline _.While(guard: unit -> CanParse, body) = whileCanParse "" Reducers.plusReducer guard body
    member inline _.For(sequence: _ seq, body) =
        let enum = sequence.GetEnumerator()
        whileCond "" Reducers.plusReducer (fun _ -> enum.MoveNext()) (fun () -> body enum.Current)

let parse = ParserBuilder()
let joinParse = ParserReduceBuilder()

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

// TODO: SepBy

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
