#if COMPILED
module TheBlunt
#endif

type Parser<'value> =
    Parser of (ParserInput -> ParserResult<'value>)

and [<Struct>] PVal<'value> =
    {
        index: int
        value: 'value
    }
and ParserInput = 
    {
        index: int
        text: string
    }
and ParserResult<'out> = 
    | POk of ParserResultValue<'out>
    | PError of ParseError
and ParserResultValue<'out> =
    {
        index: int
        value: 'out
    }
and [<Struct>] ParseError =
    { 
        index: int
        message: string 
    }
and [<Struct>] DocPos =
    {
        index: int
        line: int
        column: int 
    }

module String =
    let equalsAt (index: int) (compareWith: string) (value: string) =
        index + compareWith.Length <= value.Length
        && value.Substring(index, compareWith.Length) = compareWith

// TODO: Perf: The parser combinators could track that, instead of computing it from scratch.
module DocPos =
    let create (index: int) (input: string) =
        if index < 0 || index > input.Length then
            failwithf "Index %d is out of range of input string of length %d." index input.Length
        let lineStart,columnStart = 1,1
        let rec findLineAndColumn (currIdx: int) (line: int) (column: int) =
            if currIdx = index then
                { index = index; line = line; column = column }
            else
                let line,column =
                    if input |> String.equalsAt currIdx "\n"
                    then line + 1, columnStart
                    else line, column + 1
                findLineAndColumn (currIdx + 1) line column
        findLineAndColumn 0 lineStart columnStart
    let ofInput (pi: ParserInput) = create pi.index pi.text

let inline mkParser parser = Parser parser
let inline getParser (Parser parser) = parser

let inline bind
    ([<InlineIfLambda>] f: 'a -> _ Parser)
    (parser: _ Parser) 
    =
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

let combine (p1: _ Parser) (p2: _ Parser) =
    mkParser <| fun inp ->
        let p1Res = (getParser p1) inp
        match p1Res with
        | POk p1Res -> 
            let p2Res = (getParser p2) { inp with index = p1Res.index }
            match p2Res with
            | POk p2Res -> POk { index = p2Res.index; value = p1Res.value :: p2Res.value }
            | PError error -> PError error
        | PError error -> PError error

let whileCond (guard: unit -> bool) body =
    mkParser <| fun inp ->
        let rec iter currResults currIdx =
            match guard () with
            | true ->
                let pBody = getParser (body ())
                match pBody { inp with index = currIdx } with
                | PError error -> PError error
                | POk res ->
                    let hasConsumed = res.index > currIdx
                    if hasConsumed then
                        POk { index = currIdx; value = currResults }
                    else
                        iter (res.value :: currResults) res.index
            | false -> POk { index = currIdx; value = currResults }
        iter [] inp.index

type CanParse = CanParse

let whileCanParse (_: unit -> CanParse) body =
    mkParser <| fun inp ->
        let rec iter currResults currIdx =
            let pBody = getParser (body ())
            match pBody { inp with index = currIdx } with
            | PError _ ->
                POk { index = currIdx; value = currResults }
            | POk res ->
                let hasConsumed = res.index > currIdx
                match hasConsumed with
                | false -> POk { index = currIdx; value = currResults }
                | true -> iter (res.value :: currResults) res.index
        iter [] inp.index

let inline run (text: string) (parser: _ Parser) =
    let inp = { index = 0; text = text }
    match (getParser parser) inp with
    | POk res -> Ok res.value
    | PError error ->
        let docPos = DocPos.create error.index text
        Error {| pos = docPos; message = error.message |}

let pstr (s: string) =
    mkParser <| fun inp ->
        if inp.text |> String.equalsAt inp.index s
        then POk { index = inp.index + s.Length; value = s }
        else PError { index = inp.index; message = $"Expected: '{s}'" }

let ( ~% ) value = pstr value

type ParserBuilderBase() =
    member inline _.Bind(p, [<InlineIfLambda>] f) = bind f p
    member _.Return(value) = return' value
    member _.ReturnFrom(value) = value
    member _.Yield(value) = return' value
    member _.YieldFrom(value) = value
    member _.Zero() = zero
    member _.Combine(p1, fp2) = combine p1 (fp2 ())
    member _.Delay(f) = f
    member _.While(guard: unit -> bool, body) = whileCond guard body
    member _.While(guard: unit -> CanParse, body) = whileCanParse guard body
    member _.For(sequence: _ seq, body) =
        let enum = sequence.GetEnumerator()
        whileCond (fun _ -> enum.MoveNext()) (fun () -> body enum.Current)
    // TODO - for what?
    //member _.For(p: _ Parser, body) =

type ParserBuilder() =
    inherit ParserBuilderBase()
    member _.Run(f) = f ()

let parse = ParserBuilder()

let map proj (p: _ Parser) =
    parse {
        let! x = p
        return proj x
    }

type ParserConcatBuilder() =
    inherit ParserBuilderBase()
    member _.Run(f) = f () |> map (fun strings -> strings |> String.concat "")

let joinParse = ParserConcatBuilder()

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

let pblanks = 
    joinParse { 
        while CanParse do yield! % " "
    }

let pblanks1 = 
    joinParse {
        let! b = % " "
        yield b
        while CanParse do
            let! x = % " "
            yield x
    }

// Ok "   "
let res1 = pblanks1 |> run "   xxx"

// Ok " "
let res2 = pblanks1 |> run " xxx"

// Error { message = "Expected: ' '" pos = { index = 0; line = 1; column = 1 } }
let res3 = pblanks1 |> run "xxx"
