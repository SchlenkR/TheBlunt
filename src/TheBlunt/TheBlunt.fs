module TheBlunt

open System
open System.Runtime.CompilerServices

type Str =
    #if !FABLE_COMPILER && NETSTANDARD2_1_OR_GREATER
    System.ReadOnlySpan<char>
    #else
    System.String
    #endif

[<Extension>]
type StringExtensions =

    #if !FABLE_COMPILER && NETSTANDARD2_1_OR_GREATER
    [<Extension>] 
    static member inline StringEquals(s: Str, compareWith: string) = 
        s.SequenceEqual(compareWith.AsSpan())
    [<Extension>] 
    static member inline StringEquals(s: Str, compareWith: Str) =
        s.SequenceEqual(compareWith)
    [<Extension>]
    static member inline StringEquals(s: string, compareWith: Str)  =
        s.AsSpan().SequenceEqual(compareWith)
    #endif
    [<Extension>]
    static member inline StringEquals(s: string, compareWith: string) = 
        String.Equals(s, compareWith)

    #if !FABLE_COMPILER && NETSTANDARD2_1_OR_GREATER
    [<Extension>]
    static member StringStartsWithAt(this: Str, other: Str, idx: int) =
        idx + other.Length <= this.Length
        && this.Slice(idx, other.Length).StringEquals(other)
    [<Extension>] 
    static member StringStartsWithAt(this: Str, other: string, idx: int) =
        this.StringStartsWithAt(other.AsSpan(), idx)
    [<Extension>]
    static member StringStartsWithAt(this: string, other: string, idx: int) =
        this.AsSpan().StringStartsWithAt(other.AsSpan(), idx)
    #else
    [<Extension>]
    static member StringStartsWithAt(this: string, other: string, idx: int) =
        this.Substring(idx).StartsWith(other)
    #endif

    #if !FABLE_COMPILER && NETSTANDARD2_1_OR_GREATER
    [<Extension>]
    static member Slice(this: string, start: int) =
        this.AsSpan().Slice(start)
    #else
    [<Extension>]
    static member Slice(this: string, start: int) =
        this.Substring(start)
    #endif


// -----------------------------------------------------------------------------------------------
// BEGIN :)
// -----------------------------------------------------------------------------------------------


type Parser<'value> = Cursor -> ParserResult<'value>

and [<Struct>] Cursor =
    { original: string
      idx: int }

and [<Struct>] ParserResult<'out> =
    | POk of ok: PVal<'out>
    | PError of error: ParseError

and [<Struct>] PVal<'out> =
    { range: Range
      result: 'out }

and [<Struct>] ParseError =
    { idx: int
      message: string }

and [<Struct>] Range = 
    { startIdx: int
      endIdx: int }

type [<Struct>] DocPos =
    { idx: int
      ln: int
      col: int }

let inline mkParser ([<InlineIfLambda>] parser: Parser<_>) = parser
let inline getParser ([<InlineIfLambda>] parser: Parser<_>) = parser

type Cursor with
    member c.CanGoto(idx: int) =
        // TODO: Should be: Only forward
        idx >= c.idx && idx <= c.original.Length
    member c.CanWalkFwd(steps: int) = c.CanGoto(c.idx + steps)
    member c.IsAtEnd = c.idx = c.original.Length
    member c.HasRest = c.idx < c.original.Length
    member c.Rest : Str = c.original.Slice(c.idx)
    member c.StartsWith(s: string) = c.Rest.StringStartsWithAt(s, 0)
    member c.Goto(idx: int) =
        if not (c.CanGoto(idx)) then
            failwithf "Index %d is out of range of string of length %d." idx c.original.Length
        { idx = idx; original = c.original }
    member c.WalkFwd(steps: int) = c.Goto(c.idx + steps)
    member c.MoveNext() = c.WalkFwd(1)

module Range =
    /// Creates a range from start index (inclusive) to end index (exclusive).
    let inline create startIdx endIdx = { startIdx = startIdx; endIdx = endIdx }
    
    /// A zero-length range at position 0.
    let zero = { startIdx = 0; endIdx = 0 }
    
    /// Combines two ranges into one spanning from r1.startIdx to r2.endIdx.
    let inline add r1 r2 = { startIdx = r1.startIdx; endIdx = r2.endIdx }
    
    /// Merges a list of ranges into one spanning all. Returns Range.zero for empty list.
    let inline merge ranges = 
        match ranges with
        | [] -> zero
        | _ -> ranges |> List.reduce add

module POk =
    let inline createFromRange range result =
        POk { range = range; result = result }
    let inline create startIdx endIdx result =
        createFromRange (Range.create startIdx endIdx) result

module PError =
    let inline create (idx: int) (message: string) =
        PError { idx = idx; message = message }

module PVal =
    /// Maps the result value while preserving the range.
    let inline map ([<InlineIfLambda>] proj) (pval: PVal<'a>) =
        { range = pval.range; result = proj pval.result }
    
    /// Extracts and merges all ranges from a list of PVals. Returns Range.zero for empty list.
    let ranges (pvals: PVal<'a> list) = 
        pvals |> List.map (fun x -> x.range) |> Range.merge
    
    /// Reduces a list of PVals using the given reducer, merging their ranges.
    let reduce (pvals: PVal<'a> list) reducer = 
        let ranges = ranges pvals
        let result = pvals |> List.map (fun x -> x.result) |> List.reduce reducer
        { range = ranges; result = result }

// TODO: Perf: The parser combinators could track that, instead of computing it from scratch.
module DocPos =
    let create (index: int) (input: string) =
        if index < 0 || index > input.Length then
            failwithf "Index %d is out of range of input string of length %d." index input.Length
        let lineStart = 1
        let columnStart = 1
        let mutable currIdx = 0
        let mutable line = lineStart
        let mutable column = columnStart
        while currIdx <> index do
            let isLineBreak = input.StringStartsWithAt("\n", currIdx)
            if isLineBreak then
                line <- line + 1
                column <- columnStart
            else
                column <- column + 1
            currIdx <- currIdx + 1
        { idx = index; ln = line; col = column }
    let ofInput (pi: Cursor) = create pi.idx pi.original

module Cursor =
    let hasRemainingChars n =
        fun (inp: Cursor) ->
            if not (inp.CanWalkFwd n)
            then PError.create inp.idx (sprintf "Expected %d more characters." n)
            else POk.create inp.idx inp.idx ()
    let inline notAtEnd cursor = 
        hasRemainingChars 1 cursor

/// Runs parser p only if the predicate parser succeeds. Does not consume input from pred.
let inline pwhen pred ([<InlineIfLambda>] p) =
    mkParser <| fun inp ->
        match pred inp with
        | PError err -> PError.create inp.idx err.message
        | POk _ -> getParser p inp

/// Creates a parser that only runs if the predicate succeeds.
let inline mkParserWhen pred ([<InlineIfLambda>] pf) =
    pwhen pred <| mkParser pf

/// Monadic bind: runs parser, then applies f to the result to get the next parser.
let inline bind ([<InlineIfLambda>] f) ([<InlineIfLambda>] parser) =
    mkParser <| fun inp ->
        match getParser parser inp with
        | PError error -> PError error
        | POk pRes ->
            let fParser = getParser (f pRes)
            fParser (inp.Goto(pRes.range.endIdx))

/// Computation expression builder for parsers.
type ParserBuilder() =
    member inline _.Bind([<InlineIfLambda>] p, [<InlineIfLambda>] f) =
        bind f p
    member _.Return(pval: PVal<_>) =
        mkParser (fun inp -> POk pval)
    member _.Return(err: ParseError) =
        mkParser (fun inp -> PError err)

/// Parser computation expression. Use let! to sequence parsers.
let parse = ParserBuilder()

/// Creates a stateful parser that yields elements from a sequence one at a time.
let pseq (s: _ seq) =
    let enum = s.GetEnumerator()
    mkParser (fun inp ->
        if enum.MoveNext()
        then POk.create inp.idx inp.idx enum.Current
        else PError.create inp.idx "No more elements in sequence."
    )

/// Runs a parser on the given text string, starting at index 0.
let inline run (text: string) ([<InlineIfLambda>] parser) =
    getParser parser { idx = 0; original = text }

/// Transforms the result of a parser using the projection function.
let inline map ([<InlineIfLambda>] proj) ([<InlineIfLambda>] p) =
    mkParser (fun inp ->
        match getParser p inp with
        | PError error -> PError error
        | POk pres -> POk { range = pres.range; result = proj pres.result }
    )

/// Like map, but the projection receives the full PVal (including range).
let inline mapPVal ([<InlineIfLambda>] proj) ([<InlineIfLambda>] p) =
    mkParser (fun inp ->
        match getParser p inp with
        | PError error -> PError error
        | POk pres -> POk { range = pres.range; result = proj pres }
    )

/// Discards the parser result, returning unit.
let inline pignore ([<InlineIfLambda>] p) =
    map (fun _ -> ()) p

/// Optional parser: always succeeds. Returns ValueSome on success, ValueNone on failure.
let inline ptry ([<InlineIfLambda>] p) =
    mkParser (fun inp ->
        match getParser p inp with
        | POk res -> POk.createFromRange res.range (ValueSome res)
        | PError _ -> POk.createFromRange (Range.create inp.idx inp.idx) ValueNone
    )

/// Tests if parser succeeds. Always succeeds, returns true/false.
let inline pisOk ([<InlineIfLambda>] p) = 
    mkParser (fun inp ->
        match getParser p inp with
        | POk res -> POk.createFromRange res.range true
        | PError _ -> POk.create inp.idx inp.idx false
    )

/// Tests if parser fails. Always succeeds, returns true/false.
let inline pisErr ([<InlineIfLambda>] p) =
    pisOk p |> map not

/// Negative lookahead: succeeds (without consuming) if p fails.
let inline pnot ([<InlineIfLambda>] p) =
    mkParser (fun inp ->
        match getParser p inp with
        | POk _ -> PError.create inp.idx "Unexpected."
        | PError _ -> POk.create inp.idx inp.idx ()
    )

/// Matches an exact string literal. Consumes the string on success.
let pstr (s: string) =
    mkParser (fun inp ->
        if inp.StartsWith(s)
        then POk.create inp.idx (inp.idx + s.Length) s
        else PError.create inp.idx (sprintf "Expected: '%s'" s)
    )

/// Shorthand prefix operator for pstr. Usage: %"hello"
let ( ~% ) = pstr

/// Moves the cursor to an absolute index. Only forward movement allowed.
let pgoto (idx: int) =
    mkParser (fun inp ->
        if inp.CanGoto(idx) then 
            POk.create inp.idx idx ()
        else
            let msg = sprintf "Index %d is out of range of string of length %d." idx inp.original.Length
            PError.create idx msg
    )

/// Alternative: tries pa first, if it fails tries pb.
let inline orThen ([<InlineIfLambda>] pa) ([<InlineIfLambda>] pb) =
    mkParser (fun inp ->
        match getParser pa inp with
        | POk res -> POk res
        | PError _ -> getParser pb inp
    )

/// Alternative operator. Usage: pa <|> pb
let ( <|> ) = orThen

/// Sequence: runs pa then pb, returns tuple of both results.
let inline andThen ([<InlineIfLambda>] pa) ([<InlineIfLambda>] pb) =
    mkParser (fun inp ->
        match getParser pa inp with
        | POk ares ->
            match getParser pb (inp.Goto ares.range.endIdx) with
            | POk bres -> POk.create inp.idx bres.range.endIdx (ares.result, bres.result)
            | PError error -> PError error
        | PError error -> PError error
    )

/// Sequence operator, returns tuple. Usage: pa .>. pb
let inline ( .>. ) ([<InlineIfLambda>] pa) ([<InlineIfLambda>] pb) = andThen pa pb

/// Sequence, keep left result only. Usage: pa .>> pb
let inline ( .>> ) ([<InlineIfLambda>] pa) ([<InlineIfLambda>] pb) = andThen pa pb |> map fst

/// Sequence, keep right result only. Usage: pa >>. pb
let inline ( >>. ) ([<InlineIfLambda>] pa) ([<InlineIfLambda>] pb) = andThen pa pb |> map snd

/// Tries parsers in order, returns first success. Requires non-empty list.
let firstOf parsers = parsers |> List.reduce orThen

/// Repeats parser p at least minOccurances times. Returns list of PVal results.
let inline manyN minOccurances ([<InlineIfLambda>] p: Parser<_>) =
    mkParser (fun inp ->
        let mutable currIdx = inp.idx
        let mutable run = true
        let mutable iterations = 0
        let res =
            [ while run do
                match getParser p (inp.Goto(currIdx)) with
                | POk res ->
                    yield res
                    do currIdx <- res.range.endIdx
                    do
                        if inp.idx = currIdx
                        then run <- false
                        else iterations <- iterations + 1
                | PError _ ->
                    do run <- false
            ]
        if iterations < minOccurances 
        then PError.create currIdx $"Expected {minOccurances} occurances, but got {iterations}."
        else POk.create inp.idx currIdx res
    )

/// Zero or more: repeats parser until failure. Always succeeds.
let inline many ([<InlineIfLambda>] p) = manyN 0 p

/// One or more: repeats parser until failure. Fails if no match.
let inline many1 ([<InlineIfLambda>] p) =
    parse {
        let! res = many p
        match res.result with
        | [] -> return { idx = res.range.startIdx; message = "Expected at least one element." }
        | _ -> return res
    }

/// Matches any single character. Fails at end of input.
let anyChar =
    mkParserWhen Cursor.notAtEnd (fun inp ->
        POk.create inp.idx (inp.idx + 1) (inp.Rest.[0].ToString())
    )

/// Extracts just the result values from a parser returning PVal list.
let inline noRanges ([<InlineIfLambda>] p: Parser<PVal<'a> list>) =
    map (fun pvals -> pvals |> List.map (fun x -> x.result)) p

/// Concatenates string results from a parser returning PVal<string> list.
let inline pconcat ([<InlineIfLambda>] p: Parser<PVal<string> list>) =
    map (fun pvals -> pvals |> List.map (fun x -> x.result) |> String.concat "") p

/// End of input: succeeds only if cursor is at end.
let eoi =
    mkParser (fun inp ->
        if inp.IsAtEnd
        then POk.create inp.idx inp.idx ()
        else PError.create inp.idx "Expected end of input."
    )

/// Matches a single space character.
let blank = pstr " "

/// Matches zero or more spaces. Always succeeds.
let blanks = many blank |> pconcat

/// Matches one or more spaces.
let blanks1 = many1 blank |> pconcat

/// Consumes characters until puntil matches (not consumed). Succeeds at EOI if chars consumed.
let inline pstringUntil ([<InlineIfLambda>] puntil) =
    mkParser (fun inp ->
        let rec iter currIdx =
            match getParser puntil (inp.Goto currIdx) with
            | POk _ -> POk.create inp.idx currIdx (inp.original.Substring(inp.idx, currIdx - inp.idx))
            | PError _ ->
                if not (inp.CanGoto(currIdx + 1))
                then 
                    if currIdx > inp.idx
                    then POk.create inp.idx currIdx (inp.original.Substring(inp.idx, currIdx - inp.idx))
                    else PError.create currIdx "End of input."
                else iter (currIdx + 1)
        iter inp.idx
    )

/// Like pstringUntil but requires at least one character before terminator.
let inline pstringUntil1 ([<InlineIfLambda>] puntil) =
    mkParser (fun inp ->
        let rec iter currIdx =
            match getParser puntil (inp.Goto currIdx) with
            | POk _ -> 
                if currIdx > inp.idx
                then POk.create inp.idx currIdx (inp.original.Substring(inp.idx, currIdx - inp.idx))
                else PError.create inp.idx "Expected at least one character before terminator."
            | PError _ ->
                if not (inp.CanGoto(currIdx + 1))
                then 
                    if currIdx > inp.idx
                    then POk.create inp.idx currIdx (inp.original.Substring(inp.idx, currIdx - inp.idx))
                    else PError.create currIdx "End of input."
                else iter (currIdx + 1)
        iter inp.idx
    )

/// Matches p1 once, then p2 zero or more times, concatenates all strings.
let many1Str2 p1 p2 =
    parse {
        let! r1 = p1
        let! r2 = many p2 |> pconcat
        return
            {
                range = Range.add r1.range r2.range
                result = r1.result + r2.result
            }
    }

/// Replaces the error message of a parser on failure.
let inline setErrorMessage msg ([<InlineIfLambda>] p) =
    mkParser (fun inp ->
        match getParser p inp with
        | POk _ as res -> res
        | PError err -> PError { err with message = msg }
    )

/// Matches p one or more times, concatenates string results.
let inline many1Str ([<InlineIfLambda>] p) = many1Str2 p p

/// Matches a single character satisfying the predicate.
let pchar predicate errMsg =
    mkParserWhen Cursor.notAtEnd <| fun inp ->
        let c = inp.Rest.[0]
        if predicate c
        then POk.create inp.idx (inp.idx + 1) (string c)
        else PError.create inp.idx (errMsg c)

/// Matches a specific character exactly.
let pchar1 expectedChar =
    pchar (fun c -> c = expectedChar) (fun c -> $"Expected '{expectedChar}', got '{c}'.")

/// Matches any letter (Char.IsLetter).
let letter =
    pchar Char.IsLetter (sprintf "Expected letter, but got '%c'.")

/// Matches any digit 0-9 (Char.IsDigit).
let digit =
    pchar Char.IsDigit (sprintf "Expected digit, but got '%c'.")

/// Matches p, then asserts suffix does NOT follow.
let inline notFollowedBy ([<InlineIfLambda>] p) suffix =
    parse {
        let! x = p
        let! _ = pnot (pstr suffix)
        return x
    }

/// One or more elements separated by psep. Returns non-empty list.
let inline psepBy1 ([<InlineIfLambda>] psep) ([<InlineIfLambda>] pelem: Parser<_>) =
    parse {
        let! x = pelem
        let! xs = many (psep >>. pelem)
        return 
            {
                range = Range.add x.range xs.range
                result = x :: xs.result
            }
    }

/// Zero or more elements separated by psep. Always succeeds.
let inline psepBy ([<InlineIfLambda>] psep) ([<InlineIfLambda>] pelem: Parser<_>) =
    mkParser (fun inp ->
        match getParser (psepBy1 psep pelem) inp with
        | POk res -> POk res
        | PError _ -> POk.create inp.idx inp.idx []
    )

/// Tries each parser in the list in order, returns first success.
let pchoice parsers =
    mkParser <| fun inp ->
        let rec iter parsers =
            match parsers with
            | [] -> PError.create inp.idx "No parser matched."
            | p::ps ->
                match getParser p inp with
                | POk _ as res -> res
                | PError _ -> iter ps
        iter parsers
