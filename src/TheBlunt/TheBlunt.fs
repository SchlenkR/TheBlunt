module TheBlunt

open System

type ParserFunction<'value, 'state> = Cursor -> 'state -> ParserResult<'value>

and [<Struct>] Cursor(original: string, idx: int) =
    member _.Idx = idx
    member _.Original = original

and Str = ReadOnlySpan<char>

and [<Struct>] ParserResult<'out> =
    | POk of ok: ParserResultValue<'out>
    | PError of error: ParseError

and [<Struct>] ParserResultValue<'out> =
    { idx: int
      result: 'out }

and [<Struct>] ParseError =
    { idx: int
      message: string }

#if USE_SINGLE_CASE_DU

type Parser<'value, 'state> = Parser of ParserFunction<'value, 'state>

[<AutoOpen>]
module ParserHandling =
    let inline mkParser parserFunction = Parser parserFunction
    let inline getParser (parser: Parser<_,_>) = let (Parser p) = parser in p

module Inline =
    type IfLambdaAttribute() = inherit System.Attribute()

#else

type Parser<'value, 'state> = ParserFunction<'value, 'state>

[<AutoOpen>]
module ParserHandling =
    let inline mkParser (parser: Parser<_,_>) = parser
    let inline getParser (parser: Parser<_,_>) = parser

module Inline =
    type IfLambdaAttribute = FSharp.Core.InlineIfLambdaAttribute

#endif

type [<Struct>] DocPos =
    { idx: int
      ln: int
      col: int }

type ForState() =
    let sb = System.Text.StringBuilder()
    let mutable items = []
    let mutable isFlushed = true
    
    let appendValue (value: string) =
        do sb.Append(value) |> ignore
        do isFlushed <- false
    let addItem item = items <- item :: items
    let appendStateItem clear = 
        do addItem (sb.ToString())
        if clear then
            do sb.Clear() |> ignore
            do isFlushed <- true
    let flush () =
        if not isFlushed then
            do appendStateItem true
        let res = items |> List.rev
        res

    member val Stop = false with get, set
    member _.AppendValue(value) : unit = appendValue value
    member _.Flush() = flush ()
    member _.AddItem(item) = addItem item
    member _.AppendStateItem(clear) = appendStateItem clear

open System.Runtime.CompilerServices

[<Extension>]
type StringExtensions =
    [<Extension>] 
    static member inline StringEquals(s: Str, compareWith: string) = 
        s.SequenceEqual(compareWith.AsSpan())
    [<Extension>] 
    static member inline StringEquals(s: Str, compareWith: Str) =
        s.SequenceEqual(compareWith)
    [<Extension>] 
    static member inline StringEquals(s: string, compareWith: Str)  =
        s.AsSpan().SequenceEqual(compareWith)
    [<Extension>] 
    static member inline StringEquals(s: string, compareWith: string) = 
        String.Equals(s, compareWith)

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

type Cursor with
    member c.CanGoto(idx: int) =
        // TODO: Should be: Only forward
        idx >= c.Idx && idx <= c.Original.Length
    member c.CanWalkFwd(steps: int) = c.CanGoto(c.Idx + steps)
    member c.IsAtEnd = c.Idx = c.Original.Length
    member c.HasRest = c.Idx < c.Original.Length
    member c.Rest = c.Original.AsSpan().Slice(c.Idx)
    member c.StartsWith(s: string) = c.Rest.StringStartsWithAt(s, 0)
    member c.Goto(idx: int) =
        if not (c.CanGoto(idx)) then
            failwithf "Index %d is out of range of string of length %d." idx c.Original.Length
        Cursor(c.Original, idx)
    member c.WalkFwd(steps: int) = c.Goto(c.Idx + steps)
    member c.MoveNext() = c.WalkFwd(1)

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
    let ofInput (pi: Cursor) = create pi.Idx pi.Original

let hasConsumed lastIdx currIdx = lastIdx > currIdx

let inline bind ([<InlineIfLambda>] f: 'a -> Parser<_,_>) (parser: Parser<_,_>) =
    mkParser <| fun inp state ->
        match getParser parser inp state with
        | PError error -> PError error
        | POk pRes ->
            let fParser = getParser (f pRes.result)
            fParser (inp.Goto(pRes.idx)) state

let return' value =
    mkParser <| fun inp state -> 
        POk { idx = inp.Idx; result = value }

let pseq (s: _ seq) =
    let enum = s.GetEnumerator()
    mkParser <| fun inp state ->
        if enum.MoveNext()
        then POk { idx = inp.Idx; result = enum.Current }
        else PError { idx = inp.Idx; message = "No more elements in sequence." }

let inline run (text: string) (parser: Parser<_,_>) =
    let state = ForState()
    match getParser parser (Cursor(text, 0)) state with
    | POk res -> Ok res.result
    | PError error ->
        let docPos = DocPos.create error.idx text
        Error {| pos = docPos; message = error.message |}

type ParserBuilder() =
    member inline _.Bind(p, [<InlineIfLambda>] f) = bind f p
    member _.Return(value) = return' value
    member _.ReturnFrom(value) = value
    member _.Zero() = return' ()
    member _.Delay(f) = f
    member _.Run(f) = 
        mkParser <| fun inp state ->
            getParser (f ()) inp state
    member _.Combine(p1, fp2) = 
        mkParser <| fun inp state ->
            let p2 = fp2 ()
            match getParser p1 inp state with
            | POk p1Res ->
                match getParser p2 (inp.Goto p1Res.idx) state with
                | POk p2Res ->
                    POk
                        { idx = p2Res.idx
                          result = List.append p1Res.result p2Res.result }
                | PError error -> PError error
            | PError error -> PError error
    member _.While(guard, body) =
        mkParser <| fun inp state ->
            let rec iter currResults currIdx =
                match guard () with
                | true ->
                    match getParser (body ()) (inp.Goto currIdx) state with
                    | PError error -> PError error
                    | POk res ->
                        if hasConsumed res.idx currIdx
                        then iter (List.append currResults res.result) res.idx
                        else POk { idx = currIdx; result = currResults }
                | false -> 
                    POk { idx = currIdx; result = currResults }
            iter [] inp.Idx
    member _.For(loopParser, body: _ -> Parser<_,_>) =
        mkParser <| fun inp (state: ForState) ->
            // TODO: This is hardcoced and specialized for Strings
            let rec iter currIdx =
                let pok idx = POk { idx = idx; result = [] }
                match getParser loopParser (inp.Goto currIdx) state with
                | PError err -> pok currIdx
                | POk loopRes ->
                    let bodyP = body loopRes.result
                    match getParser bodyP (inp.Goto loopRes.idx) state with
                    | PError err -> PError err
                    | POk bodyRes ->
                        let pok () = pok bodyRes.idx
                        match state.Stop || inp.IsAtEnd with
                        | true -> pok ()
                        | false ->
                            let continue' =
                                if hasConsumed bodyRes.idx currIdx
                                then Some bodyRes.idx
                                else
                                    let nextIdx = bodyRes.idx + 1
                                    if inp.CanGoto(nextIdx)
                                    then Some nextIdx
                                    else None
                            match continue' with
                            | Some idx -> iter idx
                            | None -> pok ()
            iter inp.Idx
    member this.For(sequence: _ seq, body) = this.For(pseq sequence, body)

let parse = ParserBuilder()

module State =
    let breakLoop =
        mkParser <| fun inp (state: ForState) ->
            state.Stop <- true
            POk { idx = inp.Idx; result = () }
    let appendString (s: string) =
        mkParser <| fun inp (state: ForState) ->
            do state.AppendValue(s)
            POk { idx = inp.Idx; result = () }
    let yieldItem (s: string) =
        mkParser <| fun inp (state: ForState) ->
            do state.AddItem(s)
            POk { idx = inp.Idx; result = () }
    let yieldState clear =
        mkParser <| fun inp (state: ForState) ->
            do state.AppendStateItem clear
            POk { idx = inp.Idx; result = () }
    let getState =
        mkParser <| fun inp (state: ForState) ->
            POk { idx = inp.Idx; result = state }
    let flush =
        mkParser <| fun inp (state: ForState) ->
            POk { idx = inp.Idx; result = state.Flush() }

let map proj (p: Parser<_,_>) =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | PError error -> PError error
        | POk pRes -> POk { idx = pRes.idx; result = proj pRes.result }

let pignore (p: Parser<_,_>) =
    p |> map (fun _ -> ())

let pstr<'s> (s: string) =
    mkParser <| fun inp (state: 's) ->
        if inp.StartsWith(s)
        then POk { idx = inp.Idx + s.Length; result = s }
        else PError { idx = inp.Idx; message = $"Expected: '{s}'" }

let goto (idx: int) =
    mkParser <| fun inp state ->
        if inp.CanGoto(idx) then 
            POk { idx = idx; result = () }
        else
            // TODO: this propably would be a fatal, most propably an unexpected error
            let msg = $"Index {idx} is out of range of string of length {inp.Original.Length}."
            PError { idx = idx; message = msg }

let pAorB a b =
    mkParser <| fun inp state ->
        match getParser a inp state with
        | POk res -> POk res
        | PError _ -> getParser b inp state
let ( <|> ) a b = pAorB a b

let pAandB a b =
    mkParser <| fun inp state ->
        match getParser a inp state with
        | POk ares ->
            match getParser b (inp.Goto ares.idx) state with
            | POk bres -> POk { idx = bres.idx; result = (ares.result, bres.result) } 
            | PError error -> PError error
        | PError error -> PError error
let ( <&> ) a b = pAandB a b

let pfirst parsers = parsers |> List.reduce pAorB

// TODO: sepBy
// TODO: skipN

//let puntil (until: Parser<_,_>) =
//    parse {
//    }

let panyChar<'s> =
    mkParser <| fun inp (state: 's) ->
        if inp.IsAtEnd
        then POk { idx = inp.Idx; result = "" }
        else POk { idx = inp.Idx + 1; result = inp.Rest.Slice(0, 1).ToString() }

let pend<'s> =
    mkParser <| fun inp (state: 's) ->
        if inp.Idx = inp.Original.Length - 1
        then POk { idx = inp.Idx + 1; result = () }
        else PError { idx = inp.Idx; message = "End of input." }

let pblank<'s> = pstr<'s> " "

/// Parse at least n or more blanks.
let pblanks n =
    parse {
        for x in 1 .. n do
            let! c = pblank
            do! State.appendString c
        for x in pblank do
            do! State.appendString x
        return! State.flush
    }

// let psepByP (sep: Parser<_,_>) (p: Parser<_,_>) =
//     parse {
//         let! x = p
//         let! xs = parse {
//             for x in sep do
//                 let! x = p
//                 return! x
//         }
//         return! x :: xs
//     }

// let pSepByStr (p: Parser<_,_>) (sep: Parser<_,_>) =
//     parse {
//         for x
//     }

// TODO: passable reduce function / Zero for builder, etc.
// Name: Imparsible

module Expect =
    let ok expected res =
        match res with
        | Ok res ->
            if res <> expected then
                failwithf "Expected: %A, but got: %A" expected res
        | Error err -> failwithf "Expected: %A, but got error: %A" expected err
    let error res =
        match res with
        | Ok res -> failwithf "Expected to fail, but got: %A" res
        | Error _ -> ()


module Tests =

    pblank |> run "   " |> Expect.ok " "
    pblank |> run " "   |> Expect.ok " "
    pblank |> run "x"   |> Expect.error
    pblank |> run ""    |> Expect.error
    
    pblanks 1 |> run "   xxx" |> Expect.ok ["   "]
    pblanks 1 |> run "  xxx"  |> Expect.ok ["  "]
    pblanks 1 |> run " xxx"   |> Expect.ok [" "]
    pblanks 1 |> run "xxx"    |> Expect.error
    pblanks 0 |> run "xxx"    |> Expect.ok []

    parse {
        for x in panyChar do
            if x = "a" || x = "b" || x = "c" then
                do! State.appendString x
            elif x = "X" then
                do! State.breakLoop
        return! State.flush
    }
    |> run "abcdeaXabb"
    |> Expect.ok ["abca"]

