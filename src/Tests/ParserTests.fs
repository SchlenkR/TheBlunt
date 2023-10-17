
#if INTERACTIVE
#load "../TheBlunt/TheBlunt.fs"
#else
module TheBlunt.Tests
#endif

open System
open NUnit.Framework
open TheBlunt

module Expect =
    let ok expected res =
        match res with
        | POk res ->
            if res.result <> expected then
                failwithf "Expected: %A, but got: %A" expected res.result
        | PError err -> failwithf "Expected: %A, but got error: %A" expected err
    let error res =
        match res with
        | POk res -> failwithf "Expected to fail, but got: %A" res.result
        | PError _ -> ()


let [<TestCase>] ``blanks and whitespaces`` () =
    blank |> run "   " |> Expect.ok " "
    blank |> run " "   |> Expect.ok " "
    blank |> run "x"   |> Expect.error
    blank |> run ""    |> Expect.error

    blanks |> run "     xxx" |> Expect.ok "     "
    blanks |> run "  xxx"    |> Expect.ok "  "
    blanks |> run " xxx"     |> Expect.ok " "
    blanks |> run "xxx"      |> Expect.ok ""

    blanks1 |> run "     xxx" |> Expect.ok "     "
    blanks1 |> run "  xxx"    |> Expect.ok "  "
    blanks1 |> run " xxx"     |> Expect.ok " "
    blanks1 |> run "xxx"      |> Expect.error

let [<TestCase>] ``letter and digit`` () =
    letter |> run "abc" |> Expect.ok "a"
    letter |> run "1abc" |> Expect.error
    digit |> run "1abc" |> Expect.ok "1"
    digit |> run "abc" |> Expect.error

let [<TestCase>] ``not followed by`` () =
    notFollowedBy %"ab" "c"
    |> run "abc"
    |> Expect.error

    notFollowedBy %"ab" "d"
    |> run "abc"
    |> Expect.ok "ab"

let [<TestCase>] ``many and friends`` () =
    many (%"ab") |> pconcat
    |> run "abababX"
    |> Expect.ok "ababab"

    many (%"ab") |> pconcat
    |> run ""
    |> Expect.ok ""

    many1Str (%"ab")
    |> run ""
    |> Expect.error

    many1Str (%"ab")
    |> run "abababX"
    |> Expect.ok "ababab"

let [<TestCase>] ``separation`` () =
    %"ab" |> psepBy1 %";" |> noRanges
    |> run "ab;ab;ab"
    |> Expect.ok ["ab"; "ab"; "ab" ]

    %"ab" |> psepBy1 %";" |> noRanges
    |> run "ab;ab;abX"
    |> Expect.ok ["ab"; "ab"; "ab" ]

    %"ab" |> psepBy1 %";" |> noRanges
    |> run "ab;ab;ab;"
    |> Expect.ok ["ab"; "ab"; "ab" ]

    %"ab" |> psepBy1 %";" |> noRanges
    |> run "ab;ab;ab;"
    |> Expect.ok ["ab"; "ab"; "ab" ]

let [<TestCase>] ``alternatives`` () =
    pchoice [%"a"; %"b"; %"c" ]
    |> run "cab"
    |> Expect.ok "c"

    pchoice [%"a"; %"b"; %"c" ]
    |> run "xyz"
    |> Expect.error
