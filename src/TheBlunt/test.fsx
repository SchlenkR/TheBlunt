#load "../TheBlunt/TheBlunt.fs"

open TheBlunt

// --------------------------------------------------------------------
// EXAMPLES
// --------------------------------------------------------------------

let shouldParse (input: string) expected (parser: _ Parser) =
    match run input parser with
    | POk res ->
        if res.value <> expected then
            failwithf "Input was parsed, but result value was not as expected: '%A' but got '%A'." 
                expected
                res.value
        res.value
    | PError errors -> failwithf "ERROR: %A" errors

joinParse { while CanParse do pstr " " }
|> shouldParse "      XX" "    "


//parse {
//    while CanParse do
//        let x = pstr ""
//        yield x
//}
    