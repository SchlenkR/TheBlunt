#r "nuget: FParsec" 

open FParsec

let sigStart = "START"
let sigEnd = "END"

let pDemo : Parser<_,unit> = 
    let field = many1Chars2 letter (letter <|> digit <|> pchar '_')
    let separator = pchar ',' .>> spaces
    let fields = sepBy1 field separator
    between (pstring sigStart .>> spaces1) (spaces1 >>. pstring sigEnd) fields

let run p s =
    match runParserOnString p () "" s with
    | Success (result, _, _) -> printfn "Success: %A" result
    | Failure (msg, _, _) -> printfn "Failure: %s" msg

$"{sigStart} field_0, field_1, field_N {sigEnd}"
|> run pDemo

