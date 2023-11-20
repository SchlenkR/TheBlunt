
type Parser<'inp, 'outp> =
    Parser of ('inp -> ParserResult<'inp, 'outp>)
and ParserResult<'inp, 'outp> =
    | POk of {| rest: 'inp; result:'outp |}
    | PError of msg:string

let bind (m: Parser<'inp, 'out1>) (f: 'out1 -> Parser<'inp, 'out2>) : Parser<'inp, 'out2> =
    let runp (Parser p) = p
    Parser <| fun inp ->
        match (runp m) inp with
        | POk mRes ->
            let fParser = runp (f mRes.result)
            fParser mRes.rest
        | PError error -> PError error
