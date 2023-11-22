
#r "nuget: TheBlunt"

open TheBlunt

module UrlTemplate =
    let SegmentSeparator =
        pchar
            (fun c -> c = '/')
            (fun c -> $"$%c{c} is not a valid segment separator.")
    
    type TemplateSegment = Plain of string

    let plainSegment =
        pstringUntil SegmentSeparator |> map (fun s -> Plain s)

    let segments = parse {
        let! _ = ptry SegmentSeparator

        let! segments = psepBy1 SegmentSeparator plainSegment
        let! lastSegment = plainSegment

        let r: PVal<_> = {
            range = Range.merge [ segments.range; lastSegment.range ]
            result = [
                yield! segments.result |> List.map (fun pval -> pval.result)
                lastSegment.result
            ]
        }

        return r
    }

    let runParse s = 
        match run s segments with
        | POk { range = range; result = result } -> printfn "%A" result
        | PError err -> eprintfn "%s" err.message


let tpl = "/segment1/segment2/segment3/"
let tpl2 = "segment1/segment2/segment3"

UrlTemplate.runParse tpl
UrlTemplate.runParse tpl2



// [Plain "segment1"; Plain "segment2"; Plain "segment3"; Plain ""]
// [Plain "segment1"; Plain "segment2"; Plain ""]
// Press any key to continue . . . 


