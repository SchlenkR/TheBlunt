#r "nuget: TheBlunt"



open TheBlunt

let separator =
    pchar
        (fun c -> c = '/')
        (fun c -> $"$%c{c} is not a valid segment separator.")
let plainSegment = pstringUntil separator
let segments = psepBy1 separator plainSegment

let test s = 
    match run s segments with
    | POk { range = range; result = result } -> printfn "%A" (result |> List.map (fun x -> x.result))
    | PError err -> eprintfn "%s" err.message


let tpl1 = "segment1/segment2/segment3"
let tpl2 = "segment1/segment2/segment3/"

test tpl1
test tpl2
