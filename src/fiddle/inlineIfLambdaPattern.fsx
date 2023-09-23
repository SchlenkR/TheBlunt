
#if USE_INLINEIFLAMBDA

type Vide<'v,'s,'c> = ('s option -> 'c -> 'v * 's option)

[<AutoOpen>]
module VideHandling =
    type InlineIfLambdaAttribute() = inherit System.Attribute()
    let inline mkVide<'v,'s,'c> (v: Vide<'v,'s,'c>) = v
    let inline runVide<'v,'s,'c> (v: Vide<'v,'s,'c>) = v

#else

type Vide<'v,'s,'c> = Vide of ('s option -> 'c -> 'v * 's option)

[<AutoOpen>]
module VideHandling =
    type InlineIfLambdaAttribute = FSharp.Core.InlineIfLambdaAttribute
    let inline mkVide v = Vide v
    let inline runVide (Vide v) = v

#endif

// HowTo: Use it ...

let inline map ([<InlineIfLambda>] proj) vide =
    mkVide <| fun s ctx ->
        let v,s = (runVide vide) s ctx
        proj v, s





