import { toString, Union, Record } from "./fable_modules/fable-library.3.7.17/Types.js";
import { string_type, record_type, int32_type, union_type, class_type } from "./fable_modules/fable-library.3.7.17/Reflection.js";
import { StringBuilder__Clear, StringBuilder__Append_Z721C83C5, StringBuilder_$ctor } from "./fable_modules/fable-library.3.7.17/System.Text.js";
import { singleton, reduce, append, reverse, cons, empty } from "./fable_modules/fable-library.3.7.17/List.js";
import { printf, toFail, substring } from "./fable_modules/fable-library.3.7.17/String.js";
import { uncurry, curry, partialApply, getEnumerator, equals, compare } from "./fable_modules/fable-library.3.7.17/Util.js";
import { rangeDouble } from "./fable_modules/fable-library.3.7.17/Range.js";
import { FSharpResult$2 } from "./fable_modules/fable-library.3.7.17/Choice.js";

export class Cursor extends Record {
    constructor(original, idx) {
        super();
        this.original = original;
        this.idx = (idx | 0);
    }
}

export function Cursor$reflection() {
    return class_type("TheBlunt.Cursor", void 0, Cursor, class_type("System.ValueType"));
}

export function Cursor_$ctor_Z18115A39(original, idx) {
    return new Cursor(original, idx);
}

export class ParserResult$1 extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["POk", "PError"];
    }
}

export function ParserResult$1$reflection(gen0) {
    return union_type("TheBlunt.ParserResult`1", [gen0], ParserResult$1, () => [[["ok", ParserResultValue$1$reflection(gen0)]], [["error", ParseError$reflection()]]]);
}

export class ParserResultValue$1 extends Record {
    constructor(idx, result) {
        super();
        this.idx = (idx | 0);
        this.result = result;
    }
}

export function ParserResultValue$1$reflection(gen0) {
    return record_type("TheBlunt.ParserResultValue`1", [gen0], ParserResultValue$1, () => [["idx", int32_type], ["result", gen0]]);
}

export class ParseError extends Record {
    constructor(idx, message) {
        super();
        this.idx = (idx | 0);
        this.message = message;
    }
}

export function ParseError$reflection() {
    return record_type("TheBlunt.ParseError", [], ParseError, () => [["idx", int32_type], ["message", string_type]]);
}

export function Cursor__get_Idx(_) {
    return _.idx;
}

export function Cursor__get_Original(_) {
    return _.original;
}

export class DocPos extends Record {
    constructor(idx, ln, col) {
        super();
        this.idx = (idx | 0);
        this.ln = (ln | 0);
        this.col = (col | 0);
    }
}

export function DocPos$reflection() {
    return record_type("TheBlunt.DocPos", [], DocPos, () => [["idx", int32_type], ["ln", int32_type], ["col", int32_type]]);
}

export class ForState {
    constructor() {
        this.sb = StringBuilder_$ctor();
        this.items = empty();
        this.isFlushed = true;
        this["Stop@"] = false;
    }
}

export function ForState$reflection() {
    return class_type("TheBlunt.ForState", void 0, ForState);
}

export function ForState_$ctor() {
    return new ForState();
}

export function ForState__get_Stop(__) {
    return __["Stop@"];
}

export function ForState__set_Stop_Z1FBCCD16(__, v) {
    __["Stop@"] = v;
}

export function ForState__AppendValue_Z721C83C5(_, value) {
    ForState__appendValue_Z721C83C5(_, value);
}

export function ForState__Flush(_) {
    return ForState__flush(_);
}

export function ForState__AddItem_Z721C83C5(_, item) {
    ForState__addItem_Z721C83C5(_, item);
}

export function ForState__AppendStateItem_Z1FBCCD16(_, clear) {
    ForState__appendStateItem_Z1FBCCD16(_, clear);
}

function ForState__appendValue_Z721C83C5(this$, value) {
    StringBuilder__Append_Z721C83C5(this$.sb, value);
    this$.isFlushed = false;
}

function ForState__addItem_Z721C83C5(this$, item) {
    this$.items = cons(item, this$.items);
}

function ForState__appendStateItem_Z1FBCCD16(this$, clear) {
    ForState__addItem_Z721C83C5(this$, toString(this$.sb));
    if (clear) {
        StringBuilder__Clear(this$.sb);
        this$.isFlushed = true;
    }
}

function ForState__flush(this$) {
    if (!this$.isFlushed) {
        ForState__appendStateItem_Z1FBCCD16(this$, true);
    }
    return reverse(this$.items);
}

export class StringExtensions {
    constructor() {
    }
}

export function StringExtensions$reflection() {
    return class_type("TheBlunt.StringExtensions", void 0, StringExtensions);
}

export function StringExtensions_StringStartsWithAt_107DD5FC(this$, other, idx) {
    return substring(this$, idx).indexOf(other) === 0;
}

export function StringExtensions_Slice_Z18115A39(this$, start) {
    return substring(this$, start);
}

export function Cursor__CanGoto_Z524259A4(c, idx) {
    if (idx >= Cursor__get_Idx(c)) {
        return idx <= Cursor__get_Original(c).length;
    }
    else {
        return false;
    }
}

export function Cursor__CanWalkFwd_Z524259A4(c, steps) {
    return Cursor__CanGoto_Z524259A4(c, Cursor__get_Idx(c) + steps);
}

export function Cursor__get_IsAtEnd(c) {
    return Cursor__get_Idx(c) === Cursor__get_Original(c).length;
}

export function Cursor__get_HasRest(c) {
    return Cursor__get_Idx(c) < Cursor__get_Original(c).length;
}

export function Cursor__get_Rest(c) {
    return StringExtensions_Slice_Z18115A39(Cursor__get_Original(c), Cursor__get_Idx(c));
}

export function Cursor__StartsWith_Z721C83C5(c, s) {
    return StringExtensions_StringStartsWithAt_107DD5FC(Cursor__get_Rest(c), s, 0);
}

export function Cursor__Goto_Z524259A4(c, idx) {
    if (!Cursor__CanGoto_Z524259A4(c, idx)) {
        const arg_1 = Cursor__get_Original(c).length | 0;
        toFail(printf("Index %d is out of range of string of length %d."))(idx)(arg_1);
    }
    return Cursor_$ctor_Z18115A39(Cursor__get_Original(c), idx);
}

export function Cursor__WalkFwd_Z524259A4(c, steps) {
    return Cursor__Goto_Z524259A4(c, Cursor__get_Idx(c) + steps);
}

export function Cursor__MoveNext(c) {
    return Cursor__WalkFwd_Z524259A4(c, 1);
}

export function DocPosModule_create(index, input) {
    if ((index < 0) ? true : (index > input.length)) {
        const arg_1 = input.length | 0;
        toFail(printf("Index %d is out of range of input string of length %d."))(index)(arg_1);
    }
    const columnStart = 1;
    let currIdx = 0;
    let line = 1;
    let column = columnStart;
    while (currIdx !== index) {
        if (StringExtensions_StringStartsWithAt_107DD5FC(input, "\n", currIdx)) {
            line = ((line + 1) | 0);
            column = (columnStart | 0);
        }
        else {
            column = ((column + 1) | 0);
        }
        currIdx = ((currIdx + 1) | 0);
    }
    return new DocPos(index, line, column);
}

export function DocPosModule_ofInput(pi) {
    return DocPosModule_create(Cursor__get_Idx(pi), Cursor__get_Original(pi));
}

export function hasConsumed(lastIdx, currIdx) {
    return compare(lastIdx, currIdx) > 0;
}

export function standsStill(lastIdx, currIdx) {
    return equals(lastIdx, currIdx);
}

export function return$0027(value) {
    return (inp) => ((state) => (new ParserResult$1(0, new ParserResultValue$1(Cursor__get_Idx(inp), value))));
}

export function pseq(s) {
    const enum$ = getEnumerator(s);
    return (inp) => ((state) => (enum$["System.Collections.IEnumerator.MoveNext"]() ? (new ParserResult$1(0, new ParserResultValue$1(Cursor__get_Idx(inp), enum$["System.Collections.Generic.IEnumerator`1.get_Current"]()))) : (new ParserResult$1(1, new ParseError(Cursor__get_Idx(inp), "No more elements in sequence.")))));
}

export class ParserBuilder {
    constructor() {
    }
}

export function ParserBuilder$reflection() {
    return class_type("TheBlunt.ParserBuilder", void 0, ParserBuilder);
}

export function ParserBuilder_$ctor() {
    return new ParserBuilder();
}

export function ParserBuilder__Return_1505(_, value) {
    return return$0027(value);
}

export function ParserBuilder__ReturnFrom_1505(_, value) {
    return value;
}

export function ParserBuilder__Zero(_) {
    return return$0027();
}

export function ParserBuilder__Delay_1505(_, f) {
    return f;
}

export function ParserBuilder__Run_316F5A1A(_, f) {
    return (inp) => ((state) => partialApply(2, f, [void 0])(inp)(state));
}

export function ParserBuilder__Combine_2F4F682A(_, p1, fp2) {
    return (inp) => ((state) => {
        const p2 = partialApply(2, fp2, [void 0]);
        const matchValue = curry(2, p1)(inp)(state);
        if (matchValue.tag === 1) {
            return new ParserResult$1(1, matchValue.fields[0]);
        }
        else {
            const p1Res = matchValue.fields[0];
            const matchValue_1 = p2(Cursor__Goto_Z524259A4(inp, p1Res.idx))(state);
            if (matchValue_1.tag === 1) {
                return new ParserResult$1(1, matchValue_1.fields[0]);
            }
            else {
                const p2Res = matchValue_1.fields[0];
                return new ParserResult$1(0, new ParserResultValue$1(p2Res.idx, append(p1Res.result, p2Res.result)));
            }
        }
    });
}

export function ParserBuilder__While_Z3E4DDDD0(_, guard, body) {
    return (inp) => ((state) => {
        const iter = (currResults_mut, currIdx_mut) => {
            iter:
            while (true) {
                const currResults = currResults_mut, currIdx = currIdx_mut;
                const matchValue = guard();
                if (matchValue) {
                    const matchValue_1 = partialApply(2, body, [void 0])(Cursor__Goto_Z524259A4(inp, currIdx))(state);
                    if (matchValue_1.tag === 0) {
                        const res = matchValue_1.fields[0];
                        if (standsStill(res.idx, currIdx)) {
                            return new ParserResult$1(0, new ParserResultValue$1(currIdx, currResults));
                        }
                        else {
                            currResults_mut = append(currResults, res.result);
                            currIdx_mut = res.idx;
                            continue iter;
                        }
                    }
                    else {
                        return new ParserResult$1(1, matchValue_1.fields[0]);
                    }
                }
                else {
                    return new ParserResult$1(0, new ParserResultValue$1(currIdx, currResults));
                }
                break;
            }
        };
        return iter(empty(), Cursor__get_Idx(inp));
    });
}

export function ParserBuilder__For_Z6D7E524D(_, loopParser, body) {
    return (inp) => ((state) => {
        const iter = (currIdx_mut) => {
            iter:
            while (true) {
                const currIdx = currIdx_mut;
                const pok = (idx) => (new ParserResult$1(0, new ParserResultValue$1(idx, empty())));
                const matchValue = curry(2, loopParser)(Cursor__Goto_Z524259A4(inp, currIdx))(state);
                if (matchValue.tag === 0) {
                    const loopRes = matchValue.fields[0];
                    const matchValue_1 = partialApply(2, body, [loopRes.result])(Cursor__Goto_Z524259A4(inp, loopRes.idx))(state);
                    if (matchValue_1.tag === 0) {
                        const bodyRes = matchValue_1.fields[0];
                        const pok_1 = () => pok(bodyRes.idx);
                        if (ForState__get_Stop(state) ? true : Cursor__get_IsAtEnd(inp)) {
                            return pok_1();
                        }
                        else if (standsStill(bodyRes.idx, currIdx)) {
                            const nextIdx = (bodyRes.idx + 1) | 0;
                            if (!Cursor__CanGoto_Z524259A4(inp, nextIdx)) {
                                return pok_1();
                            }
                            else {
                                currIdx_mut = nextIdx;
                                continue iter;
                            }
                        }
                        else {
                            currIdx_mut = bodyRes.idx;
                            continue iter;
                        }
                    }
                    else {
                        return new ParserResult$1(1, matchValue_1.fields[0]);
                    }
                }
                else {
                    return pok(currIdx);
                }
                break;
            }
        };
        return iter(Cursor__get_Idx(inp));
    });
}

export function ParserBuilder__For_5C54F555(this$, sequence, body) {
    return ParserBuilder__For_Z6D7E524D(this$, uncurry(2, pseq(sequence)), body);
}

export const parse = ParserBuilder_$ctor();

export const State_breakLoop = (inp) => ((state) => {
    ForState__set_Stop_Z1FBCCD16(state, true);
    return new ParserResult$1(0, new ParserResultValue$1(Cursor__get_Idx(inp), void 0));
});

export function State_appendString(s) {
    return (inp) => ((state) => {
        ForState__AppendValue_Z721C83C5(state, s);
        return new ParserResult$1(0, new ParserResultValue$1(Cursor__get_Idx(inp), void 0));
    });
}

export function State_yieldItem(s) {
    return (inp) => ((state) => {
        ForState__AddItem_Z721C83C5(state, s);
        return new ParserResult$1(0, new ParserResultValue$1(Cursor__get_Idx(inp), void 0));
    });
}

export function State_yieldState(clear) {
    return (inp) => ((state) => {
        ForState__AppendStateItem_Z1FBCCD16(state, clear);
        return new ParserResult$1(0, new ParserResultValue$1(Cursor__get_Idx(inp), void 0));
    });
}

export const State_getState = (inp) => ((state) => (new ParserResult$1(0, new ParserResultValue$1(Cursor__get_Idx(inp), state))));

export const State_flush = (inp) => ((state) => (new ParserResult$1(0, new ParserResultValue$1(Cursor__get_Idx(inp), ForState__Flush(state)))));

export function map(proj, p) {
    return (inp) => ((state) => {
        const matchValue = curry(2, p)(inp)(state);
        if (matchValue.tag === 0) {
            const pRes = matchValue.fields[0];
            return new ParserResult$1(0, new ParserResultValue$1(pRes.idx, proj(pRes.result)));
        }
        else {
            return new ParserResult$1(1, matchValue.fields[0]);
        }
    });
}

export function pignore(p) {
    return map((_arg) => {
    }, p);
}

export function pstr(s) {
    return (inp) => ((state) => (Cursor__StartsWith_Z721C83C5(inp, s) ? (new ParserResult$1(0, new ParserResultValue$1(Cursor__get_Idx(inp) + s.length, s))) : (new ParserResult$1(1, new ParseError(Cursor__get_Idx(inp), `Expected: '${s}'`)))));
}

export function goto(idx) {
    return (inp) => ((state) => (Cursor__CanGoto_Z524259A4(inp, idx) ? (new ParserResult$1(0, new ParserResultValue$1(idx, void 0))) : (new ParserResult$1(1, new ParseError(idx, `Index ${idx} is out of range of string of length ${Cursor__get_Original(inp).length}.`)))));
}

export function orThen(a, b) {
    return (inp) => ((state) => {
        const matchValue = curry(2, a)(inp)(state);
        return (matchValue.tag === 1) ? curry(2, b)(inp)(state) : (new ParserResult$1(0, matchValue.fields[0]));
    });
}

export function op_LessBarGreater(a, b) {
    return orThen(a, b);
}

export function andThen(a, b) {
    return (inp) => ((state) => {
        const matchValue = curry(2, a)(inp)(state);
        if (matchValue.tag === 1) {
            return new ParserResult$1(1, matchValue.fields[0]);
        }
        else {
            const ares = matchValue.fields[0];
            const matchValue_1 = curry(2, b)(Cursor__Goto_Z524259A4(inp, ares.idx))(state);
            if (matchValue_1.tag === 1) {
                return new ParserResult$1(1, matchValue_1.fields[0]);
            }
            else {
                const bres = matchValue_1.fields[0];
                return new ParserResult$1(0, new ParserResultValue$1(bres.idx, [ares.result, bres.result]));
            }
        }
    });
}

export function op_LessAmpGreater(a, b) {
    return andThen(a, b);
}

export function firstOf(parsers) {
    return reduce((a, b) => orThen(uncurry(2, a), uncurry(2, b)), parsers);
}

export function anyChar() {
    return (inp) => ((state) => {
        let copyOfStruct;
        return Cursor__get_IsAtEnd(inp) ? (new ParserResult$1(0, new ParserResultValue$1(Cursor__get_Idx(inp), ""))) : (new ParserResult$1(0, new ParserResultValue$1(Cursor__get_Idx(inp) + 1, (copyOfStruct = Cursor__get_Rest(inp)[0], copyOfStruct))));
    });
}

export function eoi() {
    return (inp) => ((state) => ((Cursor__get_Idx(inp) === (Cursor__get_Original(inp).length - 1)) ? (new ParserResult$1(0, new ParserResultValue$1(Cursor__get_Idx(inp) + 1, void 0))) : (new ParserResult$1(1, new ParseError(Cursor__get_Idx(inp), "End of input.")))));
}

export function blank() {
    return pstr(" ");
}

export function blanks(n) {
    const builder$0040 = parse;
    return ParserBuilder__Run_316F5A1A(builder$0040, uncurry(3, ParserBuilder__Delay_1505(builder$0040, () => ParserBuilder__Combine_2F4F682A(builder$0040, uncurry(2, ParserBuilder__For_5C54F555(builder$0040, rangeDouble(1, 1, n), uncurry(3, (_arg) => {
        const p_1 = blank();
        return (inp_1) => ((state_1) => {
            const matchValue_1 = p_1(inp_1)(state_1);
            if (matchValue_1.tag === 0) {
                const pRes_1 = matchValue_1.fields[0];
                const state = state_1;
                const inp = Cursor__Goto_Z524259A4(inp_1, pRes_1.idx);
                const matchValue = State_appendString(pRes_1.result)(inp)(state);
                return (matchValue.tag === 0) ? ParserBuilder__Return_1505(builder$0040)(Cursor__Goto_Z524259A4(inp, matchValue.fields[0].idx))(state) : (new ParserResult$1(1, matchValue.fields[0]));
            }
            else {
                return new ParserResult$1(1, matchValue_1.fields[0]);
            }
        });
    }))), uncurry(3, ParserBuilder__Delay_1505(builder$0040, () => ParserBuilder__Combine_2F4F682A(builder$0040, uncurry(2, ParserBuilder__For_Z6D7E524D(builder$0040, uncurry(2, blank()), uncurry(3, (_arg_3) => {
        const p_2 = State_appendString(_arg_3);
        return (inp_2) => ((state_2) => {
            const matchValue_2 = p_2(inp_2)(state_2);
            return (matchValue_2.tag === 0) ? ParserBuilder__Return_1505(builder$0040)(Cursor__Goto_Z524259A4(inp_2, matchValue_2.fields[0].idx))(state_2) : (new ParserResult$1(1, matchValue_2.fields[0]));
        });
    }))), uncurry(3, ParserBuilder__Delay_1505(builder$0040, () => ParserBuilder__ReturnFrom_1505(builder$0040, State_flush))))))))));
}

export function attempt(p) {
    return (inp) => ((state) => {
        const matchValue = curry(2, p)(inp)(state);
        if (matchValue.tag === 1) {
            return new ParserResult$1(1, matchValue.fields[0]);
        }
        else {
            const res = matchValue.fields[0];
            return new ParserResult$1(0, new ParserResultValue$1(res.idx, res));
        }
    });
}

export function strUntil(untilP, p) {
    return (inp) => ((state) => {
        const iter = (currIdx_mut) => {
            iter:
            while (true) {
                const currIdx = currIdx_mut;
                const matchValue = attempt(untilP)(Cursor__Goto_Z524259A4(inp, currIdx))(state);
                if (matchValue.tag === 1) {
                    if (!Cursor__CanGoto_Z524259A4(inp, currIdx + 1)) {
                        return new ParserResult$1(1, new ParseError(currIdx, "End of input."));
                    }
                    else {
                        currIdx_mut = (currIdx + 1);
                        continue iter;
                    }
                }
                else {
                    return new ParserResult$1(0, new ParserResultValue$1(currIdx, substring(Cursor__get_Original(inp), Cursor__get_Idx(inp), currIdx - Cursor__get_Idx(inp))));
                }
                break;
            }
        };
        return iter(Cursor__get_Idx(inp));
    });
}

export function Expect_ok(expected, res) {
    if (res.tag === 1) {
        toFail(printf("Expected: %A, but got error: %A"))(expected)(res.fields[0]);
    }
    else {
        const res_1 = res.fields[0];
        if (!equals(res_1, expected)) {
            toFail(printf("Expected: %A, but got: %A"))(expected)(res_1);
        }
    }
}

export function Expect_error(res) {
    if (res.tag === 1) {
    }
    else {
        toFail(printf("Expected to fail, but got: %A"))(res.fields[0]);
    }
}

Expect_ok(" ", (() => {
    const parser = blank();
    const text_1 = "   ";
    const state = ForState_$ctor();
    const matchValue = parser(Cursor_$ctor_Z18115A39(text_1, 0))(state);
    if (matchValue.tag === 1) {
        const error = matchValue.fields[0];
        return new FSharpResult$2(1, {
            message: error.message,
            pos: DocPosModule_create(error.idx, text_1),
        });
    }
    else {
        return new FSharpResult$2(0, matchValue.fields[0].result);
    }
})());

Expect_ok(" ", (() => {
    const parser = blank();
    const text_1 = " ";
    const state = ForState_$ctor();
    const matchValue = parser(Cursor_$ctor_Z18115A39(text_1, 0))(state);
    if (matchValue.tag === 1) {
        const error = matchValue.fields[0];
        return new FSharpResult$2(1, {
            message: error.message,
            pos: DocPosModule_create(error.idx, text_1),
        });
    }
    else {
        return new FSharpResult$2(0, matchValue.fields[0].result);
    }
})());

Expect_error((() => {
    const parser = blank();
    const text_1 = "x";
    const state = ForState_$ctor();
    const matchValue = parser(Cursor_$ctor_Z18115A39(text_1, 0))(state);
    if (matchValue.tag === 1) {
        const error = matchValue.fields[0];
        return new FSharpResult$2(1, {
            message: error.message,
            pos: DocPosModule_create(error.idx, text_1),
        });
    }
    else {
        return new FSharpResult$2(0, matchValue.fields[0].result);
    }
})());

Expect_error((() => {
    const parser = blank();
    const text_1 = "";
    const state = ForState_$ctor();
    const matchValue = parser(Cursor_$ctor_Z18115A39(text_1, 0))(state);
    if (matchValue.tag === 1) {
        const error = matchValue.fields[0];
        return new FSharpResult$2(1, {
            message: error.message,
            pos: DocPosModule_create(error.idx, text_1),
        });
    }
    else {
        return new FSharpResult$2(0, matchValue.fields[0].result);
    }
})());

Expect_ok(singleton("   "), (() => {
    const parser = blanks(1);
    const text_1 = "   xxx";
    const state = ForState_$ctor();
    const matchValue = parser(Cursor_$ctor_Z18115A39(text_1, 0))(state);
    if (matchValue.tag === 1) {
        const error = matchValue.fields[0];
        return new FSharpResult$2(1, {
            message: error.message,
            pos: DocPosModule_create(error.idx, text_1),
        });
    }
    else {
        return new FSharpResult$2(0, matchValue.fields[0].result);
    }
})());

Expect_ok(singleton("  "), (() => {
    const parser = blanks(1);
    const text_1 = "  xxx";
    const state = ForState_$ctor();
    const matchValue = parser(Cursor_$ctor_Z18115A39(text_1, 0))(state);
    if (matchValue.tag === 1) {
        const error = matchValue.fields[0];
        return new FSharpResult$2(1, {
            message: error.message,
            pos: DocPosModule_create(error.idx, text_1),
        });
    }
    else {
        return new FSharpResult$2(0, matchValue.fields[0].result);
    }
})());

Expect_ok(singleton(" "), (() => {
    const parser = blanks(1);
    const text_1 = " xxx";
    const state = ForState_$ctor();
    const matchValue = parser(Cursor_$ctor_Z18115A39(text_1, 0))(state);
    if (matchValue.tag === 1) {
        const error = matchValue.fields[0];
        return new FSharpResult$2(1, {
            message: error.message,
            pos: DocPosModule_create(error.idx, text_1),
        });
    }
    else {
        return new FSharpResult$2(0, matchValue.fields[0].result);
    }
})());

Expect_error((() => {
    const parser = blanks(1);
    const text_1 = "xxx";
    const state = ForState_$ctor();
    const matchValue = parser(Cursor_$ctor_Z18115A39(text_1, 0))(state);
    if (matchValue.tag === 1) {
        const error = matchValue.fields[0];
        return new FSharpResult$2(1, {
            message: error.message,
            pos: DocPosModule_create(error.idx, text_1),
        });
    }
    else {
        return new FSharpResult$2(0, matchValue.fields[0].result);
    }
})());

Expect_ok(empty(), (() => {
    const parser = blanks(0);
    const text_1 = "xxx";
    const state = ForState_$ctor();
    const matchValue = parser(Cursor_$ctor_Z18115A39(text_1, 0))(state);
    if (matchValue.tag === 1) {
        const error = matchValue.fields[0];
        return new FSharpResult$2(1, {
            message: error.message,
            pos: DocPosModule_create(error.idx, text_1),
        });
    }
    else {
        return new FSharpResult$2(0, matchValue.fields[0].result);
    }
})());

Expect_ok(singleton("abca"), (() => {
    let parser_9;
    const builder$0040 = parse;
    parser_9 = ParserBuilder__Run_316F5A1A(builder$0040, uncurry(3, ParserBuilder__Delay_1505(builder$0040, () => ParserBuilder__Combine_2F4F682A(builder$0040, uncurry(2, ParserBuilder__For_Z6D7E524D(builder$0040, uncurry(2, anyChar()), uncurry(3, (_arg) => {
        const x = _arg;
        if (((x === "a") ? true : (x === "b")) ? true : (x === "c")) {
            const p = State_appendString(x);
            return (inp) => ((state) => {
                const matchValue = p(inp)(state);
                return (matchValue.tag === 0) ? ParserBuilder__Return_1505(builder$0040)(Cursor__Goto_Z524259A4(inp, matchValue.fields[0].idx))(state) : (new ParserResult$1(1, matchValue.fields[0]));
            });
        }
        else {
            return (x === "X") ? ((inp_1) => ((state_1) => {
                const matchValue_1 = State_breakLoop(inp_1)(state_1);
                return (matchValue_1.tag === 0) ? ParserBuilder__Return_1505(builder$0040)(Cursor__Goto_Z524259A4(inp_1, matchValue_1.fields[0].idx))(state_1) : (new ParserResult$1(1, matchValue_1.fields[0]));
            })) : ParserBuilder__Zero(builder$0040);
        }
    }))), uncurry(3, ParserBuilder__Delay_1505(builder$0040, () => ParserBuilder__ReturnFrom_1505(builder$0040, State_flush)))))));
    const text_1 = "abcdeaXabb";
    const state_2 = ForState_$ctor();
    const matchValue_2 = parser_9(Cursor_$ctor_Z18115A39(text_1, 0))(state_2);
    if (matchValue_2.tag === 1) {
        const error_2 = matchValue_2.fields[0];
        return new FSharpResult$2(1, {
            message: error_2.message,
            pos: DocPosModule_create(error_2.idx, text_1),
        });
    }
    else {
        return new FSharpResult$2(0, matchValue_2.fields[0].result);
    }
})());

