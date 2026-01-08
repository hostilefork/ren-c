//
//  file: %cell-dual.h
//  summary: "Definitions for Special Cell Dual States"
//  project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2025 Ren-C Open Source Contributors
// REBOL is a trademark of REBOL Technologies
//
// See README.md and CREDITS.md for more information.
//
// Licensed under the Lesser GPL, Version 3.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// https://www.gnu.org/licenses/lgpl-3.0.html
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Duals are states that live "underneath" Value*.  Their LIFT_BYTE() is
// DUAL_0, and they have special meanings.
//
// They can only be stored in Slot* Cells (if they can be stored at all),
// and so cannot be held in Value*... though they can be represented as
// unlifted elements inside antiform PACK!s.
//


#define Is_Bedrock(cell) \
    (LIFT_BYTE(known(Slot*, (cell))) == DUAL_0)


INLINE const Element* Opt_Extract_Match_Dual(Value* v) {
    if (not Is_Pack(v))
        return nullptr;

    const Element* tail;
    const Element* item = List_At(&tail, v);
    if (item == tail or item + 1 != tail)
        return nullptr;

    if (LIFT_BYTE(item) != NOQUOTE_3)
        return nullptr;

    return item;
}

#define Is_Dual(v)  u_cast(bool, Opt_Extract_Match_Dual(v))


//=//// BLACKHOLE ~(_)~ DUAL //////////////////////////////////////////////-//
//
// This is what slots are set to when you do things like:
//
//    for-each _ [1 2 3] [...]
//
// It makes some amount of sense that the dual would be a SPACE rune.
//
//    >> ^blackhole
//    == \~(_)~\  ; antiform (pack!) "dual: blackhole"
//

#define Is_Dual_Space_Blackhole_Signal(dual) \
    Is_Space(dual)

#define Is_Blackhole_Slot(slot) \
    Is_Cell_Space_With_Lift_Sigil((slot), DUAL_0, SIGIL_0)

INLINE Slot* Init_Blackhole_Slot(Init(Slot) out) {
    Init_Space(out);
    LIFT_BYTE(out) = DUAL_0;
    return out;
}


//=//// ~(hot-potato)~ WORD! DUALS /////////////////////////////////////////-//
//
// WORD! duals are specifically prohibited from being stored in variables
// -or- decaying, leading them to be a lightweight way of making something
// that is "ERROR!-like" which can only be taken as a ^META form.
//
// 1. VETO error antiforms signal a desire to cancel the operation that
//    requested the evaluation.  Unlike GHOST which opts out of slots but keeps
//    running, many operations that observe a VETO will return NULL:
//
//        >> reduce ["a" ^ghost "b"]
//        == ["a" "b"]
//
//        >> reduce ["a" ^veto "b"]
//        == \~null~\  ; antiform
//
//    In PARSE, a GROUP! that evaluates to VETO doesn't cancel the parse,
//    but rather just fails that specific GROUP!'s combinator, rolling over to
//    the next alternate.
//
//        >> parse [a b] ['a (if 1 < 2 [^veto]) 'b | (print "alt!") 'a 'b]
//        alt!
//        == 'b
//
// 2. DONE error antiforms report that an enumeration is exhausted and has no
//    further items to give back.  They're used by YIELD or functions that
//    want to act as generators for looping constructs like FOR-EACH or MAP:
//
//        count: 0
//        make-one-thru-five: func [
//            return: [done? integer!]
//        ][
//            if count = 5 [return ^done]
//            return count: count + 1
//        ]
//
//        >> map 'i make-one-thru-five/ [i * 10]
//        == [10 20 30 40 50]
//

INLINE bool Is_Hot_Potato_Dual(Value* v) {
    if (not Is_Pack(v))
        return false;

    const Element* tail;
    const Element* item = List_At(&tail, v);
    if (item == tail or item + 1 != tail)
        return false;
    return Is_Word(item);
}

INLINE bool Is_Hot_Potato_Dual_With_Id(Value* v, SymId id) {
    if (not Is_Pack(v))
        return false;

    const Element* tail;
    const Element* item = List_At(&tail, v);
    if (item == tail or item + 1 != tail)
        return false;
    return Is_Word_With_Id(item, id);
}

#define Is_Veto_Dual(v)  Is_Hot_Potato_Dual_With_Id(v, SYM_VETO)  // [1]
#define Is_Done_Dual(v)  Is_Hot_Potato_Dual_With_Id(v, SYM_DONE)  // [2]


//=//// ALIAS ~(^var)~ ~(^obj.field)~ DUALS ///////////////////////////////-//
//
// An alias dual lets one variable act as another.
//
//    >> x: 10
//
//    >> y: alias $x
//    == \~(^x)~\  ; antiform (pack!) "dual: alias"
//
//    >> y: 20
//
//    >> x
//    == 20
//
// It is chosen as a ^META signal because the least amount of mutation is
// needed to make it something compatible with a SET and GET operation that
// can set to anything (the decision to decay or not is done before the
// alias is written or read from).

INLINE bool Is_Dual_Meta_Alias_Signal(const Stable* dual) {
    return Is_Meta_Form_Of(WORD, (dual)) or Is_Meta_Form_Of(TUPLE, (dual));
}

INLINE bool Is_Dual_Slot_Alias_Signal(Slot* slot) {
    return Cell_Has_Lift_Sigil_Heart(
        slot, DUAL_0, SIGIL_META, TYPE_WORD
    ) or Cell_Has_Lift_Sigil_Heart(
        slot, DUAL_0, SIGIL_META, TYPE_TUPLE
    );
}


//=//// DUAL STATE DEFINITIONS ///////////////////////////////////////////////

#define Is_Dual_Word_Named_Signal(dual)  Is_Word(dual)


#define DUAL_LIFTED(v)    Lift_Cell(v ? v : Init_Nulled(OUT))
#define DUAL_SIGNAL_NULL_ABSENT  NULLED
#define Is_Dual_Nulled_Absent_Signal(dual)  Is_Nulled(dual)

#define WRITEBACK(out)  DUAL_LIFTED(out)  // commentary
#define NO_WRITEBACK_NEEDED  DUAL_SIGNAL_NULL_ABSENT
#define Is_Dual_Nulled_No_Writeback_Signal(dual)  Is_Nulled(dual)

#define Is_Dual_Nulled_Pick_Signal(dual)  Is_Nulled(dual)
#define Init_Dual_Nulled_Pick_Signal(dual)  Init_Nulled(dual)
