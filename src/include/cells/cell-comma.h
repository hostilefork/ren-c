//
//  file: %cell-comma.h
//  summary: "COMMA! Datatype and VOID! Antiform of ~"
//  project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2020-2026 Ren-C Open Source Contributors
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
// The COMMA! is a datatype whose evaluator behavior is to act as what is
// referred to as an "expression barrier".  It will stop argument fulfillment,
// but if no argument fulfillment is in place then it has no effect.
//
//     >> 1 + 2,
//     == 3
//
//     >> 1 +, 2
//     ** Error: barrier hit during evaluation
//
// It has the property that it renders "glued" to the element to the left.
//
// Commas are recognized specially by the evaluator, and produce a VOID!:
//
//     >> eval:step [1 + 2, 10 + 20]
//     == [, 10 + 20]  ; new position, but produced 3 as product
//
//     >> [x ^y]: eval:step [, 10 + 20]
//     == \~('[] ~)~\  ; antiform (pack!)
//
// (Although internally, if the evaluator knows you're not debugging, it will
// silently skip through the commas without yielding an evaluative product.)
//
//=//// NOTES //////////////////////////////////////////////////////////////=//
//
// * Something like COMMA! was actually seriously considered for R3-Alpha,
//   as an "explicit evaluation terminator":
//
//     http://www.rebol.net/r3blogs/0086.html
//

INLINE Element* Init_Comma_Untracked(Init(Element) out) {
    Reset_Cell_Header_Noquote(out, CELL_MASK_COMMA);
    Tweak_Cell_Binding(out, UNBOUND);  // Is_Bindable_Heart() due to niche use
    Corrupt_Unused_Field(out->payload.split.one.corrupt);
    Corrupt_Unused_Field(out->payload.split.two.corrupt);

    return out;
}

#define Init_Comma(out) \
    TRACK(Init_Comma_Untracked(out))


//=//// DECORATED COMMA! ("," DOES NOT RENDER) ////////////////////////////=//
//
// When a COMMA! is decorated with a Sigil, Quasi, or Quote, it does not
// render the comma character:
//
//     >> comma: first [,]
//
//     >> reduce [pin comma tie comma meta comma quasi comma quote comma]
//     == [@ ^ $ ~ ']  ; not [@, ^, $, ~, ',]
//
// These standalone forms are integral parts, and since they are so integral
// then their atomicity is more important than the scanner making it easy
// to make decorated commas.  e.g. (x: '@, y: ~, z: ', ...) should not see
// those as being decorated COMMA!s, merely separating the assignments.
//
// Given that: COMMA! becomes the perfect type to underlie the standalone
// decorated forms.  Since VOID! is antiform COMMA!, this also has the *very*
// pleasing property that the ~ antiform is void.
//

#define Init_Sigiled_Comma(out,sigil) \
    Add_Cell_Sigil(Init_Comma(out), (sigil))

#define Is_Pinned_Comma(v) /* renders as `@` [1] */ \
    Cell_Has_Lift_Sigil_Heart( \
        Known_Stable(v), NOQUOTE_3, SIGIL_PIN, TYPE_COMMA)

#define Is_Metaform_Comma(v) /* renders as `^` [1] */ \
    Cell_Has_Lift_Sigil_Heart( \
        Known_Stable(v), NOQUOTE_3, SIGIL_META, TYPE_COMMA)

#define Is_Tied_Comma(v) /* renders as `$` [1] */ \
    Cell_Has_Lift_Sigil_Heart( \
        Known_Stable(v), NOQUOTE_3, SIGIL_TIE, TYPE_COMMA)


//=//// '~' QUASIFORM (a.k.a. QUASAR) /////////////////////////////////////=//
//
// The quasiform of COMMA! is a tilde (instead of ~,~), and called QUASAR
//
//    >> lift ()
//    == ~
//

#define Is_Quasar(v) \
    Cell_Has_Lift_Sigil_Heart( \
        Known_Stable(v), QUASIFORM_4, SIGIL_0, TYPE_COMMA)

INLINE Element* Init_Quasar_Untracked(Init(Element) out) {
    Init_Comma(out);
    Quasify_Isotopic_Fundamental(out);
    assert(Is_Quasar(out));
    return out;
}

#define Init_Quasar(out) \
    TRACK(Init_Quasar_Untracked(out))


//=//// VOID! (COMMA! ANTIFORM) ///////////////////////////////////////////=//
//
// The unstable ~ antiform is used to signal vanishing intent, e.g. it is
// the return result of things like COMMENT and ELIDE.  It only *actually*
// vanishes if produced by a VANISHABLE function call, or if it is explicitly
// approved as vanishable using the IDENTITY operator (`^`).
//
// See Evaluator_Executor() for how stepping over a block retains the last
// value at each step, so that if a step produces a VOID! the previous
// evaluation can be preserved.
//

INLINE Value* Init_Void_Untracked(Init(Value) out) {
    Init_Comma_Untracked(out);
    Unstably_Antiformize_Unbound_Fundamental(out);
    assert(Is_Void(out));
    return out;
}

#define Init_Void(out) \
    TRACK(Init_Void_Untracked(out))

#define Init_Lifted_Void(out) \
    Lift_Cell(Init_Void(out))


//=//// GHOSTLY VOID! FLAG ////////////////////////////////////////////////=//
//
// The Stepper_Executor() needs to turn VOID! into an empty PACK! if the
// evaluation is "afraid of ghosts" (e.g. a multi-step operation that hasn't
// done something to indicate expectation of a void arising from a non
// vanishable function).  But it can't turn the VOID! into an empty pack
// until it has finished processing any infix operation.  So a flag is put on
// the VOID! to carry the signal.
//
// 1. The flag is in the positive sense (i.e. if the flag is set, the VOID!
//    is overwritten), because this way when the overwrite happens it also
//    clears the flag, so Stepper_Executor() doesn't leak a stray signal that
//    could have meaning to the next step (e.g. CELL_FLAG_NOTE is used by
//    frame processing for tracking if a FRAME! cell has been typechecked)
//
// 2. Since "ghostly voids" only exist in OUT cells, it may be possible to
//    check the header against a fixed bit pattern without masking...since it
//    is known that OUT cells don't (currently) have format bits on the cell.
//    But this could run afoul of fancier uses of extra header bits that
//    would apply to all cells, even OUT cells.  Keep it in mind, but we do
//    the masking for now.
//

#define CELL_FLAG_OUT_NOTE_GHOSTLY_VOID /* set if it *is* ghostly [1] */ \
    CELL_FLAG_NOTE

#define Mark_Level_Out_As_Ghostly_Void(L) \
    ((L)->out->header.bits |= CELL_FLAG_OUT_NOTE_GHOSTLY_VOID)

#define CELL_MASK_GHOSTLY_VOID \
    (FLAG_KIND_BYTE(TYPE_COMMA) \
        | FLAG_LIFT_BYTE(UNSTABLE_ANTIFORM_1) \
        | CELL_FLAG_OUT_NOTE_GHOSTLY_VOID)

#define Is_Level_Out_Ghostly_Void(L) /* check with one mask operation [2] */ \
    (((L)->out->header.bits & CELL_MASK_GHOSTLY_VOID) \
        == CELL_MASK_GHOSTLY_VOID)


//=//// VOID! UNSET INTENT DOCUMENTATION //////////////////////////////////=//
//
// VOID! applies in some places, such as when a PACK! has too few values, as
// this is more useful than erroring in the moment:
//
//     >> [a b c]: pack [1 2]
//     == \~('1 '2)~\  ; antiform
//
//     >> a
//     == 1
//
//     >> b
//     == 2
//
//     >> void? ^c
//     == \~okay~\  ; antiform
//
// Trash would be another possible choice (and able to store a message, like
// ~#PACK-TOO-SHORT~).  But the mechanics of the system are geared toward
// graceful handling of VOID! with <opt> and null inter-convertibility, which
// aren't properties that one generally wants for TRASH!...that's designed to
// throw a deliberate informative wrench into things, to let you know why
// a variable has been "poisoned".  You shouldn't really be manipulating or
// querying TRASH!, just overwriting it (assuming it's not a protected variable
// that is intended to stay trash for a reason...)
//

#define Init_Void_Signifying_Unset(out)  Init_Void(out)


//=//// VOID! UNBRANCHED INTENT DOCUMENTATION /////////////////////////////=//
//
// System-wide, a branching construct that doesn't take a branch will give
// VOID!...and if they take a branch that produces VOID!, they will turn it
// into an empty pack ("HEAVY VOID")
//

#define Init_Void_Signifying_Unbranched(out)  Init_Void(out)
