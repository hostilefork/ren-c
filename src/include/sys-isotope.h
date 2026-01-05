//
//  file: %sys-isotope.h
//  summary: "Isotope Coercion Routines"
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


#undef Any_Isotopic  // use Any_Isotopic_Type(Heart_Of(v))


// There are several rules that have to be followed with antiforms.  Having
// the logic for enforcing those rules centralized is important.
//
// 1. The convention here is that you have to pass an Value in, because at
//    the end of the operation you'll have either a Stable* or an Value*.
//    If you were allowed to pass in an Element*, then you'd have an invalid
//    Element at the callsite when the operation completed.
//
//    Note: When building with ExactWrapper, Exact(T) isn't free... costs code
//    in dereferencing.  So it's useful to extract it into an Element* first.
//
// 2. The API uses nullptr as currency with C or other languages to represent
//    the nulled state.  This allows it to be "falsey" in those languages, as
//    well as to not require cleanup by releasing a handle for it.  So you
//    should never be initializing an API value with the cell pattern used
//    internally to represent ~null~ antiforms.
//
//    To avoid risks of this being something that *could* happen, we disallow
//    coercion of API values to antiforms--as a general rule.
//
INLINE Result(Value*) Coerce_To_Antiform(Exact(Value*) v){  // [1]
    Element* elem = Known_Element(v);  // efficient unwrapped extraction [1]

    assert(not Is_Api_Value(elem));  // API uses nullptr, not nulled cells [2]

  ensure_elem_is_quasiform_with_no_sigil: {

  // Quasiforms are allowed to have Sigils, e.g. ~@~ is legal.  But for the
  // moment, there are no antiforms defined for Sigilized types.  This isn't
  // a technical limitation, it's just that the set of antiforms is limited
  // on purpose--to reserve the meanings for future use.

    if (
        (elem->header.bits & (FLAG_LIFT_BYTE(255) | CELL_MASK_SIGIL))
            != FLAG_LIFT_BYTE(QUASIFORM_4)
    ){
        if (LIFT_BYTE(elem) != QUASIFORM_4)
            return fail (
                Error_User("Can only coerce quasiforms to antiforms")
            );
        return fail (Error_User("Cells with sigils cannot become antiforms"));
    }

} coerce_to_antiform: {

  // 1. This is one of the rare functions allowed to use LIFT_BYTE_RAW(), and
  //    you can see why the LIFT_BYTE() safety is important to most code!
  //
  // 2. Antiforms can't be bound.  Though SPLICE! or PACK! can have bindings
  //    on the *elements*, the containing list is not allowed to be bound.
  //
  //    (If antiforms did have meaningful bindings, that would imply binding
  //    functions would need to accept them as parameters.  That leads to a
  //    mess--trying to handle unstable pack antiforms via meta-parameters.)
  //
  // 3. All WORD!s are allowed to have quasiforms, but only special ones are
  //    allowed to be antiform "keywords".  Others are reserved for future
  //    usage, though dialects can use quasi words however they want.

    Option(Heart) heart = Heart_Of(elem);

    switch (opt heart) {
      case TYPE_FRAME: {  // ACTION! (coerced most frequently?)
        if (Frame_Lens(elem))
            Tweak_Frame_Lens_Or_Label(elem, ANONYMOUS);  // show only inputs
        LIFT_BYTE_RAW(v) = STABLE_ANTIFORM_2;  // [1]
        break; }

      case TYPE_BLOCK: {  // SPLICE! (second most frequent?)
        Tweak_Cell_Binding(elem, UNBOUND);  // [2]
        LIFT_BYTE_RAW(v) = STABLE_ANTIFORM_2;  // [1]
        break; }

      case TYPE_GROUP: {  // PACK! (should packs validate their elements?)
        Tweak_Cell_Binding(elem, UNBOUND);  // [2]
        LIFT_BYTE_RAW(v) = STABLE_ANTIFORM_2;  // [1]
        break; }

      case TYPE_FENCE: {  // DATATYPE! (canonize binding)
        Option(Patch*) patch;
        if (
            Series_Len_At(elem) != 1
            or not Is_Word(List_Item_At(elem))
            or not (patch = Sea_Patch(
                g_datatypes_context,
                Word_Symbol(List_Item_At(elem)),
                true
            ))
        ){
            return fail (elem);
        }
        v->payload = Stub_Cell(unwrap patch)->payload;
        v->extra = Stub_Cell(unwrap patch)->extra;
        LIFT_BYTE_RAW(v) = STABLE_ANTIFORM_2;  // [1]
        break; }

      case TYPE_WORD: {  // KEYWORD!
        elem->header.bits &= ~(
            CELL_FLAG_TYPE_SPECIFIC_A | CELL_FLAG_TYPE_SPECIFIC_B
        );
        switch (opt Word_Id(elem)) {
          case SYM_NULL:
            Set_Cell_Flag(elem, KEYWORD_IS_NULL);
            break;

          case SYM_OKAY:
          case SYM_NAN:
            break;

          default:
            return fail (Error_Illegal_Keyword_Raw(elem));  // limited [3]
        }
        Unbind_Any_Word(elem);  // antiforms can't be bound [2]
        LIFT_BYTE_RAW(v) = STABLE_ANTIFORM_2;  // raw [1]
        break; }

      case TYPE_RUNE:  // TRASH!
      case TYPE_WARNING:  // ERROR! (any special work here?)
        LIFT_BYTE_RAW(v) = STABLE_ANTIFORM_2;  // raw [1]
        break;

      case TYPE_COMMA:  // GHOST!
        LIFT_BYTE_RAW(v) = STABLE_ANTIFORM_2;  // raw [1]
        break;

      default:
        return fail (Error_Non_Isotopic_Type_Raw(elem));
    }

    return v;
}}


// 1. There's an exception in the case of KEYWORD! which is the antiform of
//    WORD!.  Only a limited set of them are allowed to exist.  But all
//    words are allowed to be quasiforms.
//
INLINE Result(Element*) Coerce_To_Quasiform(Element* v) {
    Option(Heart) heart = Heart_Of(v);

    if (not Any_Isotopic_Type(heart)) {  // Note: all words have quasiforms [1]
        LIFT_BYTE(v) = NOQUOTE_3;
        return fail (Error_Non_Isotopic_Type_Raw(v));
    }

    LIFT_BYTE_RAW(v) = QUASIFORM_4;  // few places should use LIFT_BYTE_RAW!
    return v;
}


//=//// DUAL STATE DEFINITIONS ///////////////////////////////////////////////

#define Is_Dual_Word_Named_Signal(dual)  Is_Word(dual)

INLINE bool Is_Dual_Meta_Alias_Signal(const Stable* dual) {
    return Is_Meta_Form_Of(WORD, (dual)) or Is_Meta_Form_Of(TUPLE, (dual));
}

INLINE bool Is_Dual_Slot_Alias_Signal(Slot* slot) {
    return Cell_Has_Lift_Sigil_Heart(
        DUAL_0, SIGIL_META, TYPE_WORD, known(Slot*, (slot))
    ) or Cell_Has_Lift_Sigil_Heart(
        DUAL_0, SIGIL_META, TYPE_TUPLE, known(Slot*, (slot))
    );
}


//=//// ELIDING AND DECAYING UNSTABLE ANTIFORMS ///////////////////////////=//
//
// Decay is the process of producing a stable value from an unstable one.  It
// is not legal to decay an unstable antiform into another unstable antiform,
// and it's too risky to let ERROR!s inside PACK!s be silently discarded...
// so they have to be elevated to panics.
//
// "Elision" is more permissive than decay, because you're not actually trying
// to extract a value if the antiform is a PACK! or GHOST! (or a PACK! with
// a PACK! in the first slot, which must be unpacked vs. auto-decaying).  So
// you only need to be concerned about sweeping any ERROR!s under the rug.
//
// The concern about searching for embedded ERROR!s is shared between the
// decay and elide routines, so they are implemented using a common function.
//

#define Decay_If_Unstable(v) \
    Decay_Or_Elide_Core((v), true)

#define Elide_Unless_Error_Including_In_Packs(v) \
    Decay_Or_Elide_Core((v), false)

INLINE Result(Stable*) Decay_Or_Elide_Core(
    Exact(Value*) v,
    bool want_value  // ELIDE is more permissive, doesn't want the value
){
    if (Not_Antiform(v))
        goto finished;

    if (not Is_Pack(v)) {
        if (want_value and Is_Ghost(v))
            return fail ("Cannot decay GHOST! to a value");

        if (Is_Error(v))
            return fail (Cell_Error(v));

        goto finished;
    }

  handle_pack: {  // iterate until result is not a pack

  // 1. If there's an antiform error in the non-primary slot of a pack, we
  //    don't want to just silently discard it.  The best way to make sure no
  //    packs are hiding errors is to recursively decay.
  //
  // 2. If the first element in a pack is a pack, we could decay that.  Maybe
  //    we should, but my general feeling is that you should probably have to
  //    unpack more granularly, e.g. ([[a b] c]: packed-pack), and it just
  //    creates confusion to decay things automatically.  Experience may
  //    dictate that decaying automatically here is better, wait and see.

    const Element* tail;
    const Element* first = List_At(&tail, v);

    if (want_value and first == tail)
        return fail ("Empty PACK! cannot decay to single value");

    for (const Element* at = first; at != tail; ++at) {
        if (not Any_Lifted(at)) {
            if (Is_Dual_Meta_Alias_Signal(at))  // !!! new concept
                continue;  // !!! try this for alias first, others later...

            return fail ("Non-lifted element in PACK!");
        }

        if (Is_Lifted_Error(at))
            return fail (Cell_Error(at));

        if (not Is_Lifted_Pack(at))
            continue;

        Copy_Cell(v, at);
        assume (
            Unlift_Cell_No_Decay(v)
        );
        trap (  /// elide recursively to look for hidden ERROR! [1]
            Elide_Unless_Error_Including_In_Packs(v)
        );
    }

    if (want_value) {
        if (Is_Lifted_Pack(first))  // don't decay first slot [2]
            return fail ("PACK! cannot decay PACK! in first slot");

        if (Is_Lifted_Ghost(first))
            return fail ("PACK! cannot decay GHOST! in first slot");

        assert(not Is_Lifted_Error(first));  // we ruled these out already

        if (Is_Dual_Meta_Alias_Signal(first)) {
            trap (
                Get_Word_Or_Tuple(v, first, SPECIFIED)
            );
        }
        else {
            Copy_Cell(v, first);  // Note: no antiform binding (PACK!)
            assume (  // Any_Lifted() already checked for all pack items
                Unlift_Cell_No_Decay(v)
            );
        }
    }

    goto finished;

} finished: { ////////////////////////////////////////////////////////////////

  #if NEEDFUL_DOES_CORRUPTIONS
    if (not want_value)
       Corrupt_Cell_If_Needful(v);
  #endif

    return u_cast(Stable*, v);
}}

INLINE Result(Stable*) Unliftify_Decayed(Stable* v) {
    trap (
      Value *atom = Unlift_Cell_No_Decay(cast(Value*, v))
    );
    return Decay_If_Unstable(atom);
}
