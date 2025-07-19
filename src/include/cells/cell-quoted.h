//
//  file: %cell-quoted.h
//  summary: "Definitions for QUOTED! Cells"
//  project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2018-2025 Ren-C Open Source Contributors
// REBOL is a trademark of REBOL Technologies
//
// See README.md and CREDITS.md for more information
//
// Licensed under the Lesser GPL, Version 3.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// https://www.gnu.org/licenses/lgpl-3.0.html
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Cells reserve a byte in their header called the LIFT_BYTE().  The most
// basic usage is that any value can be "quote" escaped.  The depth is the
// number of apostrophes, e.g. ''''X is a depth of 4.  The operator QUOTE can
// be used to add a quoting level to a value, UNQUOTE to remove one, and
// NOQUOTE to remove all quotes.
//
//     >> quote [a]
//     == '[a]
//
//     >> noquote first ['''''a]
//     == a
//
// But the LIFT_BYTE() is used to encode other states as well: all datatypes
// (besides QUOTED! itself) have an "antiform" form as well as a "quasi" form.
// The quasi form will evaluate to the antiform form, and the antiform form is
// expressly prohibited from being put in arrays:
//
//     >> nice: first [~foo~]
//     == ~foo~
//
//     >> nice
//     == ~foo~
//
//     >> mean: ~foo~
//     == ~foo~  ; anti
//
// With the use of the `^xxx` family of types and the `^` operator, it is
// possible to leverage a form of quoting to transition antiform to quasiform,
// and quasiforms to quoted:
//
//     >> ^nice
//     == '~foo~
//
//     >> ^mean
//     == ~foo~
//
// Antiforms are new in Ren-C and central to how the design solves historical
// problems in Rebol languages.
//

INLINE Count Quotes_Of(const Element* v) {
    assert(LIFT_BYTE_RAW(v) > ANTIFORM_1);
    return (LIFT_BYTE(v) - NOQUOTE_2) >> 1;
}

INLINE Count Quotes_From_Lift_Byte(LiftByte lift_byte) {
    assert(lift_byte > ANTIFORM_1);
    return (lift_byte - NOQUOTE_2) >> 1;
}

#define Is_Unquoted(cell) \
    (LIFT_BYTE(Ensure_Readable(cell)) == NOQUOTE_2)

#define Is_Quoted(cell) \
    (LIFT_BYTE(Ensure_Readable(cell)) >= ONEQUOTE_NONQUASI_4)

INLINE bool Is_Metaform(const Cell* cell) {  // quasiform or quoted
    LiftByte lift_byte = LIFT_BYTE(Ensure_Readable(cell));
    return lift_byte >= QUASIFORM_3;
}


// Turns X into 'X, or '''[1 + 2] into '''''(1 + 2), etc.
//
INLINE Element* Quotify_Depth(Element* elem, Count depth) {
    assert(LIFT_BYTE_RAW(elem) != ANTIFORM_1);

    if (depth == 0)
        return elem;

    if (Quotes_Of(elem) + depth >  MAX_QUOTE_DEPTH)
        panic ("Quoting Depth of 126 Exceeded");

    LIFT_BYTE_RAW(elem) += Quote_Shift(depth);
    return elem;
}


// Turns 'X into X, or '''''[1 + 2] into '''(1 + 2), etc.
//
INLINE Element* Unquotify_Depth(Element* elem, Count depth) {
    assert(LIFT_BYTE_RAW(elem) != ANTIFORM_1);

    if (depth == 0)
        return elem;

    if (depth > Quotes_Of(elem))
        panic ("Attempt to set quoting level of value to less than 0");

    LIFT_BYTE_RAW(elem) -= Quote_Shift(depth);
    return elem;
}

#define Quotify(elem)      Quotify_Depth((elem), 1)
#define Unquotify(elem)    Unquotify_Depth((elem), 1)

INLINE Count Dequotify(Element* elem) {
    Count depth = Quotes_Of(elem);
    if (LIFT_BYTE_RAW(elem) & QUASI_BIT)
        LIFT_BYTE_RAW(elem) = QUASIFORM_3;  // already quasi
    else
        LIFT_BYTE_RAW(elem) = NOQUOTE_2;
    return depth;
}


//=//// ANTIFORMS /////////////////////////////////////////////////////////=//

// Antiforms are foundational in covering edge cases in representation which
// plague Rebol2 and Red.  They enable shifting into a "non-literal" domain,
// where whatever "weird" condition the antiform was attempting to capture can
// be handled without worrying about conflating with more literal usages.
// A good example is addressing the splicing intent for blocks:
//
//     >> append [a b c] [d e]
//     == [a b c [d e]]
//
//     >> ~(d e)~
//     == ~(d e)~  ; anti (this connotes a "splice")
//
//     >> append [a b c] ~(d e)~
//     == [a b c d e]
//
//     >> append [a b c] '~(d e)~
//     == [a b c ~(d e)~]
//
// As demonstrated, the reified QUASIFORM! and the "ghostly" ANTIFORM! work
// in concert to solve the problem.
//
// * A special parameter convention must be used to receive unstable antiforms.
//   Code that isn't expecting such strange circumstances can error if they
//   happen, while more sensitive code can be adapted to cleanly handle the
//   intents that they care about.
//
// Unstable antiforms like packs (block antiforms), error antiforms, and object
// antiforms aren't just not allowed in blocks, they can't be in variables.

INLINE bool Is_Antiform(const Atom* a)
  { return LIFT_BYTE(Ensure_Readable(a)) == ANTIFORM_1; }

INLINE bool Is_Lifted_Antiform(const Atom* a)
  { return LIFT_BYTE(Ensure_Readable(a)) == QUASIFORM_3; }

#if CHECK_CELL_SUBCLASSES
    INLINE bool Is_Antiform(const Element* elem) = delete;
#endif

#define Not_Antiform(a) (not Is_Antiform(a))

#undef Any_Antiform  // range-based check useful for typesets, but slower

INLINE bool Is_Antiform_Unstable(const Atom* a) {
    // Assume Is_Antiform() checked Ensure_Readable()
    assert(LIFT_BYTE(a) == ANTIFORM_1);
    return (
        Heart_Of(a) == TYPE_BLOCK  // Is_Pack()
        or Heart_Of(a) == TYPE_WARNING  // Is_Error()
        or Heart_Of(a) == TYPE_COMMA  // Is_Ghost()
    );
}

#define Is_Antiform_Stable(a) \
    (not Is_Antiform_Unstable(a))

INLINE bool Is_Cell_Stable(Need(const Atom*) a) {  // repeat for non-inlined speed
    Assert_Cell_Readable(a);
    if (LIFT_BYTE(a) != ANTIFORM_1)
        return true;
    return (
        Heart_Of(a) != TYPE_BLOCK  // Is_Pack()
        and Heart_Of(a) != TYPE_WARNING  // Is_Error()
        and Heart_Of(a) != TYPE_COMMA  // Is_Ghost()
    );
}

#define Not_Cell_Stable(atom) (not Is_Cell_Stable(atom))

#if NO_RUNTIME_CHECKS
    #define Assert_Cell_Stable(c)  NOOP
    #define Known_Stable(a)  u_cast(Value*, (a))
#else
    #define Assert_Cell_Stable(c) \
        assert(Is_Cell_Stable(cast(const Atom*, (c))));

    INLINE Value* Known_Stable(Atom* atom) {
        assert(Is_Cell_Stable(atom));
        return u_cast(Value*, atom);
    }
#endif


//=//// ENSURE THINGS ARE ELEMENTS ////////////////////////////////////////=//
//
// An array element can't be an antiform.  Use Known_Element() when you are
// sure you have an element and only want it checked in the debug build, and
// Ensure_Element() when you are not sure and want to panic if not.
//

INLINE Option(Element*) As_Element(const_if_c Value* v) {
    if (Is_Antiform(v))
        return nullptr;
    return u_cast(Element*, v);
}

#if NO_RUNTIME_CHECKS
    #define Known_Element(cell) \
        c_cast(Element*, (cell))
#else
    INLINE Element* Known_Element(const_if_c Atom* cell) {
        assert(LIFT_BYTE(cell) != ANTIFORM_1);
        return u_cast(Element*, cell);
    }
#endif

INLINE Element* Ensure_Element(const_if_c Atom* cell) {
    if (LIFT_BYTE(cell) == ANTIFORM_1)
        panic (Error_Bad_Antiform(cell));
    return u_cast(Element*, cell);
}

#if CPLUSPLUS_11
    INLINE Option(const Element*) As_Element(const Value* v) {
        if (Is_Antiform(v))
            return nullptr;
        return c_cast(Element*, v);
    }

    INLINE const Element* Ensure_Element(const Atom* cell)
      { return Ensure_Element(m_cast(Atom*, cell)); }

  #if RUNTIME_CHECKS
    INLINE const Element* Known_Element(const Atom* cell) {
        assert(LIFT_BYTE(cell) != ANTIFORM_1);
        return c_cast(Element*, cell);
    }
  #endif

  #if CHECK_CELL_SUBCLASSES
    void Ensure_Element(const Element*) = delete;

    #if RUNTIME_CHECKS
        void Known_Element(const Element*) = delete;
    #endif
  #endif
#endif


//=//// QUASIFORM! ////////////////////////////////////////////////////////=//

// * Quasiforms are truthy.  There's a reason for this, because it allows
//   operations in the ^META domain to easily use functions like ALL and ANY
//   on the lifted values.  (See the FOR-BOTH example.)

INLINE Option(Error*) Trap_Coerce_To_Antiform(Need(Atom*) atom);
INLINE Option(Error*) Trap_Coerce_To_Quasiform(Need(Element*) v);

#define Is_Quasiform(v) \
    (LIFT_BYTE(Ensure_Readable(v)) == QUASIFORM_3)

INLINE Element* Unquasify(Element* elem) {
    assert(LIFT_BYTE(elem) == QUASIFORM_3);
    LIFT_BYTE(elem) = NOQUOTE_2;
    return elem;
}

INLINE Element* Quasify_Isotopic_Fundamental(Element* elem) {
    assert(Any_Isotopic(elem));
    assert(LIFT_BYTE(elem) == NOQUOTE_2);
    LIFT_BYTE_RAW(elem) = QUASIFORM_3;
    return elem;
}

INLINE Value* Stably_Antiformize_Unbound_Fundamental(Need(Value*) v) {
    assert(Any_Isotopic(v));
    assert(LIFT_BYTE(v) == NOQUOTE_2);
    assert(Is_Stable_Antiform_Kind_Byte(KIND_BYTE(v)));
    if (Is_Bindable(v))
        assert(not Cell_Binding(v));
    LIFT_BYTE_RAW(v) = ANTIFORM_1;
    return v;
}

INLINE Atom* Unstably_Antiformize_Unbound_Fundamental(Need(Atom*) atom) {
    assert(Any_Isotopic(atom));
    assert(LIFT_BYTE(atom) == NOQUOTE_2);
    assert(not Is_Stable_Antiform_Kind_Byte(KIND_BYTE(atom)));
    if (Is_Bindable(atom))
        assert(not Cell_Binding(atom));
    LIFT_BYTE_RAW(atom) = ANTIFORM_1;
    return atom;
}

INLINE Element* Quasify_Antiform(Atom* v) {
    assert(Is_Antiform(v));
    LIFT_BYTE_RAW(v) = QUASIFORM_3;  // all antiforms can be quasi
    return u_cast(Element*, v);
}

INLINE Element* Reify(Atom* v) {
    if (LIFT_BYTE(v) == ANTIFORM_1)
        LIFT_BYTE_RAW(v) = QUASIFORM_3;  // all antiforms can be quasi
    return cast(Element*, v);
}


//=//// LIFTING ///////////////////////////////////////////////////////////=//

// Lifting is a superset of plain quoting.  It has the twist that it can
// "quote antiforms" to produce quasiforms.  This is done by LIFT, but also on
// assignment by metaforms (^foo: ...) and metaforms UNLIFT when fetching.
//
// It's hard to summarize in one place all the various applications of this
// feature!  But it's critical to accomplishing composability by which a
// usermode function can accomplish what the system is able to do internally
// with C.  See FOR-BOTH for at least one good example.
//
//  https://forum.rebol.info/t/1833
//

#define Any_Lifted(v) \
    (LIFT_BYTE(Ensure_Readable(v)) >= QUASIFORM_3)  // quasi or quoted

#define Not_Lifted(v) \
    (LIFT_BYTE(Ensure_Readable(v)) < QUASIFORM_3)  // anti or fundamental

INLINE Element* Liftify(Atom* atom) {
    if (LIFT_BYTE_RAW(atom) == ANTIFORM_1) {
        LIFT_BYTE_RAW(atom) = QUASIFORM_3;  // anti means quasi valid
        return cast(Element*, atom);
    }
    return Quotify(cast(Element*, atom));  // a non-antiform winds up quoted
}

INLINE Atom* Unliftify_Undecayed(Need(Atom*) atom) {
    if (LIFT_BYTE_RAW(atom) == QUASIFORM_3) {
        Option(Error*) e = Trap_Coerce_To_Antiform(atom);
        if (e)
            panic (unwrap e);  // !!! shouldn't abruptly panic :-(

        return atom;
    }
    return Unquotify(cast(Element*, atom));  // asserts that it's quoted
}

INLINE Value* Unliftify_Known_Stable(Need(Value*) val) {
    Unliftify_Undecayed(cast(Atom*, val));
    Assert_Cell_Stable(val);
    return val;
}

INLINE Value* Decay_If_Unstable(Need(Atom*) v);

INLINE Value* Unliftify_Decayed(Value* v) {
    return Decay_If_Unstable(Unliftify_Undecayed(cast(Atom*, v)));
}
