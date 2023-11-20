//
//  File: %t-word.c
//  Summary: "word related datatypes"
//  Section: datatypes
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2017 Ren-C Open Source Contributors
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

#include "sys-core.h"


//
//  Compare_Spellings: C
//
// Used in CT_Word() and CT_Bad_Word()
//
REBINT Compare_Spellings(const Symbol* a, const Symbol* b, bool strict)
{
    if (strict) {
        if (a == b)
            return 0;

        // !!! "Strict" is interpreted as "case-sensitive comparison".  Using
        // strcmp() means the two pointers must be to '\0'-terminated byte
        // arrays, and they are checked byte-for-byte.  This does not account
        // for unicode normalization.  Review.
        //
        // https://en.wikipedia.org/wiki/Unicode_equivalence#Normalization
        //
        REBINT diff = strcmp(String_UTF8(a), String_UTF8(b));  // byte match check
        if (diff == 0)
            return 0;
        return diff > 0 ? 1 : -1;  // strcmp result not strictly in [-1 0 1]
    }
    else {
        // Different cases acceptable, only check for a canon match
        //
        if (Are_Synonyms(a, b))
            return 0;

        // !!! "They must differ by case...."  This needs to account for
        // unicode "case folding", as well as "normalization".
        //
        REBINT diff = Compare_UTF8(String_Head(a), String_Head(b), String_Size(b));
        if (diff >= 0) {
            assert(diff == 0 or diff == 1 or diff == 3);
            return 0;  // non-case match
        }
        assert(diff == -1 or diff == -3);  // no match
        return diff + 2;
    }
}


//
//  CT_Word: C
//
// Compare the names of two words and return the difference.
// Note that words are kept UTF8 encoded.
//
REBINT CT_Word(NoQuote(const Cell*) a, NoQuote(const Cell*) b, bool strict)
{
    return Compare_Spellings(
        VAL_WORD_SYMBOL(a),
        VAL_WORD_SYMBOL(b),
        strict
    );
}


//
//  MAKE_Word: C
//
Bounce MAKE_Word(
    Level* level_,
    enum Reb_Kind kind,
    Option(Value(const*)) parent,
    const REBVAL *arg
){
    if (parent)
        fail (Error_Bad_Make_Parent(kind, unwrap(parent)));

    if (ANY_WORD(arg)) {
        //
        // !!! This only reset the type, not header bits...as it used to be
        // that header bits related to the binding state.  That's no longer
        // true since EXTRA(Binding, ...) conveys the entire bind state.
        // Rethink what it means to preserve the bits vs. not.
        //
        Copy_Cell(OUT, arg);
        HEART_BYTE(OUT) = kind;
        return OUT;
    }

    if (ANY_STRING(arg)) {
        if (Is_Series_Frozen(VAL_STRING(arg)))
            goto as_word;  // just reuse AS mechanics on frozen strings

        // Otherwise, we'll have to copy the data for a TO conversion
        //
        // !!! Note this permits `TO WORD! "    spaced-out"` ... it's not
        // clear that it should do so.  Review `Analyze_String_For_Scan()`

        Size size;
        const Byte* bp = Analyze_String_For_Scan(&size, arg, MAX_SCAN_WORD);

        if (NULL == Scan_Any_Word(OUT, kind, bp, size))
            return RAISE(Error_Bad_Char_Raw(arg));

        return OUT;
    }
    else if (IS_ISSUE(arg)) {
        //
        // Run the same mechanics that AS WORD! would, since it's immutable.
        //
      as_word: {
        REBVAL *as = rebValue("as", Datatype_From_Kind(kind), arg);
        Copy_Cell(OUT, as);
        rebRelease(as);

        return OUT;
      }
    }
    else if (IS_LOGIC(arg)) {
        return Init_Any_Word(
            OUT,
            kind,
            VAL_LOGIC(arg) ? Canon(TRUE) : Canon(FALSE)
        );
    }

    return RAISE(Error_Unexpected_Type(REB_WORD, VAL_TYPE(arg)));
}


//
//  TO_Word: C
//
Bounce TO_Word(Level* level_, enum Reb_Kind kind, const REBVAL *arg)
{
    // This is here to convert `to word! /a` into `a`.  It also allows
    // `to word! ////a////` and variants, because it seems interesting to try
    // that vs. erroring for a bit, to see if it turns out to be useful.
    //
    // !!! This seems like something TO does more generally, e.g.
    // `to integer! /"10"` making 10.  We might call these "solo paths" as
    // a generalization of "refinement paths"
    //
    if (IS_PATH(arg) or IS_TUPLE(arg)) {
        FRESHEN(OUT);

        DECLARE_LOCAL (temp);

        REBLEN len = VAL_SEQUENCE_LEN(arg);
        REBLEN i;
        for (i = 0; i < len; ++i) {
            const Cell* item = VAL_SEQUENCE_AT(temp, arg, i);
            if (IS_BLANK(item))
                continue;
            if (not IS_WORD(item))
                return RAISE(
                    "Can't make ANY-WORD! from path unless it's one WORD!"
                );
            if (not Is_Fresh(OUT))
                return RAISE(
                    "Can't make ANY-WORD! from path w/more than one WORD!"
                );
            Derelativize(OUT, item, VAL_SEQUENCE_SPECIFIER(arg));
        }

        if (Is_Fresh(OUT))
            return RAISE("Can't MAKE ANY-WORD! from PATH! that's all BLANK!s");

        HEART_BYTE(OUT) = kind;
        return OUT;
    }

    return MAKE_Word(level_, kind, nullptr, arg);
}


inline static void Mold_Word(REB_MOLD *mo, const Symbol* symbol, bool escape)
{
    if (escape) {
        Append_Codepoint(mo->series, '|');
        Append_Utf8(mo->series, String_UTF8(symbol), String_Size(symbol));
        Append_Codepoint(mo->series, '|');
    }
    else
        Append_Utf8(mo->series, String_UTF8(symbol), String_Size(symbol));
}


//
//  MF_Word: C
//
void MF_Word(REB_MOLD *mo, NoQuote(const Cell*) v, bool form) {
    const Symbol* symbol = VAL_WORD_SYMBOL(v);
    bool escape = form
        ? false
        : Get_Subclass_Flag(SYMBOL, symbol, ESCAPE_PLAIN);

    Mold_Word(mo, symbol, escape);
}


//
//  MF_Set_word: C
//
void MF_Set_word(REB_MOLD *mo, NoQuote(const Cell*) v, bool form) {
    const Symbol* symbol = VAL_WORD_SYMBOL(v);
    bool escape = form
        ? false
        : Get_Subclass_Flag(SYMBOL, symbol, ESCAPE_WITH_SIGIL);

    Mold_Word(mo, symbol, escape);
    Append_Codepoint(mo->series, ':');
}


//
//  MF_Get_word: C
//
void MF_Get_word(REB_MOLD *mo, NoQuote(const Cell*) v, bool form) {
    const Symbol* symbol = VAL_WORD_SYMBOL(v);
    bool escape = form
        ? false
        : Get_Subclass_Flag(SYMBOL, symbol, ESCAPE_WITH_SIGIL);

    Append_Codepoint(mo->series, ':');
    Mold_Word(mo, symbol, escape);
}


//
//  MF_Meta_word: C
//
void MF_Meta_word(REB_MOLD *mo, NoQuote(const Cell*) v, bool form) {
    const Symbol* symbol = VAL_WORD_SYMBOL(v);
    bool escape = form
        ? false
        : Get_Subclass_Flag(SYMBOL, symbol, ESCAPE_WITH_SIGIL);

    Append_Codepoint(mo->series, '^');
    Mold_Word(mo, symbol, escape);
}


//
//  MF_The_word: C
//
void MF_The_word(REB_MOLD *mo, NoQuote(const Cell*) v, bool form) {
    const Symbol* symbol = VAL_WORD_SYMBOL(v);
    bool escape = form
        ? false
        : Get_Subclass_Flag(SYMBOL, symbol, ESCAPE_WITH_SIGIL);

    Append_Codepoint(mo->series, '@');
    Mold_Word(mo, symbol, escape);
}


//
//  MF_Type_word: C
//
void MF_Type_word(REB_MOLD *mo, NoQuote(const Cell*) v, bool form) {
    const Symbol* symbol = VAL_WORD_SYMBOL(v);
    bool escape = form
        ? false
        : Get_Subclass_Flag(SYMBOL, symbol, ESCAPE_WITH_SIGIL);

    Append_Codepoint(mo->series, '&');
    Mold_Word(mo, symbol, escape);
}


//
//  REBTYPE: C
//
// The future plan for WORD! types is that they will be unified somewhat with
// strings...but that bound words will have read-only data.  Under such a
// plan, string-converting words would not be necessary for basic textual
// operations.
//
REBTYPE(Word)
{
    REBVAL *v = D_ARG(1);
    assert(ANY_WORD(v));

    switch (ID_OF_SYMBOL(verb)) {
      case SYM_REFLECT: {
        INCLUDE_PARAMS_OF_REFLECT;

        UNUSED(ARG(value));
        Option(SymId) property = VAL_WORD_ID(ARG(property));

        switch (property) {
          case SYM_LENGTH: {  // byte size stored, but not # of codepoints
            const String* spelling = VAL_WORD_SYMBOL(v);
            Utf8(const*) cp = String_Head(spelling);
            Size size = String_Size(spelling);
            Length len = 0;
            for (; size > 0; cp = Skip_Codepoint(cp)) {  // manually walk codepoints
                size = size - 1;
                len = len + 1;
            }
            return Init_Integer(OUT, len); }

          case SYM_BINDING: {
            if (not Did_Get_Binding_Of(OUT, v))
                return nullptr;

            if (not IS_MODULE(OUT))  // ordinary contexts don't have "attach"
                return OUT;

            if (VAL_CONTEXT(OUT) == Lib_Context)
                return OUT;  // lib context doesn't inherit

            // BINDING OF answers just ~attached~ if it's a module and there
            // is no variable instance in the module.  Hack that together for
            // the moment.
            //
            REBVAL *var = MOD_VAR(
                VAL_CONTEXT(OUT),
                VAL_WORD_SYMBOL(v),
                true
            );
            if (var)
                return OUT;  // found variable actually in module.

            if (MOD_VAR(Lib_Context, VAL_WORD_SYMBOL(v), true))
                return Init_Quasi_Word(OUT, Canon(INHERITED));

            return Init_Quasi_Word(OUT, Canon(ATTACHED)); }

          case SYM_ATTACH: {  // hack it up...
            if (not IS_WORD_BOUND(v))
                return nullptr;

            if (CTX_TYPE(VAL_WORD_CONTEXT(v)) == REB_MODULE) {
                if (MOD_VAR(VAL_WORD_CONTEXT(v), VAL_WORD_SYMBOL(v), true))
                    return Copy_Cell(OUT, CTX_ARCHETYPE(VAL_WORD_CONTEXT(v)));
            }

            if (not Did_Get_Binding_Of(OUT, v))
                assert(!"Did_Get_Binding_Of() should have worked.");

            return OUT; }

          default:
            break;
        }
        break; }

      case SYM_COPY:
        return COPY(v);

      default:
        break;
    }

    fail (UNHANDLED);
}
