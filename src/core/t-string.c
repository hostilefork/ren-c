//
//  File: %t-string.c
//  Summary: "string related datatypes"
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

#undef Byte  // sys-zlib.h defines it compatibly (unsigned char)
#include "sys-zlib.h"  // for crc32_z()

#include "sys-int-funcs.h"

#include "datatypes/sys-money.h"

#define MAX_QUOTED_STR  50  // max length of "string" before going to { }

Byte* Char_Escapes;
#define MAX_ESC_CHAR (0x60-1) // size of escape table
#define IS_CHR_ESC(c) ((c) <= MAX_ESC_CHAR and Char_Escapes[c])

Byte* URL_Escapes;
#define MAX_URL_CHAR (0x80-1)
#define IS_URL_ESC(c)  ((c) <= MAX_URL_CHAR and (URL_Escapes[c] & ESC_URL))
#define IS_FILE_ESC(c) ((c) <= MAX_URL_CHAR and (URL_Escapes[c] & ESC_FILE))

enum {
    ESC_URL = 1,
    ESC_FILE = 2,
    ESC_EMAIL = 4
};


//
//  CT_String: C
//
REBINT CT_String(NoQuote(Cell(const*)) a, NoQuote(Cell(const*)) b, bool strict)
{
    assert(
        ANY_STRING_KIND(CELL_HEART(a))
        or REB_ISSUE == CELL_HEART(a)
        or REB_URL == CELL_HEART(a)
    );
    assert(
        ANY_STRING_KIND(CELL_HEART(b))
        or REB_ISSUE == CELL_HEART(b)
        or REB_URL == CELL_HEART(a)
    );

    REBLEN l1;
    Utf8(const*) cp1 = VAL_UTF8_LEN_SIZE_AT(&l1, nullptr, a);

    REBLEN l2;
    Utf8(const*) cp2 = VAL_UTF8_LEN_SIZE_AT(&l2, nullptr, b);

    REBLEN len = MIN(l1, l2);

    for (; len > 0; len--) {
        Codepoint c1;
        Codepoint c2;

        cp1 = NEXT_CHR(&c1, cp1);
        cp2 = NEXT_CHR(&c2, cp2);

        REBINT d;
        if (strict)
            d = c1 - c2;
        else
            d = LO_CASE(c1) - LO_CASE(c2);

        if (d != 0)
            return d > 0 ? 1 : -1;
    }

    if (l1 == l2)
        return 0;

    return l1 > l2 ? 1 : -1;
}


/***********************************************************************
**
**  Local Utility Functions
**
***********************************************************************/


static void reverse_string(String(*) str, REBLEN index, REBLEN len)
{
    if (len == 0)
        return; // if non-zero, at least one character in the string

    if (Is_String_Definitely_ASCII(str)) {
        Byte* bp = STR_AT(str, index);

        REBLEN n = 0;
        REBLEN m = len - 1;
        for (; n < len / 2; n++, m--) {
            Byte b = bp[n];
            bp[n] = bp[m];
            bp[m] = b;
        }
    }
    else {
        // !!! This is an inefficient method for reversing strings with
        // variable size codepoints.  Better way could work in place:
        //
        // https://stackoverflow.com/q/199260/

        DECLARE_MOLD (mo);
        Push_Mold(mo);

        REBLEN val_len_head = STR_LEN(str);

        Utf8(const*) up = STR_TAIL(str);  // last exists due to len != 0
        REBLEN n;
        for (n = 0; n < len; ++n) {
            Codepoint c;
            up = BACK_CHR(&c, up);
            Append_Codepoint(mo->series, c);
        }

        DECLARE_STABLE (temp);
        Init_Text(temp, Pop_Molded_String(mo));

        // Effectively do a CHANGE/PART to overwrite the reversed portion of
        // the string (from the input value's index to the tail).

        DECLARE_STABLE (string);  // !!! Temp value, string type is irrelevant
        Init_Any_String_At(string, REB_TEXT, str, index);
        Modify_String_Or_Binary(
            string,
            SYM_CHANGE,
            temp,
            AM_PART,  // heed len for deletion
            len,
            1 // dup count
        );

        // Regardless of whether the whole string was reversed or just some
        // part from the index to the tail, the length shouldn't change.
        //
        assert(VAL_LEN_HEAD(string) == val_len_head);
        UNUSED(val_len_head);
    }
}


//
//  MAKE_String: C
//
Bounce MAKE_String(
    Level(*) level_,
    enum Reb_Kind kind,
    Option(Value(const*)) parent,
    const REBVAL *def
){
    if (parent)
        return RAISE(Error_Bad_Make_Parent(kind, unwrap(parent)));

    if (IS_INTEGER(def)) {  // new string with given integer capacity
        //
        // !!! We can't really know how many bytes to allocate for a certain
        // number of codepoints.  UTF-8 may take up to UNI_ENCODED_MAX bytes
        // (typically 4) per CHAR!.  For now we just assume the integer is
        // the expected *byte* capacity, not length, as we can do that.
        //
        // !!! R3-Alpha tolerated decimal, e.g. `make text! 3.14`, which
        // is semantically nebulous (round up, down?) and generally bad.
        // Red continues this behavior.
        //
        return Init_Any_String(OUT, kind, Make_String(Int32s(def, 0)));
    }

    if (ANY_UTF8(def)) {  // new type for the UTF-8 data with new allocation
        Length len;
        Size size;
        const Byte* utf8 = VAL_UTF8_LEN_SIZE_AT(&len, &size, def);
        UNUSED(len);  // !!! Data already valid and checked, should leverage
        return Init_Any_String(
            OUT,
            kind,
            Append_UTF8_May_Fail(  // !!! Should never fail
                nullptr,
                cs_cast(utf8),
                size,
                STRMODE_ALL_CODEPOINTS
            )
        );
    }

    if (IS_BINARY(def)) {  // not necessarily valid UTF-8, so must check
        Size size;
        const Byte* at = VAL_BINARY_SIZE_AT(&size, def);
        return Init_Any_String(
            OUT,
            kind,
            Append_UTF8_May_Fail(nullptr, cs_cast(at), size, STRMODE_NO_CR)
        );
    }

    if (IS_BLOCK(def)) {
        //
        // The construction syntax for making strings that are preloaded with
        // an offset into the data is #[string ["abcd" 2]].
        //
        // !!! In R3-Alpha make definitions didn't have to be a single value
        // (they are for compatibility between construction syntax and MAKE
        // in Ren-C).  So the positional syntax was #[string! "abcd" 2]...
        // while #[string ["abcd" 2]] would join the pieces together in order
        // to produce #{abcd2}.  That behavior is not available in Ren-C.

        REBLEN len;
        Cell(const*) first = VAL_ARRAY_LEN_AT(&len, def);

        if (len != 2)
            goto bad_make;

        if (not ANY_STRING(first))
            goto bad_make;

        Cell(const*) index = first + 1;
        if (!IS_INTEGER(index))
            goto bad_make;

        REBINT i = Int32(index) - 1 + VAL_INDEX(first);
        if (i < 0 or i > cast(REBINT, VAL_LEN_AT(first)))
            goto bad_make;

        return Init_Series_Cell_At(OUT, kind, VAL_SERIES(first), i);
    }

  bad_make:

    return RAISE(Error_Bad_Make(kind, def));
}


//
//  TO_String: C
//
Bounce TO_String(Level(*) level_, enum Reb_Kind kind, const REBVAL *arg)
{
    if (kind == REB_ISSUE) {  // encompasses what would have been TO CHAR!
        if (IS_INTEGER(arg)) {
            //
            // `to issue! 1` is slated to keep the visual consistency intact,
            // so that you'd get #1 back.  With issue! and char! unified,
            // that means a way to get a codepoint is needed.
            //
            return RAISE("Use CODEPOINT-TO-CHAR for codepoint to ISSUE!");
        }
        if (IS_CHAR(arg) and VAL_CHAR(arg) == 0)
            return RAISE(Error_Illegal_Zero_Byte_Raw());  // `#` as codepoint 0

        // Fall through
    }

    if (IS_BINARY(arg)) {
        //
        // !!! Historically TO would convert binaries to strings.  But as
        // the definition of TO has been questioned and evolving, that no
        // longer seems to make sense (e.g. if `TO TEXT! 1` is "1", the
        // concept of implementation transformations doesn't fit).  Keep
        // compatible for right now, but ultimately MAKE or AS should be
        // used for this.
        //
        Size size;
        const Byte* at = VAL_BINARY_SIZE_AT(&size, arg);
        return Init_Any_String(
            OUT,
            kind,
            Append_UTF8_May_Fail(nullptr, cs_cast(at), size, STRMODE_NO_CR)
        );
    }

    // !!! Historical behavior for TO TEXT! of TAG! did not FORM:
    //
    //     >> to text! <abc>
    //     == "abc"
    //
    // However, that behavior is likely to change, as this behavior should
    // be covered by `make text!` or `copy as text!`.  For the present
    // moment, it is kept as-is to avoid disruption.
    //
    if (IS_TAG(arg))
        return MAKE_String(level_, kind, nullptr, arg);

    return Init_Any_String(
        OUT,
        kind,
        Copy_Form_Value(arg, MOLD_FLAG_TIGHT)
    );
}


//
//  to-text: native [
//      {Variant of TO TEXT! with option to tolerate invisible codepoints}
//
//      return: [<opt> text!]
//      value [<maybe> any-value!]
//      /relax "Allow invisible codepoints like CR when converting BINARY!"
//  ]
//
DECLARE_NATIVE(to_text)
{
    INCLUDE_PARAMS_OF_TO_TEXT;

    if (IS_BINARY(ARG(value)) and REF(relax)) {
        Size size;
        const Byte* at = VAL_BINARY_SIZE_AT(&size, ARG(value));
        return Init_Any_String(
            OUT,
            REB_TEXT,
            Append_UTF8_May_Fail(
                nullptr,
                cs_cast(at),
                size,
                STRMODE_ALL_CODEPOINTS
            )
        );
    }

    return rebValue("to text! @", ARG(value));
}


enum COMPARE_CHR_FLAGS {
    CC_FLAG_CASE = 1 << 0, // Case sensitive sort
    CC_FLAG_REVERSE = 1 << 1 // Reverse sort order
};


//
//  Compare_Chr: C
//
// This function is called by qsort_r, on behalf of the string sort
// function.  The `thunk` is an argument passed through from the caller
// and given to us by the sort routine, which tells us about the string
// and the kind of sort that was requested.
//
// !!! As of UTF-8 everywhere, this will only work on all-ASCII strings.
//
static int Compare_Chr(void *thunk, const void *v1, const void *v2)
{
    REBLEN * const flags = cast(REBLEN*, thunk);

    Byte b1 = *cast(const Byte*, v1);
    Byte b2 = *cast(const Byte*, v2);

    assert(b1 < 0x80 and b2 < 0x80);

    if (*flags & CC_FLAG_CASE) {
        if (*flags & CC_FLAG_REVERSE)
            return b2 - b1;
        else
            return b1 - b2;
    }
    else {
        if (*flags & CC_FLAG_REVERSE)
            return LO_CASE(b2) - LO_CASE(b1);
        else
            return LO_CASE(b1) - LO_CASE(b2);
    }
}


//
//  Form_Uni_Hex: C
//
// Fast var-length hex output for uni-chars.
// Returns next position (just past the insert).
//
Byte* Form_Uni_Hex(Byte* out, REBLEN n)
{
    Byte buffer[10];
    Byte* bp = &buffer[10];

    while (n != 0) {
        *(--bp) = Hex_Digits[n & 0xf];
        n >>= 4;
    }

    while (bp < &buffer[10])
        *out++ = *bp++;

    return out;
}


//
//  Mold_Uni_Char: C
//
// !!! These heuristics were used in R3-Alpha to decide when to output
// characters in strings as escape for molding.  It's not clear where to
// draw the line with it...should most printable characters just be emitted
// normally in the UTF-8 string with a few exceptions (like newline as ^/)?
//
// For now just preserve what was there, but do it as UTF8 bytes.
//
void Mold_Uni_Char(REB_MOLD *mo, Codepoint c, bool parened)
{
    String(*) buf = mo->series;

    // !!! The UTF-8 "Byte Order Mark" is an insidious thing which is not
    // necessary for UTF-8, not recommended by the Unicode standard, and
    // Rebol should not invisibly be throwing it out of strings or file reads:
    //
    // https://stackoverflow.com/q/2223882/
    //
    // But the codepoint (U+FEFF, byte sequence #{EF BB BF}) has no printable
    // representation.  So if it's going to be loaded as-is then it should
    // give some hint that it's there.
    //
    // !!! 0x1e is "record separator" which is handled specially too.  The
    // following rationale is suggested by @MarkI:
    //
    //     "Rebol special-cases RS because traditionally it is escape-^
    //      but Rebol uses ^ to indicate escaping so it has to do
    //      something else with that one."

    if (c >= 0x7F or c == 0x1E or c == 0xFEFF) {
        //
        // non ASCII, "^" (RS), or byte-order-mark must be ^(00) escaped.
        //
        // !!! Comment here said "do not AND with the above"
        //
        if (parened or c == 0x1E or c == 0xFEFF) {
            Append_Ascii(buf, "^(");

            Length len_old = STR_LEN(buf);
            Size size_old = STR_SIZE(buf);
            EXPAND_SERIES_TAIL(buf, 5);  // worst case: ^(1234), ^( is done
            TERM_STR_LEN_SIZE(buf, len_old, size_old);

            Byte* bp = BIN_TAIL(buf);
            Byte* ep = Form_Uni_Hex(bp, c); // !!! Make a mold...
            TERM_STR_LEN_SIZE(
                buf,
                len_old + (ep - bp),
                size_old + (ep - bp)
            );
            Append_Codepoint(buf, ')');
            return;
        }

        Append_Codepoint(buf, c);
        return;
    }
    else if (not IS_CHR_ESC(c)) { // Spectre mitigation in MSVC w/o `not`
        Append_Codepoint(buf, c);
        return;
    }

    Append_Codepoint(buf, '^');
    Append_Codepoint(buf, Char_Escapes[c]);
}


//
//  Mold_Text_Series_At: C
//
void Mold_Text_Series_At(REB_MOLD *mo, String(const*) s, REBLEN index) {
    String(*) buf = mo->series;

    if (index >= STR_LEN(s)) {
        Append_Ascii(buf, "\"\"");
        return;
    }

    REBLEN len = STR_LEN(s) - index;

    bool parened = GET_MOLD_FLAG(mo, MOLD_FLAG_NON_ANSI_PARENED);

    // Scan to find out what special chars the string contains?

    REBLEN escape = 0;      // escaped chars
    REBLEN brace_in = 0;    // {
    REBLEN brace_out = 0;   // }
    REBLEN newline = 0;     // lf
    REBLEN quote = 0;       // "
    REBLEN paren = 0;       // (1234)
    REBLEN chr1e = 0;
    REBLEN malign = 0;

    Utf8(const*) up = STR_AT(s, index);

    REBLEN x;
    for (x = index; x < len; x++) {
        Codepoint c;
        up = NEXT_CHR(&c, up);

        switch (c) {
          case '{':
            brace_in++;
            break;

          case '}':
            brace_out++;
            if (brace_out > brace_in)
                malign++;
            break;

          case '"':
            quote++;
            break;

          case '\n':
            newline++;
            break;

          default:
            if (c == 0x1e)
                chr1e += 4; // special case of ^(1e)
            else if (IS_CHR_ESC(c))
                escape++;
            else if (c >= 0x1000)
                paren += 6; // ^(1234)
            else if (c >= 0x100)
                paren += 5; // ^(123)
            else if (c >= 0x80)
                paren += 4; // ^(12)
        }
    }

    if (brace_in != brace_out)
        malign++;

    if (NOT_MOLD_FLAG(mo, MOLD_FLAG_NON_ANSI_PARENED))
        paren = 0;

    up = STR_AT(s, index);

    // If it is a short quoted string, emit it as "string"
    //
    if (len <= MAX_QUOTED_STR and quote == 0 and newline < 3) {
        Append_Codepoint(buf, '"');

        REBLEN n;
        for (n = index; n < STR_LEN(s); n++) {
            Codepoint c;
            up = NEXT_CHR(&c, up);
            Mold_Uni_Char(mo, c, parened);
        }

        Append_Codepoint(buf, '"');
        return;
    }

    // It is a braced string, emit it as {string}:
    if (malign == 0)
        brace_in = brace_out = 0;

    Append_Codepoint(buf, '{');

    REBLEN n;
    for (n = index; n < STR_LEN(s); n++) {
        Codepoint c;
        up = NEXT_CHR(&c, up);

        switch (c) {
          case '{':
          case '}':
            if (malign)
                Append_Codepoint(buf, '^');
            Append_Codepoint(buf, c);
            break;

          case '\n':
          case '"':
            Append_Codepoint(buf, c);
            break;

          default:
            Mold_Uni_Char(mo, c, parened);
        }
    }

    Append_Codepoint(buf, '}');

    USED(escape);
    USED(paren);
    USED(chr1e);
}


// R3-Alpha's philosophy on URL! was:
//
// "Only alphanumerics [0-9a-zA-Z], the special characters $-_.+!*'(),
//  and reserved characters used for their reserved purposes may be used
//  unencoded within a URL."
//
// http://www.blooberry.com/indexdot/html/topics/urlencoding.htm
//
// Ren-C is working with a different model, where URL! is generic to custom
// schemes which may or may not follow the RFC for Internet URLs.  It also
// wishes to preserve round-trip copy-and-paste from URL bars in browsers
// to source and back.  Encoding concerns are handled elsewhere.
//
static void Mold_Url(REB_MOLD *mo, NoQuote(Cell(const*)) v)
{
    Append_String(mo->series, v);
}


static void Mold_File(REB_MOLD *mo, NoQuote(Cell(const*)) v)
{

    Append_Codepoint(mo->series, '%');

    REBLEN len;
    Utf8(const*) cp = VAL_UTF8_LEN_SIZE_AT(&len, nullptr, v);

    REBLEN n;
    for (n = 0; n < len; ++n) {
        Codepoint c;
        cp = NEXT_CHR(&c, cp);

        if (IS_FILE_ESC(c))
            Form_Hex_Esc(mo, c); // c => %xx
        else
            Append_Codepoint(mo->series, c);
    }
}


static void Mold_Tag(REB_MOLD *mo, NoQuote(Cell(const*)) v)
{
    Append_Codepoint(mo->series, '<');
    Append_String(mo->series, v);
    Append_Codepoint(mo->series, '>');
}


//
//  MF_String: C
//
void MF_String(REB_MOLD *mo, NoQuote(Cell(const*)) v, bool form)
{
    String(*) buf = mo->series;

    assert(ANY_STRINGLIKE(v));

    enum Reb_Kind kind = CELL_HEART(v);

    // Special format for MOLD/ALL string series when not at head
    //
    if (GET_MOLD_FLAG(mo, MOLD_FLAG_ALL) and VAL_INDEX(v) != 0) {
        Pre_Mold(mo, v); // e.g. #[file! part
        Mold_Text_Series_At(mo, VAL_STRING(v), 0);
        Post_Mold(mo, v);
        return;
    }

    // The R3-Alpha forming logic was that every string type besides TAG!
    // would form with no delimiters, e.g. `form #foo` is just foo
    //
    if (form and kind != REB_TAG) {
        Append_String(buf, v);
        return;
    }

    switch (kind) {
      case REB_TEXT:
        Mold_Text_Series_At(mo, VAL_STRING(v), VAL_INDEX(v));
        break;

      case REB_FILE:
        if (VAL_LEN_AT(v) == 0) {
            Append_Ascii(buf, "%\"\"");
            break;
        }
        Mold_File(mo, v);
        break;

      case REB_EMAIL:
      case REB_URL:
        Mold_Url(mo, v);
        break;

      case REB_TAG:
        Mold_Tag(mo, v);
        break;

      default:
        panic (v);
    }
}


//
//  Did_Get_Series_Index_From_Picker: C
//
// Will fail if the picker is outright invalid, but return false if it should
// be NULL on the last step of a PICK.
//
bool Did_Get_Series_Index_From_Picker(
    REBINT *out,
    const REBVAL *v,
    Cell(const*) picker
){
    if (not (IS_INTEGER(picker) or IS_DECIMAL(picker)))  // !!! why DECIMAL! ?
        fail (Error_Bad_Pick_Raw(picker));

    REBINT n = Int32(picker);
    if (n == 0)
        return false;  // Rebol2 and Red pick of 0 is none

    if (n < 0)
        ++n;

    n += VAL_INDEX(v) - 1;

    if (n < 0 or cast(REBLEN, n) >= VAL_LEN_HEAD(v))
        return false;  // out of range, null unless POKE or more PICK-ing

    *out = n;
    return true;
}


//
//  REBTYPE: C
//
// Action handler for ANY-STRING!
//
REBTYPE(String)
{
    REBVAL *v = D_ARG(1);
    assert(ANY_STRING(v));

    Option(SymId) id = ID_OF_SYMBOL(verb);

    switch (id) {

    //=//// PICK* (see %sys-pick.h for explanation) ////////////////////////=//

      case SYM_PICK_P: {
        INCLUDE_PARAMS_OF_PICK_P;
        UNUSED(ARG(location));

        Cell(const*) picker = ARG(picker);
        REBINT n;
        if (not Did_Get_Series_Index_From_Picker(&n, v, picker))
            return nullptr;

        Codepoint c = GET_CHAR_AT(VAL_STRING(v), n);

        return Init_Char_Unchecked(OUT, c); }


    //=//// POKE* (see %sys-pick.h for explanation) ////////////////////////=//

      case SYM_POKE_P: {
        INCLUDE_PARAMS_OF_POKE_P;
        UNUSED(ARG(location));

        Cell(const*) picker = ARG(picker);
        REBINT n;
        if (not Did_Get_Series_Index_From_Picker(&n, v, picker))
            fail (Error_Out_Of_Range(picker));

        REBVAL *setval = ARG(value);

        Codepoint c;
        if (IS_CHAR(setval)) {
            c = VAL_CHAR(setval);
        }
        else if (IS_INTEGER(setval)) {
            c = Int32(setval);
        }
        else  // CHANGE is a better route for splicing/removal/etc.
            fail (PARAM(value));

        if (c == 0)
            fail (Error_Illegal_Zero_Byte_Raw());

        String(*) s = VAL_STRING_ENSURE_MUTABLE(v);
        SET_CHAR_AT(s, n, c);

        return nullptr; }  // Array(*) is still fine, caller need not update


      case SYM_REFLECT: {
        INCLUDE_PARAMS_OF_REFLECT;
        UNUSED(ARG(value));  // accounted for by `v`

        if (VAL_WORD_ID(ARG(property)) == SYM_SIZE) {
            Size size;
            VAL_UTF8_SIZE_AT(&size, v);
            return Init_Integer(OUT, size);
        }
        return Series_Common_Action_Maybe_Unhandled(level_, verb); }

      case SYM_UNIQUE:
      case SYM_INTERSECT:
      case SYM_UNION:
      case SYM_DIFFERENCE:
      case SYM_EXCLUDE:
        //
      case SYM_SKIP:
      case SYM_AT:
        return Series_Common_Action_Maybe_Unhandled(level_, verb);

      case SYM_REMOVE: {
        INCLUDE_PARAMS_OF_REMOVE;

        UNUSED(PARAM(series)); // already accounted for

        String(*) s = VAL_STRING_ENSURE_MUTABLE(v);

        REBINT limit;
        if (REF(part))
            limit = Part_Len_May_Modify_Index(v, ARG(part));
        else
            limit = 1;

        REBLEN index = VAL_INDEX(v);  // Part calculation may have changed!
        REBLEN tail = VAL_LEN_HEAD(v);

        if (index >= tail or limit == 0)
            return COPY(v);

        REBLEN len;
        Size size = VAL_SIZE_LIMIT_AT(&len, v, limit);

        Size offset = VAL_BYTEOFFSET_FOR_INDEX(v, index);
        Size size_old = STR_SIZE(s);

        Remove_Series_Units(s, offset, size);  // !!! at one time, kept term
        Free_Bookmarks_Maybe_Null(s);
        TERM_STR_LEN_SIZE(s, tail - len, size_old - size);

        return COPY(v); }

    //-- Modification:
      case SYM_APPEND:
      case SYM_INSERT:
      case SYM_CHANGE: {
        INCLUDE_PARAMS_OF_INSERT;
        UNUSED(PARAM(series));

        Value(*) arg = ARG(value);
        assert(not Is_Nulled(arg));  // not an <opt> parameter

        REBLEN len; // length of target
        if (ID_OF_SYMBOL(verb) == SYM_CHANGE)
            len = Part_Len_May_Modify_Index(v, ARG(part));
        else
            len = Part_Limit_Append_Insert(ARG(part));

        // Note that while inserting or appending NULL is a no-op, CHANGE with
        // a /PART can actually erase data.
        //
        if (Is_Void(arg) and len == 0) {
            if (id == SYM_APPEND) // append always returns head
                VAL_INDEX_RAW(v) = 0;
            return COPY(v);  // don't fail on read only if would be a no-op
        }

        Flags flags = 0;
        if (REF(part))
            flags |= AM_PART;
        if (REF(line))
            flags |= AM_LINE;

        // !!! This mimics historical type tolerance, e.g. not everything that
        // gets appended has to be a string:
        //
        //     rebol2>> append "abc" 'd
        //     == "abcd"
        //
        // However it will not try to FORM blocks or other arrays; it only
        // accepts isotopic blocks to imply "append each item individually".
        //
        if (Is_Void(arg)) {
            // not necessarily a no-op (e.g. CHANGE can erase)
        }
        else if (Is_Splice(arg)) {
            mutable_QUOTE_BYTE(arg) = UNQUOTED_1;
        }
        else if (Is_Isotope(arg)) {  // only SPLICE! in typecheck
            fail (Error_Bad_Isotope(arg));  // ...but that doesn't filter yet
        }
        else if (ANY_ARRAY(arg))
            fail (ARG(value));  // error on `append "abc" [d e]` w/o SPREAD

        VAL_INDEX_RAW(v) = Modify_String_Or_Binary(  // does read-only check
            v,
            unwrap(id),
            arg,
            flags,
            len,
            REF(dup) ? Int32(ARG(dup)) : 1
        );
        return COPY(v); }

    //-- Search:
      case SYM_SELECT:
      case SYM_FIND: {
        INCLUDE_PARAMS_OF_FIND;
        if (Is_Isotope(ARG(pattern)))
            fail (ARG(pattern));

        UNUSED(PARAM(series));

        Flags flags = (
            (REF(match) ? AM_FIND_MATCH : 0)
            | (REF(case) ? AM_FIND_CASE : 0)
        );

        REBLEN tail = Part_Tail_May_Modify_Index(v, ARG(part));

        REBINT skip;
        if (REF(skip)) {
            skip = VAL_INT32(ARG(skip));
            if (skip == 0)
                fail (PARAM(skip));
        }
        else
            skip = 1;

        REBLEN len;
        REBINT find = Find_Value_In_Binstr(
            &len, v, tail, ARG(pattern), flags, skip
        );

        if (find == NOT_FOUND)
            return nullptr;  // don't Proxy_Multi_Returns

        REBLEN ret = cast(REBLEN, find);
        assert(ret <= cast(REBLEN, tail));
        UNUSED(find);

        if (id == SYM_FIND) {
            Init_Series_Cell_At(
                ARG(tail),
                VAL_TYPE(v),
                VAL_SERIES(v),
                ret + len
            );
            Init_Series_Cell_At(
                OUT,
                VAL_TYPE(v),
                VAL_SERIES(v),
                ret
            );
            return Proxy_Multi_Returns(level_);
        }

        assert(id == SYM_SELECT);

        ++ret;
        if (ret == tail)
            return nullptr;

        return Init_Char_Unchecked(
            OUT,
            CHR_CODE(STR_AT(VAL_STRING(v), ret))
        ); }

      case SYM_TAKE: {
        INCLUDE_PARAMS_OF_TAKE;

        ENSURE_MUTABLE(v);

        UNUSED(PARAM(series));

        if (REF(deep))
            fail (Error_Bad_Refines_Raw());

        REBLEN len;
        if (REF(part)) {
            len = Part_Len_May_Modify_Index(v, ARG(part));
            if (len == 0)
                return Init_Any_String(OUT, VAL_TYPE(v), Make_String(0));
        } else
            len = 1;

        // Note that /PART can change index

        REBLEN tail = VAL_LEN_HEAD(v);

        if (REF(last)) {
            if (len > tail) {
                VAL_INDEX_RAW(v) = 0;
                len = tail;
            }
            else
                VAL_INDEX_RAW(v) = cast(REBLEN, tail - len);
        }

        if (VAL_INDEX(v) >= tail) {
            if (not REF(part))
                return RAISE(Error_Nothing_To_Take_Raw());
            return Init_Any_String(OUT, VAL_TYPE(v), Make_String(0));
        }

        // if no /PART, just return value, else return string
        //
        if (REF(part))
            Init_Any_String(OUT, VAL_TYPE(v), Copy_String_At_Limit(v, len));
        else
            Init_Char_Unchecked(OUT, CHR_CODE(VAL_STRING_AT(v)));

        Remove_Any_Series_Len(v, VAL_INDEX(v), len);
        return OUT; }

      case SYM_CLEAR: {
        String(*) s = VAL_STRING_ENSURE_MUTABLE(v);

        REBLEN index = VAL_INDEX(v);
        REBLEN tail = VAL_LEN_HEAD(v);

        if (index >= tail)
            return COPY(v);  // clearing after available data has no effect

        // !!! R3-Alpha would take this opportunity to make it so that if the
        // series is now empty, it reclaims the "bias" (unused capacity at
        // the head of the series).  One of many behaviors worth reviewing.
        //
        if (index == 0 and Get_Series_Flag(s, DYNAMIC))
            Unbias_Series(s, false);

        Free_Bookmarks_Maybe_Null(s);  // review!
        Size offset = VAL_BYTEOFFSET_FOR_INDEX(v, index);
        Free_Bookmarks_Maybe_Null(s);

        TERM_STR_LEN_SIZE(s, cast(REBLEN, index), offset);
        return COPY(v); }

    //-- Creation:

      case SYM_COPY: {
        INCLUDE_PARAMS_OF_COPY;

        UNUSED(PARAM(value));
        UNUSED(REF(deep));  // /DEEP is historically ignored on ANY-STRING!

        REBINT len = Part_Len_May_Modify_Index(v, ARG(part));

        return Init_Any_String(
            OUT,
            VAL_TYPE(v),
            Copy_String_At_Limit(v, len)
        ); }

    //-- Special actions:

      case SYM_SWAP: {
        REBVAL *arg = D_ARG(2);

        if (VAL_TYPE(v) != VAL_TYPE(arg))
            fail (Error_Not_Same_Type_Raw());

        String(*) v_str = VAL_STRING_ENSURE_MUTABLE(v);
        String(*) arg_str = VAL_STRING_ENSURE_MUTABLE(arg);

        REBLEN index = VAL_INDEX(v);
        REBLEN tail = VAL_LEN_HEAD(v);

        if (index < tail and VAL_INDEX(arg) < VAL_LEN_HEAD(arg)) {
            Codepoint v_c = GET_CHAR_AT(v_str, VAL_INDEX(v));
            Codepoint arg_c = GET_CHAR_AT(arg_str, VAL_INDEX(arg));

            SET_CHAR_AT(v_str, VAL_INDEX(v), arg_c);
            SET_CHAR_AT(arg_str, VAL_INDEX(arg), v_c);
        }
        return COPY(v); }

      case SYM_REVERSE: {
        INCLUDE_PARAMS_OF_REVERSE;
        UNUSED(ARG(series));

        String(*) str = VAL_STRING_ENSURE_MUTABLE(v);

        Copy_Cell(OUT, v);  // save before index adjustment
        REBINT len = Part_Len_May_Modify_Index(v, ARG(part));
        if (len > 0)
            reverse_string(str, VAL_INDEX(v), len);
        return OUT; }

      case SYM_SORT: {
        INCLUDE_PARAMS_OF_SORT;

        UNUSED(PARAM(series));

        String(*) str = VAL_STRING_ENSURE_MUTABLE(v);  // just ensure mutability
        UNUSED(str);  // we use the VAL_UTF8_AT() accessor, which is const

        if (REF(all))
            fail (Error_Bad_Refines_Raw());

        if (REF(compare))
            fail (Error_Bad_Refines_Raw());  // !!! not in R3-Alpha

        Copy_Cell(OUT, v);  // before index modification
        REBLEN limit = Part_Len_May_Modify_Index(v, ARG(part));
        if (limit <= 1)
            return OUT;

        Length len;
        Size size;
        const Byte* utf8 = VAL_UTF8_LEN_SIZE_AT_LIMIT(&len, &size, v, limit);

        // Test for if the range is all ASCII can just be if (len == size)...
        // that means every codepoint is one byte.
        //
        if (len != size)
            fail ("Non-ASCII string sorting temporarily unavailable");

        REBLEN skip;
        if (not REF(skip))
            skip = 1;
        else {
            skip = Get_Num_From_Arg(ARG(skip));
            if (skip <= 0 or len % skip != 0 or skip > len)
                fail (PARAM(skip));
        }

        // Use fast quicksort library function:
        REBLEN span = 1;
        if (skip > 1) {
            len /= skip;
            span *= skip;
        }

        REBLEN thunk = 0;
        if (REF(case))
            thunk |= CC_FLAG_CASE;
        if (REF(reverse))
            thunk |= CC_FLAG_REVERSE;

        reb_qsort_r(
            m_cast(Byte*, utf8),  // ok due to VAL_STRING_MUTABLE()
            len,
            span * sizeof(Byte),
            &thunk,
            Compare_Chr
        );
        return OUT; }

      case SYM_RANDOM: {
        INCLUDE_PARAMS_OF_RANDOM;

        UNUSED(PARAM(value));

        if (REF(seed)) { // string/binary contents are the seed
            assert(ANY_STRING(v));

            Size utf8_size;
            Utf8(const*) utf8 = VAL_UTF8_SIZE_AT(&utf8_size, v);
            Set_Random(crc32_z(0L, utf8, utf8_size));
            return NONE;
        }

        REBLEN index = VAL_INDEX(v);
        REBLEN tail = VAL_LEN_HEAD(v);

        if (REF(only)) {
            if (index >= tail)
                return nullptr;
            index += cast(REBLEN, Random_Int(REF(secure)))
                % (tail - index);

            return Init_Char_Unchecked(
                OUT,
                GET_CHAR_AT(VAL_STRING(v), index)
            );
        }

        String(*) str = VAL_STRING_ENSURE_MUTABLE(v);

        if (not Is_String_Definitely_ASCII(str))
            fail ("UTF-8 Everywhere: String shuffle temporarily unavailable");

        bool secure = REF(secure);

        REBLEN n;
        for (n = STR_LEN(str) - index; n > 1;) {
            REBLEN k = index + cast(REBLEN, Random_Int(secure)) % n;
            n--;
            Codepoint swap = GET_CHAR_AT(str, k);
            SET_CHAR_AT(str, k, GET_CHAR_AT(str, n + index));
            SET_CHAR_AT(str, n + index, swap);
        }
        return COPY(v); }

      default:
        // Let the port system try the action, e.g. OPEN %foo.txt
        //
        if ((IS_FILE(v) or IS_URL(v))) {
            const REBVAL *made = rebValue("make port! @", D_ARG(1));
            assert(IS_PORT(made));
            Copy_Cell(D_ARG(1), made);
            rebRelease(made);
            return BOUNCE_CONTINUE;
        }
    }

    fail (UNHANDLED);
}


//
//  Startup_String: C
//
void Startup_String(void)
{
    Char_Escapes = TRY_ALLOC_N_ZEROFILL(Byte, MAX_ESC_CHAR + 1);

    Byte* cp = Char_Escapes;
    Byte c;
    for (c = '@'; c <= '_'; c++)
        *cp++ = c;

    Char_Escapes[cast(Byte, '\t')] = '-'; // tab
    Char_Escapes[cast(Byte, '\n')] = '/'; // line feed
    Char_Escapes[cast(Byte, '"')] = '"';
    Char_Escapes[cast(Byte, '^')] = '^';

    URL_Escapes = TRY_ALLOC_N_ZEROFILL(Byte, MAX_URL_CHAR + 1);

    for (c = 0; c <= ' '; c++)
        URL_Escapes[c] = ESC_URL | ESC_FILE;

    const Byte* dc = cb_cast(";%\"()[]{}<>");

    for (c = strsize(dc); c > 0; c--)
        URL_Escapes[*dc++] = ESC_URL | ESC_FILE;
}


//
//  Shutdown_String: C
//
void Shutdown_String(void)
{
    FREE_N(Byte, MAX_ESC_CHAR + 1, Char_Escapes);
    FREE_N(Byte, MAX_URL_CHAR + 1, URL_Escapes);
}
