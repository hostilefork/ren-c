//
//  File: %t-blank.c
//  Summary: "Blank datatype"
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
//  MF_Void: C
//
// Prior to generalized quoting, VOID did not have a rendering function and
// it was considered an error to try and mold them.  When quoting arrived,
// escaped VOID was renderable as its ticks, followed by nothing.  This is
// the "nothing" part, saving on a special-case for that.
//
void MF_Void(Molder* mo, const Cell* v, bool form)
{
    UNUSED(mo);
    UNUSED(form);
    UNUSED(v);
}


//
//  MF_Blank: C
//
// Considerable debate was invested into whether BLANK! should act like a
// space when formed in string contexts.  As blanks have moved further away
// from representing "nothing" (delegating shades of that to NULL and VOID)
// it seems to make sense that their presence indicate *something*:
//
//    >> append [a b c] _
//    == [a b c _]
//
// But although some contexts (such as DELIMIT) will treat source-level blanks
// as spaces, their general meaning is underscore.
//
//    >> unspaced ["a" _ "b"]
//    == "a b"
//
//    >> unspaced ["a" @blank "b"]
//    == "a_b"
//
//    >> append "abc" _   ; is it better to support this than not?
//    == "abc_"
//
void MF_Blank(Molder* mo, const Cell* v, bool form)
{
    UNUSED(v);
    UNUSED(form);

    Append_Ascii(mo->string, "_");
}


//
//  CT_Blank: C
//
// Must have a comparison function, otherwise SORT would not work on lists
// with blanks in them.
//
REBINT CT_Blank(const Cell* a, const Cell* b, bool strict)
{
    UNUSED(strict);  // no strict form of comparison
    UNUSED(a);
    UNUSED(b);

    return 0;  // All blanks are equal
}


//
//  DECLARE_GENERICS: C
//
DECLARE_GENERICS(Blank)
{
    switch (Symbol_Id(verb)) {
      case SYM_REFLECT: {
        INCLUDE_PARAMS_OF_REFLECT;

        switch (Cell_Word_Id(ARG(property))) {
          case SYM_INDEX:
            return RAISE(Error_Type_Has_No_Index_Raw(Type_Of(ARG(value))));

          case SYM_LENGTH:
            return Init_Integer(OUT, 0);

          default: break;
        }
        break; }

      case SYM_SELECT:
      case SYM_FIND:
        return nullptr;

      case SYM_TAKE:
        return RAISE(Error_Nothing_To_Take_Raw());

      case SYM_PICK: {
        INCLUDE_PARAMS_OF_PICK;
        UNUSED(ARG(location));
        return RAISE(Error_Bad_Pick_Raw(ARG(picker))); }

      case SYM_COPY: {  // since (copy:deep [1 _ 2]) is legal, allow (copy _)
        INCLUDE_PARAMS_OF_COPY;
        UNUSED(ARG(value));

        if (REF(part))
            return FAIL(Error_Bad_Refines_Raw());

        UNUSED(REF(deep));

        return Init_Blank(OUT); }

      default: break;
    }

    return UNHANDLED;
}



//
//  MF_Handle: C
//
void MF_Handle(Molder* mo, const Cell* v, bool form)
{
    UNUSED(form);  // !!! Handles have "no printable form", what to do here?
    UNUSED(v);

    Append_Ascii(mo->string, "#[handle!]");
}


//
//  CT_Handle: C
//
// !!! Comparing handles is something that wasn't in R3-Alpha and wasn't
// specially covered by Cmp_Value() in R3-Alpha...it fell through to the
// `default:` that just returned a "difference" of 0, so all handles were
// equal.  Ren-C eliminated the default case and instead made comparison of
// handles an error...but that meant comparing objects that contained
// fields that were handles an error.  This meant code looking for "equal"
// PORT!s via FIND did not work.  This raises a larger issue about sameness
// vs. equality that should be studied.
//
REBINT CT_Handle(const Cell* a, const Cell* b, bool strict)
{
    UNUSED(strict);

    // Shared handles are equal if their nodes are equal.  (It may not make
    // sense to have other ideas of equality, e.g. if two nodes incidentally
    // point to the same thing?)
    //
    if (Cell_Has_Node1(a)) {
        if (not Cell_Has_Node1(b))
            return 1;

        if (Cell_Node1(a) == Cell_Node1(b))
            return 0;

        return Cell_Node1(a) > Cell_Node1(b) ? 1 : -1;
    }
    else if (Cell_Has_Node1(b))
        return -1;

    // There is no "identity" when it comes to a non-shared handles, so we
    // can only compare the pointers.
    //
    if (Is_Handle_Cfunc(a)) {
        if (not Is_Handle_Cfunc(b))
            return 1;

        if (Cell_Handle_Cfunc(a) == Cell_Handle_Cfunc(b))
            return 0;

        // !!! Function pointers aren't > or < comparable in ISO C.  This is
        // indicative of what we know already, that HANDLE!s are members of
        // "Eq" but not "Ord" (in Haskell speak).  Comparison is designed to
        // not know whether we're asking for equality or orderedness and must
        // return -1, 0, or 1...so until that is remedied, give back an
        // inconsistent result that just conveys inequality.
        //
        return 1;
    }
    else if (Is_Handle_Cfunc(b))
        return -1;

    if (Cell_Handle_Pointer(Byte, a) == Cell_Handle_Pointer(Byte, b)) {
        if (Cell_Handle_Len(a) == Cell_Handle_Len(b))
            return 0;

        return Cell_Handle_Len(a) > Cell_Handle_Len(b) ? 1 : -1;
    }

    return Cell_Handle_Pointer(Byte, a) > Cell_Handle_Pointer(Byte, b)
        ? 1
        : -1;
}


//
// DECLARE_GENERICS: C
//
// !!! Currently, in order to have a comparison function a datatype must also
// have a dispatcher for generics, and the comparison is essential.  Hence
// this cannot use a `-` in the %reb-types.r in lieu of this dummy function.
//
DECLARE_GENERICS(Handle)
{
    UNUSED(verb);

    return UNHANDLED;
}
