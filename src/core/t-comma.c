//
//  File: %t-comma.c
//  Summary: "Comma Datatype"
//  Section: datatypes
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2020 Ren-C Open Source Contributors
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
//  MF_Comma: C
//
// The special behavior of commas makes them "glue" their rendering to the
// thing on their left.
//
void MF_Comma(REB_MOLD *mo, NoQuote(const Cell*) v, bool form)
{
    UNUSED(form);
    UNUSED(v);

    Size size = String_Size(mo->series);
    if (
        size > mo->base.size + 1
        and *Binary_At(mo->series, size - 1) == ' '  // not multibyte char
        and *Binary_At(mo->series, size - 2) != ','  // also safe compare
    ){
        *Binary_At(mo->series, size - 1) = ',';
    }
    else
        Append_Codepoint(mo->series, ',');
}


//
//  CT_Comma: C
//
// Must have a comparison function, otherwise SORT would not work on arrays
// with commas in them.
//
REBINT CT_Comma(NoQuote(const Cell*) a, NoQuote(const Cell*) b, bool strict)
{
    UNUSED(strict);  // no strict form of comparison
    UNUSED(a);
    UNUSED(b);

    return 0;  // All commas are equal
}


//
//  REBTYPE: C
//
REBTYPE(Comma)
{
    switch (ID_OF_SYMBOL(verb)) {
      case SYM_COPY: { // since `copy/deep [1 , 2]` is legal, allow `copy ,`
        INCLUDE_PARAMS_OF_COPY;
        UNUSED(ARG(value));

        if (REF(part))
            fail (Error_Bad_Refines_Raw());

        UNUSED(REF(deep));

        return Init_Comma(OUT); }

      default: break;
    }

    fail (UNHANDLED);
}
