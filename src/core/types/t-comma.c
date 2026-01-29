//
//  file: %t-comma.c
//  summary: "Comma Datatype"
//  section: datatypes
//  project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  homepage: https://github.com/metaeducation/ren-c/
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


// The special behavior of commas makes them "glue" their rendering to the
// thing on their left.
//
// !!! Should this be limited to list rendering, and the MOLD of a lone
// blank doing something special?  Probably so.
//
IMPLEMENT_GENERIC(MOLDIFY, Is_Blank)
{
    INCLUDE_PARAMS_OF_MOLDIFY;

    Element* v = Element_ARG(VALUE);
    Molder* mo = Cell_Handle_Pointer(Molder, ARG(MOLDER));
    bool form = did ARG(FORM);

    UNUSED(form);
    UNUSED(v);

    Size size = Strand_Size(mo->strand);
    if (
        size > mo->base.size + 1
        and *Binary_At(mo->strand, size - 1) == ' '  // not multibyte char
        and *Binary_At(mo->strand, size - 2) != ','  // also safe compare
    ){
        *Binary_At(mo->strand, size - 1) = ',';
    }
    else
        Append_Codepoint(mo->strand, ',');

    return TRASH_OUT;
}


IMPLEMENT_GENERIC(EQUAL_Q, Is_Blank)
{
    INCLUDE_PARAMS_OF_EQUAL_Q;

    Element* v1 = Element_ARG(VALUE1);
    Element* v2 = Element_ARG(VALUE2);
    UNUSED(ARG(RELAX));

    assert(Is_Blank(v1) and Is_Blank(v2));

    return LOGIC_OUT(true);  // all commas are equal
}
