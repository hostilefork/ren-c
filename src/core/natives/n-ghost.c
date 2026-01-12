//
//  file: %n-ghost.c
//  summary: "Native Functions for GHOST! Datatype (COMMENT, ELIDE, etc.)"
//  section: natives
//  project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012-2025 Ren-C Open Source Contributors
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
// For a long time, vanishing functions were not implemented as natives, due
// to the desire to prove that they could be implemented in usermode.  But
// now that GHOST! is well understood and simple to use (vs. being esoteric
// evaluator tricks on special infix functions), there's no reason not to
// just implement them as fast intrinsics.
//

#include "sys-core.h"


//
//  nihil: vanishable native [
//
//  "Generate GHOST! (arity-0 COMMENT)"
//
//      return: [ghost!]
//  ]
//
DECLARE_NATIVE(NIHIL)
{
    INCLUDE_PARAMS_OF_NIHIL;

    return GHOST;
}


//
//  ghost?: native:intrinsic [
//
//  "Tells you if argument is a comma antiform (unstable)"
//
//      return: [logic?]
//      ^value '[any-value?]
//  ]
//
DECLARE_NATIVE(GHOST_Q)
{
    INCLUDE_PARAMS_OF_GHOST_Q;

    Value* v = Unchecked_Intrinsic_Arg(LEVEL);

    // !!! what about FAILURE!?  Should this panic?

    return LOGIC(Is_Ghost(v));
}


//
//  void?: native:intrinsic [
//
//  "Is VALUE a VOID (antiform comma, e.g. ghost!) or HEAVY VOID (empty pack!)"
//
//      return: [logic?]
//      ^value '[any-value?]
//  ]
//
DECLARE_NATIVE(VOID_Q)
{
    INCLUDE_PARAMS_OF_VOID_Q;

    Value* v = Unchecked_Intrinsic_Arg(LEVEL);

    // !!! what about FAILURE!?  Should this panic?

    return LOGIC(Any_Void(v));
}


//
//  heavy-void?: native:intrinsic [
//
//  "Is VALUE specifically HEAVY VOID (empty pack!)"
//
//      return: [logic?]
//      ^value '[any-value?]
//  ]
//
DECLARE_NATIVE(HEAVY_VOID_Q)
{
    INCLUDE_PARAMS_OF_HEAVY_VOID_Q;

    Value* v = Unchecked_Intrinsic_Arg(LEVEL);

    // !!! what about FAILURE!?  Should this panic?

    return LOGIC(Is_Heavy_Void(v));
}


//
//  comment: vanishable native:intrinsic [
//
//  "Skip one element ahead, doing no evaluation (see also ELIDE)"
//
//      return: [ghost!]
//      @skipped "Literal to skip, (comment print -[x]-) disallowed"
//          '[any-list? any-utf8? blob! any-scalar?]
//  ]
//
DECLARE_NATIVE(COMMENT)
{
    INCLUDE_PARAMS_OF_COMMENT;

    Element* v = As_Element(Unchecked_Intrinsic_Arg(LEVEL));

    if (not (Any_List(v) or Any_Utf8(v) or Is_Blob(v) or Any_Scalar(v)))
       panic (Error_Bad_Intrinsic_Arg_1(LEVEL));

    return GHOST;
}


//
//  elide: vanishable native:intrinsic [
//
//  "Argument evaluated, result discarded (not FAILURE!, or packs w/FAILURE!s)"
//
//      return: [ghost!]
//      ^discarded '[any-stable? pack! ghost!]
//  ]
//
DECLARE_NATIVE(ELIDE)
{
    INCLUDE_PARAMS_OF_ELIDE;  // no ARG(DISCARDED), parameter is intrinsic

    Value* v = Unchecked_Intrinsic_Arg(LEVEL);

    require (
      Ensure_No_Failures_Including_In_Packs(v)
    );

    return GHOST;
}


//
//  elide-if-void: vanishable native:intrinsic [
//
//  "Argument is evaluative, but discarded if VOID"
//
//      return: [any-value?]
//      ^value '[any-value?]  ; ghost! is passed through
//  ]
//
DECLARE_NATIVE(ELIDE_IF_VOID)
{
    INCLUDE_PARAMS_OF_ELIDE_IF_VOID;

    Value* v = Unchecked_Intrinsic_Arg(LEVEL);

    if (Any_Void(v))
        return GHOST;

    return COPY(v);  // trust that a piped FAILURE! etc. behaves appropriately
}


//
//  ignore: native:intrinsic [
//
//  "Argument evaluated and discarded (even FAILURE! and undecayable packs)"
//
//      return: [ghost!]
//      ^discarded '[any-value?]
//  ]
//
DECLARE_NATIVE(IGNORE)
{
    INCLUDE_PARAMS_OF_IGNORE;  // no ARG(DISCARDED), parameter is intrinsic

    return GHOST;
}


//
//  unvoid: native:intrinsic [  ; !!! Better name?
//
//  "If the argument is a GHOST!, convert it to a HEAVY VOID, else passthru"
//
//      return: [any-value?]
//      ^value '[any-value?]
//  ]
//
DECLARE_NATIVE(UNVOID)
//
// Functions should be cautious about "leaking voids", as we want to limit
// the cases where expressions vanish some of the time and not others.
{
    INCLUDE_PARAMS_OF_UNVOID;

    Value* v = Unchecked_Intrinsic_Arg(LEVEL);

    if (Is_Ghost(v))
        return Init_Heavy_Void(OUT);

    return COPY(v);
}
