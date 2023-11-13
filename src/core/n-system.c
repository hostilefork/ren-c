//
//  File: %n-system.c
//  Summary: "native functions for system operations"
//  Section: natives
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
//  halt: native [
//
//  "Stops evaluation and returns to the input prompt."
//
//      return: []  ; !!! notation for divergent functions?
//  ]
//
DECLARE_NATIVE(halt)
{
    INCLUDE_PARAMS_OF_HALT;

    return Init_Thrown_With_Label(FRAME, Lib(NULL), Lib(HALT));
}


//
//  quit: native [
//
//  {Stop evaluating and return control to command shell or calling script}
//
//      return: []  ; !!! Notation for divergent functions?
//      /with "See: http://en.wikipedia.org/wiki/Exit_status"
//          [<opt> <void> any-value!]
//  ]
//
DECLARE_NATIVE(quit)
//
// QUIT is implemented via a thrown signal that bubbles up through the stack.
// It uses the value of its own native function as the name of the throw, like
// `throw/name value :quit`.
{
    INCLUDE_PARAMS_OF_QUIT;

    REBVAL *v = ARG(with);

    if (not REF(with)) {  // e.g. QUIT VOID or QUIT
        //
        // This returns an isotope if there is no arg, and labels it ~quit~
        // It's a pretty good generic signal of what happened if there's not
        // some explicit handling.
        //
        // !!! Should the decision of what happens here be done by the receiver
        // of the throw?  We have to turn the END to a BLANK!, but other than
        // that we might not editorialize...as it means we can't distinguish
        // `quit ~quit~` at the catch site.
        //
        Init_Word_Isotope(v, Canon(QUIT));
    }

    return Init_Thrown_With_Label(FRAME, v, Lib(QUIT));
}


//
//  exit-rebol: native [
//
//  {Stop the current Rebol interpreter (cannot be caught by CATCH/QUIT)}
//
//      return: []  ; !!! Notation for divergent functions?
//      status "See: http://en.wikipedia.org/wiki/Exit_status"
//          [<opt> <end> integer!]
//  ]
//
DECLARE_NATIVE(exit_rebol)
{
    INCLUDE_PARAMS_OF_EXIT_REBOL;

    int status;
    if (Is_Nulled(ARG(status)))
        status = EXIT_SUCCESS;
    else
        status = VAL_INT32(ARG(status));

    exit(status);
}


//
//  recycle: native [
//
//  "Recycles unused memory."
//
//      return: "Number of series nodes recycled (if applicable)"
//          [<opt> integer!]
//      /off "Disable auto-recycling"
//      /on "Enable auto-recycling"
//      /ballast "Trigger for auto-recycle (memory used)"
//          [integer!]
//      /torture "Constant recycle (for internal debugging)"
//      /watch "Monitor recycling (debug only)"
//      /verbose "Dump information about series being recycled (debug only)"
//  ]
//
DECLARE_NATIVE(recycle)
{
    INCLUDE_PARAMS_OF_RECYCLE;

    if (REF(off)) {
        GC_Disabled = true;
        return nullptr;
    }

    if (REF(on)) {
        GC_Disabled = false;
        TG_Ballast = TG_Max_Ballast;
    }

    if (REF(ballast)) {
        TG_Max_Ballast = VAL_INT32(ARG(ballast));
        TG_Ballast = TG_Max_Ballast;
    }

    if (REF(torture)) {
        GC_Disabled = false;
        TG_Ballast = 0;
    }

    if (GC_Disabled)
        return nullptr; // don't give misleading "0", since no recycle ran

    REBLEN count;

    if (REF(verbose)) {
      #if defined(NDEBUG)
        fail (Error_Debug_Only_Raw());
      #else
        REBSER *sweeplist = Make_Series_Core(100, FLAG_FLAVOR(NODELIST));
        count = Recycle_Core(false, sweeplist);
        assert(count == SER_USED(sweeplist));

        REBLEN index = 0;
        for (index = 0; index < count; ++index) {
            Node* node = *SER_AT(Node*, sweeplist, index);
            PROBE(node);
            UNUSED(node);
        }

        Free_Unmanaged_Series(sweeplist);

        REBLEN recount = Recycle_Core(false, NULL);
        assert(recount == count);
      #endif
    }
    else {
        count = Recycle();
    }

    if (REF(watch)) {
      #if defined(NDEBUG)
        fail (Error_Debug_Only_Raw());
      #else
        // There might should be some kind of generic way to set these kinds
        // of flags individually, perhaps having them live in SYSTEM/...
        //
        Reb_Opts->watch_recycle = not Reb_Opts->watch_recycle;
        Reb_Opts->watch_expand = not Reb_Opts->watch_expand;
      #endif
    }

    return Init_Integer(OUT, count);
}


//
//  limit-usage: native [
//
//  "Set a usage limit only once (used for SECURE)."
//
//      return: <none>
//      field [word!]
//          "eval (count) or memory (bytes)"
//      limit [any-number!]
//  ]
//
DECLARE_NATIVE(limit_usage)
{
    INCLUDE_PARAMS_OF_LIMIT_USAGE;

    option(SymId) sym = VAL_WORD_ID(ARG(field));

    // !!! comment said "Only gets set once"...why?
    //
    if (sym == SYM_EVAL) {
        if (Eval_Limit == 0)
            Eval_Limit = Int64(ARG(limit));
    }
    else if (sym == SYM_MEMORY) {
        if (PG_Mem_Limit == 0)
            PG_Mem_Limit = Int64(ARG(limit));
    }
    else
        fail (PARAM(field));

    return NONE;
}


//
//  check: native [
//
//  "Run an integrity check on a value in debug builds of the interpreter"
//
//      return: [<opt> any-value!]
//      value "System will terminate abnormally if this value is corrupt"
//          [<opt> any-value!]
//  ]
//
DECLARE_NATIVE(check)
//
// This forces an integrity check to run on a series.  In R3-Alpha there was
// no debug build, so this was a simple validity check and it returned an
// error on not passing.  But Ren-C is designed to have a debug build with
// checks that aren't designed to fail gracefully.  So this just runs that
// assert rather than replicating code here that can "tolerate" a bad series.
// Review the necessity of this native.
{
    INCLUDE_PARAMS_OF_CHECK;

#ifdef NDEBUG
    UNUSED(ARG(value));

    fail (Error_Debug_Only_Raw());
#else
    REBVAL *value = ARG(value);

    // For starters, check the memory (if it's bad, all other bets are off)
    //
    Check_Memory_Debug();

    // !!! Should call generic ASSERT_VALUE macro with more cases
    //
    if (ANY_SERIES(value)) {
        ASSERT_SERIES(VAL_SERIES(value));
    }
    else if (IS_FRAME(value)) {
        ASSERT_SERIES(VAL_ACTION_KEYLIST(value));
        ASSERT_ARRAY(ACT_DETAILS(VAL_ACTION(value)));
    }
    else if (ANY_CONTEXT(value)) {
        ASSERT_CONTEXT(VAL_CONTEXT(value));
    }

    return COPY(value);
#endif
}


// Fast count of number of binary digits in a number:
//
// https://stackoverflow.com/a/15327567/211160
//
int ceil_log2(unsigned long long x) {
    static const unsigned long long t[6] = {
        0xFFFFFFFF00000000ull,
        0x00000000FFFF0000ull,
        0x000000000000FF00ull,
        0x00000000000000F0ull,
        0x000000000000000Cull,
        0x0000000000000002ull
    };

    int y = (((x & (x - 1)) == 0) ? 0 : 1);
    int j = 32;
    int i;

    for (i = 0; i < 6; i++) {
    int k = (((x & t[i]) == 0) ? 0 : j);
        y += k;
        x >>= k;
        j >>= 1;
    }

    return y;
}


//
//  c-debug-tick: native [
//
//  {Get the evaluator tick count (currently only available in debug builds)}
//
//      return: [<opt> integer!]
//  ]
//
DECLARE_NATIVE(c_debug_tick)
{
    INCLUDE_PARAMS_OF_C_DEBUG_TICK;

  #if !defined(NDEBUG) && DEBUG_COUNT_TICKS
    return Init_Integer(OUT, TG_tick);
  #else
    return nullptr;
  #endif
}


//
//  c-debug-break: native [
//
//  {Break at next evaluation point (only use when running under C debugger)}
//
//      return: <void>
//          "Invisibly returns what the expression to the right would have"
//  ]
//
DECLARE_NATIVE(c_debug_break)
{
    INCLUDE_PARAMS_OF_C_DEBUG_BREAK;

  #if !INCLUDE_C_DEBUG_BREAK_NATIVE
    fail (Error_Debug_Only_Raw());
  #else
    #if DEBUG_COUNT_TICKS
        //
        // For instance with:
        //
        //    print c-debug-break mold value
        //
        // Queue it so the break happens right before the MOLD, not after it
        // happened and has been passed as an argument.
        //
        TG_break_at_tick = frame_->tick + 1;
        return VOID;
     #else
        // No tick counting or tick-break checking, but still want some
        // debug break functionality (e.g. callgrind build).  Break here--
        // you'll have to step up out into the evaluator stack.
        //
        debug_break();
        return VOID;
      #endif
  #endif
}
