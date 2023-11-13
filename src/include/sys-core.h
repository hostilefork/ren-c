//
//  File: %sys-core.h
//  Summary: "Single Complete Include File for Using the Internal Api"
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012-2021 Ren-C Open Source Contributors
// Copyright 2012 REBOL Technologies
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
// This is the main include file used in the implementation of the core.
//
// * It defines all the data types and structures used by the auto-generated
//   function prototypes.  This includes the obvious REBINT, REBVAL, REBSER.
//   It also includes any enumerated type parameters to functions which are
//   shared between various C files.
//
// * With those types defined, it includes %tmp-internals.h - which is all
//   all the non-inline "internal API" functions.  This list of function
//   prototypes is generated automatically by a Rebol script that scans the
//   %.c files during the build process.
//
// * Next it starts including various headers in a specific order.  These
//   build on the data definitions and call into the internal API.  Since they
//   are often inline functions and not macros, the complete prototypes and
//   data definitions they use must have already been defined.
//
// %sys-core.h is supposed to be platform-agnostic.  All the code which would
// include something like <windows.h> would be linked in as extensions.  Yet
// if a file wishes to include %sys-core.h and <windows.h>, it should do:
//
//     #define WIN32_LEAN_AND_MEAN  // usually desirable for leaner inclusion
//     #include <windows.h>
//
//     /* #include any non-Rebol windows dependencies here */
//
//     #undef IS_ERROR // means something different
//     #undef max // same
//     #undef min // same
//     #undef OUT  // %minwindef.h defines this, we have a better use for it
//     #undef VOID  // %winnt.h defines this, we have a better use for it
//     #include "sys-core.h"
//
// !!! Because this header is included by all files in the core, it has been a
// bit of a dumping ground for flags and macros that have no particular home.
// Addressing that is an ongoing process.
//

#include "tmp-version.h"  // historical 5 numbers in a TUPLE! (see %platforms.r)
#include "reb-config.h"


//=//// INCLUDE EXTERNAL API /////////////////////////////////////////////=//
//
// Historically, Rebol source did not include the external library, because it
// was assumed the core would never want to use the less-privileged and higher
// overhead API.  However, libRebol now operates on REBVAL* directly (though
// opaque to clients).  It has many conveniences, and is the preferred way to
// work with isolated values that need indefinite duration.
//
#include <stdlib.h>  // size_t and other types used in rebol.h
#include "pstdint.h"  // polyfill <stdint.h> for pre-C99/C++11 compilers
#include "pstdbool.h"  // polyfill <stdbool.h> for pre-C99/C++11 compilers
#include "rebol.h"

// assert() is enabled by default; disable with `#define NDEBUG`
// http://stackoverflow.com/a/17241278
//
#if !defined(NDEBUG) && TO_WINDOWS && INCLUDE_C_DEBUG_BREAK_NATIVE

    #include <assert.h>  // include so it will think it has been included
    #undef assert  // (this way its include guard prevents it defining again)

    #include "debugbreak.h"
    #include <stdio.h>

    // For some reason, Windows implementation of "_wassert" corrupts the stack
    // by calling abort(), to where you only see at most 3 C stack frames above
    // the assert in the VSCode debugger.  That's unusable, so replace it.

    inline static void asserted(const char* file, int line, const char* expr) {
        printf("Assertion failure: %s\n", expr);
        printf("Line %d, File: %s\n", line, file);
        debug_break();  // calling debug_break() allows us to step afterward
    }

    #define assert(expr) \
        ((expr) ? (void)0 : asserted(__FILE__, __LINE__, #expr))
#else
    #include <assert.h>
#endif



//=//// STANDARD DEPENDENCIES FOR CORE ////////////////////////////////////=//

#include "reb-c.h"

#if CPLUSPLUS_11 && DEBUG_HAS_PROBE
    //
    // We allow you to do PROBE(some_integer) as well as PROBE(some_rebval)
    // etc. in C++11 - and the stringification comes from the << operator.
    // We must do C++ includes before defining the fail() macro, otherwise
    // the use of fail() methods in C++ won't be able to compile.
    //
    #include <sstream>
#endif

#include <stdarg.h> // va_list, va_arg()...
#include <string.h>
#include <math.h>
#include <stddef.h> // for offsetof()


//=//// ALLOW ONLY MINIMAL USE OF STDIO.H IN RELEASE BUILDS ////////////////=//
//
// The core build of Rebol published in R3-Alpha sought to not be dependent
// on <stdio.h>.  Since Rebol has richer tools like WORD!s and BLOCK! for
// dialecting, including a brittle historic string-based C "mini-language" of
// printf into the executable was a wasteful dependency.  Also, many
// implementations are clunky:
//
// http://blog.hostilefork.com/where-printf-rubber-meets-road/
//
// Attempts to use macro trickery to make inclusions of <stdio.h> in release
// build were used for some time.  These tricks began to run afoul of recent
// compilers that are cavalier about making the inclusion of one standard
// header mean you must want them all...so trying to avoid printf() being
// *available* was nigh impossible.
//
// Current focus on avoiding dependencies on printf() are at the object and
// linker level, where in general it's more direct to examine bloat.
//
#if !defined(NDEBUG) || DEBUG_PRINTF_FAIL_LOCATIONS || DEBUG_HAS_PROBE
    //
    // Debug builds may use printf() and such liberally (helps to debug the
    // Ren-C I/O system itself!)
    //
    #include <stdio.h>

    // NOTE: F/PRINTF DOES NOT ALWAYS FFLUSH() BUFFERS AFTER NEWLINES; it is
    // an "implementation defined" behavior, and never applies to redirects:
    //
    // https://stackoverflow.com/a/5229135/211160
    //
    // So when writing information you intend to be flushed before a potential
    // crash, be sure to fflush(), regardless of using `\n` or not.

    // The "right" way in C99 to print out things like uintptr_t is to use
    // weird type specifiers from <inttypes.h>, which looks like:
    //
    //     uintptr_t p = SOME_VALUE;
    //     printf("Here's a pointer for you: %" PRIxPTR "\n", p);
    //
    // So if a uintptr_t is being used to represent an integer, we'd use
    // `PRIuPTR`.  You get compiler warnings otherwise.
    //
    // *or you can just cast it to int and lose precision*.  But since printf()
    // is only included in debug builds, that loss of precision could wind up
    // being a problem in what's being diagnosed.  :-/  So we use it.
    //
  #if CPLUSPLUS_11
    #include <cinttypes>  // GCC requires to get macros, MSVC doesn't
  #else
    #define __STDC_FORMAT_MACROS
    #include "inttypes.h"
  #endif
#endif


// Internal configuration:
#define STACK_MIN   4000        // data stack increment size
#define STACK_LIMIT 400000      // data stack max (6.4MB)
#define MIN_COMMON 10000        // min size of common buffer
#define MAX_COMMON 100000       // max size of common buffer (shrink trigger)
#define MAX_NUM_LEN 64          // As many numeric digits we will accept on input
#define MAX_EXPAND_LIST 5       // number of series-1 in Prior_Expand list


//=//// FORWARD-DECLARE TYPES USED IN %tmp-internals.h ////////////////////=//
//
// This does all the forward definitions that are necessary for the compiler
// to be willing to build %tmp-internals.h.  Some structures are fully defined
// and some are only forward declared.  See notes in %structs/README.md
//

#include "tmp-symid.h"  // small integer IDs for words (e.g. SYM_THRU, SYM_ON)

#include "reb-defs.h"  // basic typedefs like Byte (wraps symbol IDs as SymId)

#include "structs/sys-rebnod.h"
#include "mem-pools.h"

#include "tmp-kinds.h"  // Defines `enum Reb_Kind` (REB_BLOCK, REB_TEXT, etc)
#include "sys-ordered.h"  // changing the type enum *must* update these macros

#include "structs/sys-rebcel.h"
#include "structs/sys-rebval.h"  // low level Rebol cell structure definition

#include "sys-flavor.h"  // series subclass byte (uses sizeof(REBVAL))

#include "structs/sys-rebser.h"  // series structure definition, embeds REBVAL

#include "structs/sys-rebarr.h"  // array structure (REBSER subclass)
#include "structs/sys-rebact.h"  // action structure
#include "structs/sys-rebctx.h"  // context structure
#include "structs/sys-rebpat.h"  // virtual binding patch definitions

#include "structs/sys-rebchr.h"  // Utf8(*) is Byte* in validated UTF8

#include "structs/sys-rebfed.h"  // REBFED (feed) definition
#include "structs/sys-rebjmp.h"  // Jump state (for TRAP)
#include "structs/sys-rebfrm.h"  // C struct for running frame, uses REBFED


#include "sys-hooks.h"  // function pointer definitions


// There is a significant amount of code that wants to enumerate the parameters
// of functions or keys of a frame.  It's fairly complex logic, because the
// same frame context is viewed different ways depending on what phase is
// encoded in the FRAME! value cell.  Doing it in a callback style creates a
// lot of inconvenience for C code, needing to wrap up state...so this does
// it with an enumeration struct.

enum Reb_Var_Visibility {
    VAR_VISIBILITY_ALL,
    VAR_VISIBILITY_INPUTS,
    VAR_VISIBILITY_NONE
};

struct Reb_Enum_Vars {
    const REBKEY *key;
    const REBKEY *key_tail;
    REBPAR *param;
    enum Reb_Var_Visibility visibility;
    REBVAR *var;
    REBLEN index;  // important for enumerations that are binding

    // !!! Enumerating key/val pairs in modules in the "sea of words" model is
    // tricky, as what it really is hooks the variables in a linked list off
    // the Symbol series node for the symbol.  This is accessed via a global
    // hash table that can expand and rearrange freely...it's not possible
    // to lock the table during enumeration.  Locking the module itself may
    // be possible, but the iteration order could get messed up by a hash
    // table resize.  There are technical ways to attack such problems that
    // are within the realm of possibility, but building an array and then
    // enumerating the array is the easiest near-term option.  This is a list
    // of the bound words.
    //
    Context(*) ctx;
    Array(*) wordlist;
    REBVAL *word;
    REBVAL *word_tail;
    const Raw_Symbol* keybuf;  // backing store for key
};

typedef struct Reb_Enum_Vars EVARS;


//=////////////////////////////////////////////////////////////////////////=//
//
// #INCLUDE THE AUTO-GENERATED FUNCTION PROTOTYPES FOR THE INTERNAL API
//
//=////////////////////////////////////////////////////////////////////////=//
//
// The somewhat-awkward requirement to have all the definitions up-front for
// all the prototypes, instead of defining them in a hierarchy, comes from
// the automated method of prototype generation.  If they were defined more
// naturally in individual includes, it could be cleaner...at the cost of
// needing to update prototypes separately from the definitions.
//
// See %make/make-headers.r for the generation of this list.
//

#include "tmp-internals.h"
#include "tmp-native-fwd-decls.h"


/***********************************************************************
**
**  Structures
**
***********************************************************************/

//-- Measurement Variables:
typedef struct rebol_stats {
    REBI64  Series_Memory;
    REBLEN  Series_Made;
    REBLEN  Series_Freed;
    REBLEN  Series_Expanded;
    REBLEN  Recycle_Counter;
    REBLEN  Recycle_Series_Total;
    REBLEN  Recycle_Series;
    REBI64  Recycle_Prior_Eval;
    REBLEN  Mark_Count;
    REBLEN  Blocks;
    REBLEN  Objects;
} REB_STATS;

//-- Options of various kinds:
typedef struct rebol_opts {
    bool  watch_recycle;
    bool  watch_series;
    bool  watch_expand;
    bool  crash_dump;
} REB_OPTS;


/***********************************************************************
**
**  Threaded Global Variables
**
***********************************************************************/

// !!! In the R3-Alpha open source release, there had apparently been a switch
// from the use of global variables to the classification of all globals as
// being either per-thread (TVAR) or for the whole program (PVAR).  This
// was apparently intended to use the "thread-local-variable" feature of the
// compiler.  It used the non-standard `__declspec(thread)`, which as of C11
// and C++11 is standardized as `thread_local`.
//
// Despite this basic work for threading, greater issues were not hammered
// out.  And so this separation really just caused problems when two different
// threads wanted to work with the same data (at different times).  Such a
// feature is better implemented as in the V8 JavaScript engine as "isolates"

#ifdef __cplusplus
    #define PVAR extern "C" RL_API
    #define TVAR extern "C" RL_API
#else
    // When being preprocessed by TCC and combined with the user - native
    // code, all global variables need to be declared
    // `extern __attribute__((dllimport))` on Windows, or incorrect code
    // will be generated for dereferences.  Hence these definitions for
    // PVAR and TVAR allow for overriding at the compiler command line.
    //
    #if !defined(PVAR)
        #define PVAR extern RL_API
    #endif
    #if !defined(TVAR)
        #define TVAR extern RL_API
    #endif
#endif

#include "sys-globals.h"  // includes things like TG_tick, used by panic()


#include "sys-panic.h"  // "blue screen of death"-style termination
#include "sys-casts.h"  // coercion macros like SER(), uses panic() to alert

#include "sys-mold.h"



/***********************************************************************
**
**  Constants
**
***********************************************************************/

enum Boot_Phases {
    BOOT_START = 0,
    BOOT_LOADED,
    BOOT_ERRORS,
    BOOT_MEZZ,
    BOOT_DONE
};

enum Boot_Levels {
    BOOT_LEVEL_BASE,
    BOOT_LEVEL_SYS,
    BOOT_LEVEL_MODS,
    BOOT_LEVEL_FULL
};

// Modes allowed by Make_Function:
enum {
    MKF_RETURN      = 1 << 0,   // give a return COPY(but local RETURN: overrides)
    MKF_KEYWORDS    = 1 << 1,   // respond to tags like <opt>, <with>, <local>
    MKF_2           = 1 << 2,

    // These flags are set during the process of spec analysis.  It helps
    // avoid the inefficiency of creating documentation frames on functions
    // that don't have any.
    //
    MKF_HAS_DESCRIPTION = 1 << 3,
    MKF_HAS_TYPES = 1 << 4,
    MKF_HAS_NOTES = 1 << 5,

    // These flags are also set during the spec analysis process.
    //
    MKF_HAS_RETURN = 1 << 6
};

#define MKF_MASK_NONE 0 // no special handling



#define TAB_SIZE 4



#define ALL_BITS \
    ((REBLEN)(-1))


typedef int cmp_t(void *, const void *, const void *);
extern void reb_qsort_r(void *a, size_t n, size_t es, void *thunk, cmp_t *cmp);



#include "tmp-constants.h"

// %tmp-paramlists.h is the file that contains macros for natives and actions
// that map their argument names to indices in the frame.  This defines the
// macros like INCLUDE_ARGS_FOR_INSERT which then allow you to naturally
// write things like REF(part) and ARG(limit), instead of the brittle integer
// based system used in R3-Alpha such as D_REF(7) and D_ARG(3).
//
#include "tmp-paramlists.h"

#include "tmp-boot.h"
#include "tmp-sysobj.h"




#include "tmp-error-funcs.h" // functions below are called


#include "sys-trap.h" // includes PUSH_TRAP, fail()

#include "sys-node.h"


// Lives in %sys-bind.h, but needed for Copy_Cell() and Derelativize()
//
inline static void INIT_BINDING_MAY_MANAGE(
    Cell(*) out,
    const REBSER* binding
);

#include "sys-track.h"
#include "datatypes/sys-value.h"  // these defines don't need series accessors


enum rebol_signals {
    //
    // SIG_RECYCLE indicates a need to run the garbage collector, when
    // running it synchronously could be dangerous.  This is important in
    // particular during memory allocation, which can detect crossing a
    // memory usage boundary that suggests GC'ing would be good...but might
    // be in the middle of code that is halfway through manipulating a
    // managed series.
    //
    SIG_RECYCLE = 1 << 0,

    // SIG_HALT means return to the topmost level of the evaluator, regardless
    // of how deep a debug stack might be.  It is the only instruction besides
    // QUIT and RESUME that can currently get past a breakpoint sandbox.
    //
    SIG_HALT = 1 << 1,

    // SIG_INTERRUPT indicates a desire to enter an interactive debugging
    // state.  Because the ability to manage such a state may not be
    // registered by the host, this could generate an error.
    //
    SIG_INTERRUPT = 1 << 2
};

inline static void SET_SIGNAL(Flags f) { // used in %sys-series.h
    Eval_Signals |= f;

    if (Eval_Countdown == -1)  // already set to trigger on next tick...
        return;  // ...we already reconciled the dose

    assert(Eval_Countdown > 0);  // transition to 0 triggers signals

    // This forces the next step in the evaluator to count down to 0 and
    // trigger an interrupt.  But we have to reconcile the count first.
    //
    Total_Eval_Cycles += Eval_Dose - Eval_Countdown;
  #if !defined(NDEBUG)
    assert(Total_Eval_Cycles == Total_Eval_Cycles_Doublecheck);
  #endif

    Eval_Countdown = -1;
}

#define GET_SIGNAL(f) \
    (did (Eval_Signals & (f)))

#define CLR_SIGNAL(f) \
    cast(void, Eval_Signals &= ~(f))


#include "datatypes/sys-series.h"
#include "datatypes/sys-array.h"  // Array(*) used by UTF-8 string bookmarks


//=//// LIB BUILTINS ACCESS MACRO //////////////////////////////////////////=//

#include "sys-symbol.h"

inline static const REBVAR *Try_Lib_Var(SymId id) {
    assert(id < LIB_SYMS_MAX);

    // !!! We allow a "removed state", in case modules implement a
    // feature for dropping variables.
    //
    if (INODE(PatchContext, &PG_Lib_Patches[id]) == nullptr)
        return nullptr;

    return cast(REBVAR*, ARR_SINGLE(&PG_Lib_Patches[id]));
}

#define Lib(name) \
    Try_Lib_Var(SYM_##name)

inline static REBVAR *Force_Lib_Var(SymId id) {
    REBVAR *var = m_cast(REBVAR*, Try_Lib_Var(id));
    if (var)
        return var;
    return Append_Context(Lib_Context, Canon_Symbol(id));
}

#define force_Lib(name) \
    Force_Lib_Var(SYM_##name)

#define SysUtil(name) \
    cast(const REBVAR*, MOD_VAR(Sys_Context, Canon_Symbol(SYM_##name), true))


//=//// CONTINUE VALUE TYPES ///////////////////////////////////////////////=//

#include "sys-void.h"
#include "sys-trash.h"  // quasi-void with special behavior in debug build

#include "datatypes/sys-blank.h"

#include "datatypes/sys-comma.h"

#include "datatypes/sys-integer.h"
#include "datatypes/sys-decimal.h"

#include "sys-protect.h"


#include "datatypes/sys-binary.h"  // BIN_XXX(), etc. used by strings

#include "datatypes/sys-char.h"  // use Init_Integer() for bad codepoint error
#include "datatypes/sys-string.h"  // SymId needed for typesets

#include "datatypes/sys-quoted.h"

#include "datatypes/sys-pair.h"

#include "sys-stack.h"

#include "datatypes/sys-action.h"
#include "datatypes/sys-context.h"  // needs actions for FRAME! contexts

#include "datatypes/sys-word.h"  // needs to know about QUOTED! for binding
#include "sys-nulled.h"
#include "datatypes/sys-logic.h"
#include "datatypes/sys-datatype.h"  // uses words

#include "datatypes/sys-error.h"

#include "datatypes/sys-bitset.h"

#include "sys-patch.h"
#include "sys-bind.h" // needs PUSH() and TOP from %sys-stack.h

#include "datatypes/sys-typeset.h"  // needed for keys in contexts

#include "datatypes/sys-token.h"
#include "datatypes/sys-sequence.h"  // also needs PUSH()

#include "sys-roots.h"


#include "sys-throw.h"
#include "sys-feed.h"
#include "datatypes/sys-frame.h"  // needs words for frame-label helpers

#include "datatypes/sys-time.h"
#include "datatypes/sys-handle.h"
#include "datatypes/sys-map.h"
#include "datatypes/sys-varargs.h"

#include "sys-eval.h"  // low-level single-step evaluation API
#include "sys-do.h"  // higher-level evaluate-until-end API

#include "sys-pick.h"
