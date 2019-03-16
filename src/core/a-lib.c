//
//  File: %a-lib.c
//  Summary: "Lightweight Export API (REBVAL as opaque type)"
//  Section: environment
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2019 Rebol Open Source Contributors
// REBOL is a trademark of REBOL Technologies
//
// See README.md and CREDITS.md for more information.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
//=////////////////////////////////////////////////////////////////////////=//
//
// This is the "external" API, and %rebol.h contains its exported
// definitions.  That file (and %make-reb-lib.r which generates it) contains
// comments and notes which will help understand it.
//
// What characterizes the external API is that it is not necessary to #include
// the extensive definitions of `struct REBSER` or the APIs for dealing with
// all the internal details (e.g. PUSH_GC_GUARD(), which are easy to get
// wrong).  Not only does this simplify the interface, but it also means that
// the C code using the library isn't competing as much for definitions in
// the global namespace.
//
// Also, due to the nature of REBNOD (see %sys-node.h), it's possible to feed
// the scanner with a list of pointers that may be to UTF-8 strings or to
// Rebol values.  The behavior is to "splice" in the values at the point in
// the scan that they occur, e.g.
//
//     REBVAL *item1 = ...;
//     REBVAL *item2 = ...;
//     REBVAL *item3 = ...;
//
//     REBARR *result = rebRun(
//         "if not", item1, "[\n",
//             item2, "| print {Close brace separate from content}\n",
//         "] else [\n",
//             item3, "| print {Close brace with content}]\n",
//         rebEND
//     );
//
// (The rebEND is needed by the variadic processing, but C99-based macros or
// other language bindings can inject it automatically...only C89 has no way
// to work around it.)
//
// While the approach is flexible, any token must appear fully inside its
// UTF-8 string component.  So you can't--for instance--divide a scan up like
// ("{abc", "def", "ghi}") and get the TEXT! {abcdefghi}.  On that note,
// ("a", "/", "b") produces `a / b` and not the PATH! `a/b`.
//
//==//// NOTES ////////////////////////////////////////////////////////////=//
//
// Each exported routine here has a name RL_rebXxxYyy.  This is a name by
// which it can be called internally from the codebase like any other function
// that is part of the core.  However, macros for calling it from the core
// are given as `#define rebXxxYyy RL_rebXxxYyy`.  This is a little bit nicer
// and consistent with the way it looks when an external client calls the
// functions.
//
// Then extension clients use macros which have you call the functions through
// a struct-based "interface" (similar to the way that interfaces work in
// something like COM).  Here the macros merely pick the API functions through
// a table, e.g. `#define rebXxxYyy interface_struct->rebXxxYyy`.  This means
// paying a slight performance penalty to dereference that API per call, but
// it keeps API clients from depending on the conventional C linker...so that
// DLLs can be "linked" against a Rebol EXE.
//
// (It is not generically possible to export symbols from an executable, and
// just in general there's no cross-platform assurances about how linking
// works, so this provides the most flexibility.)
//

#include "sys-core.h"


// "Linkage back to HOST functions. Needed when we compile as a DLL
// in order to use the OS_* macro functions."
//
#ifdef REB_API  // Included by C command line
    const REBOL_HOST_LIB *Host_Lib = nullptr;
    EXTERN_C REBOL_HOST_LIB Host_Lib_Init;
#endif


//
// rebEnterApi_Internal: RL_API
//
// This stub is added automatically to the calling wrappers.
//
// !!! Review how much checking one wants to do when calling API routines,
// and what the balance should be of debug vs. release.  Right now, this helps
// in particular notice if the core tries to use an API function before the
// proper moment in the boot.
//
void RL_rebEnterApi_internal(void) {
    if (not Host_Lib)
        panic ("rebStartup() not called before API call");
}


//=//// SERIES-BACKED ALLOCATORS //////////////////////////////////////////=//
//
// These are replacements for malloc(), realloc(), and free() which use a
// byte-sized REBSER as the backing store for the data.
//
// One benefit of using a series is that it offers more options for automatic
// memory management (such as being freed in case of a fail(), vs. leaked as
// a malloc() would, or perhaps being GC'd when a particular FRAME! ends).
//
// It also has the benefit of helping interface with client code that has
// been stylized to use malloc()-ish hooks to produce data, when the eventual
// target of that data is a Rebol series.  It does this without exposing
// REBSER* internals to the external API, by allowing one to "rebRepossess()"
// the underlying series as a BINARY! REBVAL*.
//


//
//  rebMalloc: RL_API
//
// * Unlike plain malloc(), this will fail() instead of return null if an
//   allocation cannot be fulfilled.
//
// * Like plain malloc(), if size is zero, the implementation just has to
//   return something that free() will take.  A backing series is added in
//   this case vs. returning null, in order to avoid null handling in other
//   routines (e.g. rebRepossess() or handle lifetime control functions).
//
// * Because of the above points, null is *never* returned.
//
// * It tries to be like malloc() by giving back a pointer "suitably aligned
//   for the size of any fundamental type".  See notes on ALIGN_SIZE.
//
// !!! rebAlignedMalloc() could exist to take an alignment, which could save
// on wasted bytes when ALIGN_SIZE > sizeof(REBSER*)...or work with "weird"
// large fundamental types that need more alignment than ALIGN_SIZE.
//
void *RL_rebMalloc(size_t size)
{
    REBSER *s = Make_Series_Core(
        ALIGN_SIZE // stores REBSER* (must be at least big enough for void*)
            + size // for the actual data capacity (may be 0...see notes)
            + 1, // for termination (even BINARY! has this, review necessity)
        sizeof(REBYTE), // rebRepossess() only creates binary series ATM
        SERIES_FLAG_DONT_RELOCATE // direct data pointer is being handed back!
            | SERIES_FLAG_ALWAYS_DYNAMIC // rebRepossess() needs bias field
    );

    REBYTE *ptr = BIN_HEAD(s) + ALIGN_SIZE;

    REBSER **ps = (cast(REBSER**, ptr) - 1);
    *ps = s; // save self in bytes *right before* data
    POISON_MEMORY(ps, sizeof(REBSER*)); // let ASAN catch underruns

    // !!! The data is uninitialized, and if it is turned into a BINARY! via
    // rebRepossess() before all bytes are assigned initialized, it could be
    // worse than just random data...MOLDing such a binary and reading those
    // bytes could be bad (due to, for instance, "trap representations"):
    //
    // https://stackoverflow.com/a/37184840
    //
    // It may be that rebMalloc() and rebRealloc() should initialize with 0
    // to defend against that, but that isn't free.  For now we make no such
    // promise--and leave it uninitialized so that address sanitizer notices
    // when bytes are used that haven't been assigned.
    //
    TERM_BIN_LEN(s, ALIGN_SIZE + size);

    return ptr;
}


//
//  rebRealloc: RL_API
//
// * Like plain realloc(), null is legal for ptr (despite the fact that
//   rebMalloc() never returns null, this can still be useful)
//
// * Like plain realloc(), it preserves the lesser of the old data range or
//   the new data range, and memory usage drops if new_size is smaller:
//
// https://stackoverflow.com/a/9575348
//
// * Unlike plain realloc() (but like rebMalloc()), this fails instead of
//   returning null, hence it is safe to say `ptr = rebRealloc(ptr, new_size)`
//
// * A 0 size is considered illegal.  This is consistent with the C11 standard
//   for realloc(), but not with malloc() or rebMalloc()...which allow it.
//
void *RL_rebRealloc(void *ptr, size_t new_size)
{
    assert(new_size > 0); // realloc() deprecated this as of C11 DR 400

    if (not ptr) // C realloc() accepts null
        return rebMalloc(new_size);

    // rebMalloc() returns aligned memory, but rebSteal() doesn't.  Use
    // memcpy() to extract the pointer as it may not be aligned.
    //
    char *ps = cast(char*, ptr) - sizeof(REBSER*);
    UNPOISON_MEMORY(ps, sizeof(REBSER*)); // need to underrun to fetch `s`

    REBSER *s;
    memcpy(&s, ps, sizeof(REBSER*)); // may not be pointer-aligned!

    REBCNT old_size = BIN_LEN(s) - ALIGN_SIZE;

    // !!! It's less efficient to create a new series with another call to
    // rebMalloc(), but simpler for the time being.  Switch to do this with
    // the same series node.
    //
    void *reallocated = rebMalloc(new_size);
    memcpy(reallocated, ptr, old_size < new_size ? old_size : new_size);
    Free_Unmanaged_Series(s);

    return reallocated;
}


//
//  rebFree: RL_API
//
// * As with free(), null is accepted as a no-op.
//
void RL_rebFree(void *ptr)
{
    if (not ptr)
        return;

    char *ps = cast(char*, ptr) - sizeof(REBSER*);
    UNPOISON_MEMORY(ps, sizeof(REBSER*)); // need to underrun to fetch `s`

    REBSER *s;
    memcpy(&s, ps, sizeof(REBSER*)); // may not be pointer aligned!
    if (s->header.bits & NODE_FLAG_CELL) {
        rebJumps(
            "PANIC [",
                "{rebFree() mismatched with allocator!}"
                "{Did you mean to use free() instead of rebFree()?}",
            "]",
            rebEND
        );
    }

    assert(BYTE_SIZE(s));

    Free_Unmanaged_Series(s);
}


//
//  rebRepossess: RL_API
//
// Alternative to rebFree() is to take over the underlying series as a
// BINARY!.  The old void* should not be used after the transition, as this
// operation makes the series underlying the memory subject to relocation.
//
// If the passed in size is less than the size with which the series was
// allocated, the overage will be treated as unused series capacity.
//
// Note that all rebRepossess()'d data will be terminated by an 0x00 byte
// after the end of its capacity.
//
// !!! All bytes in the allocation are expected to be initialized by this
// point, as failure to do so will mean reads crash the interpreter.  See
// remarks in rebMalloc() about the issue, and possibly doing zero fills.
//
// !!! It might seem tempting to use (BIN_LEN(s) - ALIGN_SIZE).  However,
// some routines make allocations bigger than they ultimately need and do not
// realloc() before converting the memory to a series...rebInflate() and
// rebDeflate() do this.  So a version passing the size will be necessary,
// and since C does not have the size exposed in malloc() and you track it
// yourself, it seems fair to *always* ask the caller to pass in a size.
//
REBVAL *RL_rebRepossess(void *ptr, size_t size)
{
    REBSER **ps = cast(REBSER**, ptr) - 1;
    UNPOISON_MEMORY(ps, sizeof(REBSER*)); // need to underrun to fetch `s`

    REBSER *s = *ps;
    assert(NOT_SERIES_FLAG(s, MANAGED));

    if (size > BIN_LEN(s) - ALIGN_SIZE)
        fail ("Attempt to rebRepossess() more than rebMalloc() capacity");

    assert(GET_SERIES_FLAG(s, DONT_RELOCATE));
    CLEAR_SERIES_FLAG(s, DONT_RELOCATE);

    if (IS_SER_DYNAMIC(s)) {
        //
        // Dynamic series have the concept of a "bias", which is unused
        // allocated capacity at the head of a series.  Bump the "bias" to
        // treat the embedded REBSER* (aligned to REBI64) as unused capacity.
        //
        SER_SET_BIAS(s, ALIGN_SIZE);
        s->content.dynamic.data += ALIGN_SIZE;
        s->content.dynamic.rest -= ALIGN_SIZE;
    }
    else {
        // Data is in REBSER node itself, no bias.  Just slide the bytes down.
        //
        memmove( // src overlaps destination, can't use memcpy()
            BIN_HEAD(s),
            BIN_HEAD(s) + ALIGN_SIZE,
            size
        );
    }

    TERM_BIN_LEN(s, size);
    return Init_Binary(Alloc_Value(), s);
}



//
//  Startup_Api: C
//
// RL_API routines may be used by extensions (which are invoked by a fully
// initialized Rebol core) or by normal linkage (such as from within the core
// itself).  A call to rebStartup() won't be needed in the former case.  So
// setup code that is needed to interact with the API needs to be done by the
// core independently.
//
void Startup_Api(void)
{
}


//
//  Shutdown_Api: C
//
// See remarks on Startup_Api() for the difference between this idea and
// rebShutdown.
//
void Shutdown_Api(void)
{
    assert(Host_Lib);
    Host_Lib = nullptr;
}


//
//  rebStartup: RL_API
//
// This function will allocate and initialize all memory structures used by
// the REBOL interpreter. This is an extensive process that takes time.
//
// `lib` is the host lib table (OS_XXX functions) which Rebol core does not
// take for granted--and assumes a host must provide to operate.  An example
// of this would be that getting the current UTC date and time varies from OS
// to OS, so for the NOW native to be implemented it has to call something
// outside of standard C...e.g. OS_GET_TIME().  So even though NOW is in the
// core, it will be incomplete without having that function supplied.
//
// !!! Increased modularization of the core, and new approaches, are making
// this concept obsolete.  For instance, the NOW native might not even live
// in the core, but be supplied by a "Timer Extension" which is considered to
// be sandboxed and non-core enough that having platform-specific code in it
// is not a problem.  Also, hooks can be supplied in the form of natives that
// are later HIJACK'd by some hosts (see PANIC and FAIL), as a way of
// injecting richer platform-or-scenario-specific code into a more limited
// default host operation.  It is expected that the OS_XXX functions will
// eventually disappear completely.
//
void RL_rebStartup(void)
{
    if (Host_Lib)
        panic ("rebStartup() called when it's already started");

    Host_Lib = &Host_Lib_Init;

    if (Host_Lib->size < HOST_LIB_SIZE)
        panic ("Host-lib wrong size");

    if (((HOST_LIB_VER << 16) + HOST_LIB_SUM) != Host_Lib->ver_sum)
        panic ("Host-lib wrong version/checksum");

    Startup_Core();
}


//
//  rebShutdown: RL_API
//
// Shut down a Rebol interpreter initialized with rebStartup().
//
// The `clean` parameter tells whether you want Rebol to release all of its
// memory accrued since initialization.  If you pass false, then it will
// only do the minimum needed for data integrity (it assumes you are planning
// to exit the process, and hence the OS will automatically reclaim all
// memory/handles/etc.)
//
// For rigor, the debug build *always* runs a "clean" shutdown.
//
void RL_rebShutdown(bool clean)
{
    // At time of writing, nothing Shutdown_Core() does pertains to
    // committing unfinished data to disk.  So really there is
    // nothing to do in the case of an "unclean" shutdown...yet.

  #if defined(NDEBUG)
    if (not clean)
        return; // Only do the work above this line in an unclean shutdown
  #else
    UNUSED(clean);
  #endif

    // Run a clean shutdown anyway in debug builds--even if the
    // caller didn't need it--to see if it triggers any alerts.
    //
    Shutdown_Core();
}


//
//  rebTick: RL_API
//
// If the executable is built with tick counting, this will return the tick
// without requiring any Rebol code to run (which would disrupt the tick).
//
long RL_rebTick(void)
{
  #ifdef DEBUG_COUNT_TICKS
    return cast(long, TG_Tick);
  #else
    return 0;
  #endif
}


//=//// VALUE CONSTRUCTORS ////////////////////////////////////////////////=//
//
// These routines are for constructing Rebol values from C primitive types.
// The general philosophy is that this stay limited.  Hence there is no
// constructor for making DATE! directly (one is expected to use MAKE DATE!
// and pass in parts that were constructed from integers.)  This also avoids
// creation of otherwise useless C structs, while the Rebol function designs
// are needed to create the values from the interpreter itself.
//
// * There's no function for returning a null pointer, because C's notion of
//   (void*)0 is used.  But note that the C standard permits NULL defined as
//   simply 0.  This breaks use in variadics, so it is advised to use C++'s
//   nullptr, or do `#define nullptr (void*)0
//
// * Routines with full written out names like `rebInteger()` return API
//   handles which must be rebRelease()'d.  Shorter versions like rebI() don't
//   return REBVAL* but are designed for transient use when evaluating, e.g.
//   `rebRun("print [", rebI(count), "]");` does not need to rebRelease()
//   the resulting variable because the evaluator frees it after use.
//
//=////////////////////////////////////////////////////////////////////////=//


//
//  rebVoid: RL_API
//
REBVAL *RL_rebVoid(void)
 { return Init_Void(Alloc_Value()); }


//
//  rebBlank: RL_API
//
REBVAL *RL_rebBlank(void)
 { return Init_Blank(Alloc_Value()); }


//
//  rebLogic: RL_API
//
// !!! For the C and C++ builds to produce compatible APIs, we assume the
// C <stdbool.h> gives a bool that is the same size as for C++.  This is not
// a formal guarantee, but there's no "formal" guarantee the `int`s would be
// compatible either...more common sense: https://stackoverflow.com/q/3529831
//
// Use DID on the bool, in case it's a "shim bool" (e.g. just some integer
// type) and hence may have values other than strictly 0 or 1.
//
//
REBVAL *RL_rebLogic(bool logic)
 { return Init_Logic(Alloc_Value(), did logic); }


//
//  rebChar: RL_API
//
REBVAL *RL_rebChar(uint32_t codepoint)
{
    if (codepoint > MAX_UNI)
        fail ("Codepoint out of range, see: https://forum.rebol.info/t/374");

    return Init_Char(Alloc_Value(), codepoint);
}


//
//  rebInteger: RL_API
//
// !!! Should there be rebSigned() and rebUnsigned(), in order to catch cases
// of using out of range values?
//
REBVAL *RL_rebInteger(int64_t i)
 { return Init_Integer(Alloc_Value(), i); }


//
//  rebDecimal: RL_API
//
REBVAL *RL_rebDecimal(double dec)
 { return Init_Decimal(Alloc_Value(), dec); }


//
//  rebSizedBinary: RL_API
//
// The name "rebBinary()" is reserved for use in languages who have some
// concept of data which can serve as a single argument because it knows its
// own length.  C doesn't have this for raw byte buffers, but JavaScript has
// things like Int8Array.
//
REBVAL *RL_rebSizedBinary(const void *bytes, size_t size)
{
    REBSER *bin = Make_Binary(size);
    memcpy(BIN_HEAD(bin), bytes, size);
    TERM_BIN_LEN(bin, size);

    return Init_Binary(Alloc_Value(), bin);
}


//
//  rebUninitializedBinary_internal: RL_API
//
// !!! This is a dicey construction routine that users shouldn't have access
// to, because it gives the internal pointer of the binary out.  The reason
// it exists is because emscripten's writeArrayToMemory() is based on use of
// an Int8Array.set() call.
//
// When large binary blobs come back from file reads/etc. we already have one
// copy of it.  We don't want to extract it into a temporary malloc'd buffer
// just to be able to pass it to reb.Binary() to make *another* copy.
//
// Note: It might be interesting to have a concept of "external" memory by
// which the data wasn't copied but a handle was kept to the JavaScript
// Int8Array that came back from fetch() (or whatever).  But emscripten does
// not at this time have a way to read anything besides the HEAP8:
//
// https://stackoverflow.com/a/43325166
//
REBVAL *RL_rebUninitializedBinary_internal(size_t size)
{
    REBSER *bin = Make_Binary(size);

    // !!! Caution, unfilled bytes, access or molding may be *worse* than
    // random by the rules of C if they don't get written!  Must be filled
    // immediately by caller--before a GC or other operation.
    //
    TERM_BIN_LEN(bin, size);

    return Init_Binary(Alloc_Value(), bin);
}


//
//  rebBinaryHead_internal: RL_API
//
// Complementary "evil" routine to rebUninitializedBinary().  Should not
// be generally used, as passing out raw pointers to binaries can have them
// get relocated out from under the caller.  If pointers are going to be
// given out in this fashion, there has to be some kind of locking semantics.
//
// (Note: This could be a second return value from rebUninitializedBinary(),
// but that would involve pointers-to-pointers which are awkward in
// emscripten and probably cheaper to make two direct WASM calls.
//
unsigned char *RL_rebBinaryHead_internal(const REBVAL *binary)
{
    return VAL_BIN_HEAD(binary);
}


//
//  rebSizedText: RL_API
//
// If utf8 does not contain valid UTF-8 data, this may fail().
//
REBVAL *RL_rebSizedText(const char *utf8, size_t size)
 { return Init_Text(Alloc_Value(), Make_Sized_String_UTF8(utf8, size)); }


//
//  rebText: RL_API
//
REBVAL *RL_rebText(const char *utf8)
 { return rebSizedText(utf8, strsize(utf8)); }


//
//  rebLengthedTextWide: RL_API
//
REBVAL *RL_rebLengthedTextWide(const REBWCHAR *wstr, unsigned int num_chars)
{
    DECLARE_MOLD (mo);
    Push_Mold(mo);

    for (; num_chars != 0; --num_chars, ++wstr)
        Append_Utf8_Codepoint(mo->series, *wstr);

    return Init_Text(Alloc_Value(), Pop_Molded_String(mo));
}


//
//  rebTextWide: RL_API
//
REBVAL *RL_rebTextWide(const REBWCHAR *wstr)
{
    DECLARE_MOLD (mo);
    Push_Mold(mo);

    for (; *wstr != 0; ++wstr)
        Append_Utf8_Codepoint(mo->series, *wstr);

    return Init_Text(Alloc_Value(), Pop_Molded_String(mo));
}


//
//  rebHandle: RL_API
//
// !!! The HANDLE! type has some complexity to it, because function pointers
// in C and C++ are not actually guaranteed to be the same size as data
// pointers.  Also, there is an optional size stored in the handle, and a
// cleanup function the GC may call when references to the handle are gone.
//
REBVAL *RL_rebHandle(void *data, size_t length, CLEANUP_CFUNC *cleaner)
 { return Init_Handle_Managed(Alloc_Value(), data, length, cleaner); }


//
//  rebArgR: RL_API
//
// This is the version of getting an argument that does not require a release.
// However, it is more optimal than `rebR(rebArg(...))`, because how it works
// is by returning the actual REBVAL* to the argument in the frame.  It's not
// good to have client code having those as handles--however--as they do not
// follow the normal rules for lifetime, so rebArg() should be used if the
// client really requires a REBVAL*.
//
// !!! When code is being used to look up arguments of a function, exactly
// how that will work is being considered:
//
// https://forum.rebol.info/t/817
// https://forum.rebol.info/t/820
//
// For the moment, this routine specifically accesses arguments of the most
// recent ACTION! on the stack.
//
const void *RL_rebArgR(const void *p, va_list *vaptr)
{
    REBFRM *f = FS_TOP;
    REBACT *act = FRM_PHASE(f);

    // !!! Currently the JavaScript wrappers do not do the right thing for
    // taking just a `const char*`, so this falsely is a variadic to get the
    // JavaScript string proxying.
    //
    const char *name = cast(const char*, p);
    const void *p2 = va_arg(*vaptr, const void*);
    if (Detect_Rebol_Pointer(p2) != DETECTED_AS_END)
        fail ("rebArg() isn't actually variadic, it's arity-1");

    REBSTR *spelling = Intern_UTF8_Managed(
        cb_cast(name),
        LEN_BYTES(cb_cast(name))
    );

    REBVAL *param = ACT_PARAMS_HEAD(act);
    REBVAL *arg = FRM_ARGS_HEAD(f);
    for (; NOT_END(param); ++param, ++arg) {
        if (SAME_STR(VAL_PARAM_SPELLING(param), spelling))
            return arg;
    }

    fail ("Unknown rebArg(...) name.");
}


//
//  rebArg: RL_API
//
// Wrapper over the more optimal rebArgR() call, which can be used to get
// an "safer" API handle to the argument.
//
REBVAL *RL_rebArg(const void *p, va_list *vaptr)
{
    const void* argR = RL_rebArgR(p, vaptr);
    if (not argR)
        return nullptr;

    REBVAL *arg = cast(REBVAL*, c_cast(void*, argR)); // sneaky, but we know!
    return Move_Value(Alloc_Value(), arg); // don't give REBVAL* arg directly
}


//=//// EVALUATIVE EXTRACTORS /////////////////////////////////////////////=//
//
// The libRebol API evaluative routines are all variadic, and call the
// evaluator on multiple pointers.  Each pointer may be:
//
// - a REBVAL*
// - a UTF-8 string to be scanned as one or more values in the sequence
// - a REBSER* that represents an "API instruction"
//
// There isn't a separate concept of routines that perform evaluations and
// ones that extract C fundamental types out of Rebol values.  Hence you
// don't have to say:
//
//      REBVAL *value = rebRun("1 +", some_rebol_integer);
//      int sum = rebUnboxInteger(value);
//      rebRelease(value);
//
// You can just write:
//
//      int sum = rebUnboxInteger("1 +", some_rebol_integer);
//
// The default evaluators splice Rebol values "as-is" into the feed.  This
// means that any evaluator active types (like WORD!, ACTION!, GROUP!...)
// will run.  This can be mitigated with rebQ, but to make it easier for
// some cases variants like `rebRunQ()` and `rebUnboxIntegerQ()` are provided
// which default to splicing with quotes.
//
// (see FLAG_QUOTING_BYTE(1) for why quoting splices is not the default)
//
//=////////////////////////////////////////////////////////////////////////=//

static void Run_Va_May_Fail(
    REBVAL *out,
    REBFLGS feed_flags,  // e.g. FLAG_QUOTING_BYTE(1)
    const void *opt_first,  // optional element to inject *before* the va_list
    va_list *vaptr  // va_end() handled by feed for all cases (throws, fails)
){
    Init_Void(out);

    DECLARE_VA_FEED (feed, opt_first, vaptr, feed_flags);
    if (Do_Feed_To_End_Maybe_Stale_Throws(out, feed)) {
        //
        // !!! Being able to THROW across C stacks is necessary in the general
        // case (consider implementing QUIT or HALT).  Probably need to be
        // converted to a kind of error, and then re-converted into a THROW
        // to bubble up through Rebol stacks?  Development on this is ongoing.
        //
        fail (Error_No_Catch_For_Throw(out));
    }

    CLEAR_CELL_FLAG(out, OUT_MARKED_STALE);
}


//=//// rebRun + rebRunQ //////////////////////////////////////////////////=//
//
// Most basic evaluator that returns a REBVAL*, which must be rebRelease()'d.
//
static REBVAL *rebRun_internal(REBFLGS flags, const void *p, va_list *vaptr)
{
    REBVAL *result = Alloc_Value();
    Run_Va_May_Fail(result, flags, p, vaptr);  // calls va_end()

    if (not IS_NULLED(result))
        return result;  // caller must rebRelease()

    rebRelease(result);
    return nullptr;  // No NULLED cells in API, see notes on NULLIFY_NULLED()
}

//
//  rebRun: RL_API
//
REBVAL *RL_rebRun(const void *p, va_list *vaptr)
  { return rebRun_internal(FEED_MASK_DEFAULT, p, vaptr); }

//
//  rebRunQ: RL_API
//
REBVAL *RL_rebRunQ(const void *p, va_list *vaptr)
 { return rebRun_internal(FLAG_QUOTING_BYTE(1), p, vaptr); }


//=//// rebQuote + rebQuoteQ //////////////////////////////////////////////=//
//
// Variant of rebRun() that simply quotes its result.  So `rebQuote(...)` is
// equivalent to `rebRun("quote", ...)`, with the advantage of being faster
// and not depending on what the QUOTE word looks up to.
//
// (It also has the advantage of not showing QUOTE on the call stack.  That
// is important for the console when trapping its generated result, to be
// able to quote it without the backtrace showing a QUOTE stack frame.)
//
static REBVAL *rebQuote_internal(REBFLGS flags, const void *p, va_list *vaptr)
{
    REBVAL *result = Alloc_Value();
    Run_Va_May_Fail(result, flags, p, vaptr);  // calls va_end()

    return Quotify(result, 1);  // nulled cells legal for API if quoted
}

//
//  rebQuote: RL_API
//
REBVAL *RL_rebQuote(const void *p, va_list *vaptr)
 { return rebQuote_internal(FEED_MASK_DEFAULT, p, vaptr); }

//
//  rebQuoteQ: RL_API
//
REBVAL *RL_rebQuoteQ(const void *p, va_list *vaptr)
 { return rebQuote_internal(FLAG_QUOTING_BYTE(1), p, vaptr); }


//=//// rebElide + rebElideQ //////////////////////////////////////////////=//
//
// Variant of rebRun() which assumes you don't need the result.  This saves on
// allocating an API handle, or the caller needing to manage its lifetime.
//
static void rebElide_internal(REBFLGS flags, const void *p, va_list *vaptr)
{
    DECLARE_LOCAL (elided);
    Run_Va_May_Fail(elided, flags, p, vaptr);  // calls va_end()
}

//
//  rebElide: RL_API
//
void RL_rebElide(const void *p, va_list *vaptr)
 { rebElide_internal(FEED_MASK_DEFAULT, p, vaptr); }


//
//  rebElideQ: RL_API
//
void RL_rebElideQ(const void *p, va_list *vaptr)
 { rebElide_internal(FLAG_QUOTING_BYTE(1), p, vaptr); }


//=//// rebJumps + rebJumpsQ //////////////////////////////////////////////=//
//
// rebJumps() is like rebElide, but has the noreturn attribute.  This helps
// inform the compiler that the routine is not expected to return.  Use it
// with things like `rebJumps("FAIL", ...)` or `rebJumps("THROW", ...)`.  If
// by some chance the code passed to it does not jump and finishes normally,
// then an error will be raised.
//
// (Note: Capitalizing the "FAIL" or other non-returning operation is just a
// suggestion to help emphasize the operation.  Capitalizing rebJUMPS was
// considered, but looked odd.)
//
// !!! The name is not ideal, but other possibilites aren't great:
//
//    rebDeadEnd(...) -- doesn't sound like it should take arguments
//    rebNoReturn(...) -- whose return?
//    rebStop(...) -- STOP is rather final sounding, the code keeps going
//
static void rebJumps_internal(REBFLGS flags, const void *p, va_list *vaptr)
{
    DECLARE_LOCAL (dummy);
    Run_Va_May_Fail(dummy, flags, p, vaptr);  // calls va_end()

    fail ("rebJumps() was used to run code, but it didn't FAIL/QUIT/THROW!");
}

//
//  rebJumps: RL_API [
//      #noreturn
//  ]
//
void RL_rebJumps(const void *p, va_list *vaptr)
 { rebJumps_internal(FEED_MASK_DEFAULT, p, vaptr); }

//
//  rebJumpsQ: RL_API [
//      #noreturn
//  ]
//
void RL_rebJumpsQ(const void *p, va_list *vaptr)
 { rebJumps_internal(FLAG_QUOTING_BYTE(1), p, vaptr); }


//=//// rebDid + rebNot + rebDidQ + rebNotQ ///////////////////////////////=//
//
// Simply returns the logical result, with no returned handle to release.
//
// !!! If this were going to be a macro like (not (rebDid(...))) it would have
// to be a variadic macro.  Just make a separate entry point for now.
//
static bool rebDid_internal(REBFLGS flags, const void *p, va_list *vaptr)
{
    DECLARE_LOCAL (condition);
    Run_Va_May_Fail(condition, flags, p, vaptr);  // calls va_end()

    return IS_TRUTHY(condition);  // will fail() on voids
}

//
//  rebDid: RL_API
//
bool RL_rebDid(const void *p, va_list *vaptr)
 { return rebDid_internal(FEED_MASK_DEFAULT, p, vaptr); }

//
//  rebNot: RL_API
//
bool RL_rebNot(const void *p, va_list *vaptr)
 { return not rebDid_internal(FEED_MASK_DEFAULT, p, vaptr); }

//
//  rebDidQ: RL_API
//
bool RL_rebDidQ(const void *p, va_list *vaptr)
 { return rebDid_internal(FLAG_QUOTING_BYTE(1), p, vaptr); }

//
//  rebNotQ: RL_API
//
bool RL_rebNotQ(const void *p, va_list *vaptr)
 { return not rebDid_internal(FLAG_QUOTING_BYTE(1), p, vaptr); }


//=//// rebUnbox + rebUnboxQ //////////////////////////////////////////////=//
//
// C++, JavaScript, and other languages can do some amount of intelligence
// with a generic `rebUnbox()` operation...either picking the type to return
// based on the target in static typing, or returning a dynamically typed
// value.  For convenience in C, make the generic unbox operation return
// an integer for INTEGER!, LOGIC!, CHAR!...assume it's most common so the
// short name is worth it.
//
static long rebUnbox_internal(REBFLGS flags, const void *p, va_list *vaptr)
{
    DECLARE_LOCAL (result);
    Run_Va_May_Fail(result, flags, p, vaptr);  // calls va_end()

    switch (VAL_TYPE(result)) {
      case REB_INTEGER:
        return VAL_INT64(result);

      case REB_CHAR:
        return VAL_CHAR(result);

      case REB_LOGIC:
        return VAL_LOGIC(result) ? 1 : 0;

      default:
        fail ("C-based rebUnbox() only supports INTEGER!, CHAR!, and LOGIC!");
    }
}

//
//  rebUnbox: RL_API
//
long RL_rebUnbox(const void *p, va_list *vaptr)
 { return rebUnbox_internal(FEED_MASK_DEFAULT, p, vaptr); }


//
//  rebUnboxQ: RL_API
//
long RL_rebUnboxQ(const void *p, va_list *vaptr)
 { return rebUnbox_internal(FLAG_QUOTING_BYTE(1), p, vaptr); }


//=//// rebUnboxInteger + rebUnboxIntegerQ ////////////////////////////////=//
//
static long rebUnboxInteger_internal(REBFLGS flags, const void *p, va_list *vaptr)
{
    DECLARE_LOCAL (result);
    Run_Va_May_Fail(result, flags, p, vaptr);  // calls va_end()

    if (VAL_TYPE(result) != REB_INTEGER)
        fail ("rebUnboxInteger() called on non-INTEGER!");

    return VAL_INT64(result);
}

//
//  rebUnboxInteger: RL_API
//
long RL_rebUnboxInteger(const void *p, va_list *vaptr)
 { return rebUnboxInteger_internal(FEED_MASK_DEFAULT, p, vaptr); }

//
//  rebUnboxIntegerQ: RL_API
//
long RL_rebUnboxIntegerQ(const void *p, va_list *vaptr)
 { return rebUnboxInteger_internal(FLAG_QUOTING_BYTE(1), p, vaptr); }



//=//// rebUnboxDecimal + rebUnboxDecimalQ ////////////////////////////////=//
//
static double rebUnboxDecimal_internal(
    REBFLGS flags,
    const void *p,
    va_list *vaptr
){
    DECLARE_LOCAL (result);
    Run_Va_May_Fail(result, flags, p, vaptr);  // calls va_end()

    if (VAL_TYPE(result) == REB_DECIMAL)
        return VAL_DECIMAL(result);

    if (VAL_TYPE(result) == REB_INTEGER)
        return cast(double, VAL_INT64(result));

    fail ("rebUnboxDecimal() called on non-DECIMAL! or non-INTEGER!");
}

//
//  rebUnboxDecimal: RL_API
//
double RL_rebUnboxDecimal(const void *p, va_list *vaptr)
 { return rebUnboxDecimal_internal(FEED_MASK_DEFAULT, p, vaptr); }

//
//  rebUnboxDecimalQ: RL_API
//
double RL_rebUnboxDecimalQ(const void *p, va_list *vaptr)
 { return rebUnboxDecimal_internal(FLAG_QUOTING_BYTE(1), p, vaptr); }



//=//// rebUnboxChar + rebUnboxCharQ //////////////////////////////////////=//
//
static uint32_t rebUnboxChar_internal(
    REBFLGS flags,
    const void *p,
    va_list *vaptr
){
    DECLARE_LOCAL (result);
    Run_Va_May_Fail(result, flags, p, vaptr);  // calls va_end()

    if (VAL_TYPE(result) != REB_CHAR)
        fail ("rebUnboxChar() called on non-CHAR!");

    return VAL_CHAR(result);
}

//
//  rebUnboxChar: RL_API
//
uint32_t RL_rebUnboxChar(const void *p, va_list *vaptr)
 { return rebUnboxChar_internal(FEED_MASK_DEFAULT, p, vaptr); }

//
//  rebUnboxCharQ: RL_API
//
uint32_t RL_rebUnboxCharQ(const void *p, va_list *vaptr)
 { return rebUnboxChar_internal(FLAG_QUOTING_BYTE(1), p, vaptr); }


//
//  rebSpellIntoQ_internal: RL_API
//
// Extract UTF-8 data from an ANY-STRING! or ANY-WORD!.
//
// API does not return the number of UTF-8 characters for a value, because
// the answer to that is always cached for any value position as LENGTH OF.
// The more immediate quantity of concern to return is the number of bytes.
//
size_t RL_rebSpellIntoQ_internal(
    char *buf,
    size_t buf_size, // number of bytes
    const REBVAL *v,
    const void *end
){
    if (Detect_Rebol_Pointer(end) != DETECTED_AS_END)
        fail ("rebSpellInto() doesn't support more than one value yet");

    const char *utf8;
    REBSIZ utf8_size;
    if (ANY_STRING(v)) {
        REBSIZ offset;
        REBSER *temp = Temp_UTF8_At_Managed(
            &offset, &utf8_size, v, VAL_LEN_AT(v)
        );
        utf8 = cs_cast(BIN_AT(temp, offset));
    }
    else {
        assert(ANY_WORD(v));

        REBSTR *spelling = VAL_WORD_SPELLING(v);
        utf8 = STR_HEAD(spelling);
        utf8_size = STR_SIZE(spelling);
    }

    if (not buf) {
        assert(buf_size == 0);
        return utf8_size; // caller must allocate a buffer of size + 1
    }

    REBSIZ limit = MIN(buf_size, utf8_size);
    memcpy(buf, utf8, limit);
    buf[limit] = '\0';
    return utf8_size;
}


//=//// rebSpell + rebSpellQ //////////////////////////////////////////////=//
//
// This gives the spelling as UTF-8 bytes.  Length in codepoints should be
// extracted with LENGTH OF.  If size in bytes of the encoded UTF-8 is needed,
// use the binary extraction API (works on ANY-STRING! to get UTF-8)
//
static char *rebSpell_internal(REBFLGS flags, const void *p, va_list *vaptr) {
    DECLARE_LOCAL (string);
    Run_Va_May_Fail(string, flags, p, vaptr);  // calls va_end()

    if (IS_NULLED(string))
        return nullptr;  // NULL is passed through, for opting out

    size_t size = rebSpellIntoQ_internal(nullptr, 0, string, rebEND);
    char *result = cast(char*, rebMalloc(size + 1)); // add space for term
    rebSpellIntoQ_internal(result, size, string, rebEND);
    return result;
}

//
//  rebSpell: RL_API
//
char *RL_rebSpell(const void *p, va_list *vaptr)
 { return rebSpell_internal(FEED_MASK_DEFAULT, p, vaptr); }

//
//  rebSpellQ: RL_API
//
char *RL_rebSpellQ(const void *p, va_list *vaptr)
 { return rebSpell_internal(FLAG_QUOTING_BYTE(1), p, vaptr); }


//
//  rebSpellIntoWideQ_internal: RL_API
//
// Extract UCS-2 data from an ANY-STRING! or ANY-WORD!.  Note this is *not*
// UTF-16, so codepoints that require more than two bytes to represent will
// cause errors.
//
// !!! Although the rebSpellInto API deals in bytes, this deals in count of
// characters.  (The use of REBCNT instead of REBSIZ indicates this.)  It may
// be more useful for the wide string APIs to do this so leaving it that way
// for now.
//
unsigned int RL_rebSpellIntoWideQ_internal(
    REBWCHAR *buf,
    unsigned int buf_chars, // chars buf can hold (not including terminator)
    const REBVAL *v,
    const void *end
){
    if (Detect_Rebol_Pointer(end) != DETECTED_AS_END)
        fail ("rebSpellIntoWide() doesn't support more than one value yet");

    REBSER *s;
    REBCNT index;
    REBCNT len;
    if (ANY_STRING(v)) {
        s = VAL_SERIES(v);
        index = VAL_INDEX(v);
        len = VAL_LEN_AT(v);
    }
    else {
        assert(ANY_WORD(v));

        REBSTR *spelling = VAL_WORD_SPELLING(v);
        s = Make_Sized_String_UTF8(STR_HEAD(spelling), STR_SIZE(spelling));
        index = 0;
        len = SER_LEN(s);
    }

    if (not buf) { // querying for size
        assert(buf_chars == 0);
        if (ANY_WORD(v))
            Free_Unmanaged_Series(s);
        return len; // caller must now allocate buffer of len + 1
    }

    REBCNT limit = MIN(buf_chars, len);
    REBCNT n = 0;
    for (; index < limit; ++n, ++index)
        buf[n] = GET_ANY_CHAR(s, index);

    buf[limit] = 0;

    if (ANY_WORD(v))
        Free_Unmanaged_Series(s);
    return len;
}


//=//// rebSpellWide + rebSpellWideQ //////////////////////////////////////=//
//
// Gives the spelling as WCHARs.  If length in codepoints is needed, use
// a separate LENGTH OF call.
//
// !!! Unlike with rebSpell(), there is not an alternative for getting
// the size in UTF-16-encoded characters, just the LENGTH OF result.  While
// that works for UCS-2 (where all codepoints are two bytes), it would not
// work if Rebol supported UTF-16.  Which it may never do in the core or
// API (possible solutions could include usermode UTF-16 conversion to binary,
// and extraction of that with rebBytes(), then dividing the size by 2).
//
static REBWCHAR *rebSpellWide_internal(
    REBFLGS flags,
    const void *p,
    va_list *vaptr
){
    DECLARE_LOCAL (string);
    Run_Va_May_Fail(string, flags, p, vaptr);  // calls va_end()

    if (IS_NULLED(string))
        return nullptr; // NULL is passed through, for opting out

    REBCNT len = rebSpellIntoWideQ_internal(nullptr, 0, string, rebEND);
    REBWCHAR *result = cast(
        REBWCHAR*, rebMalloc(sizeof(REBWCHAR) * (len + 1))
    );
    rebSpellIntoWideQ_internal(result, len, string, rebEND);
    return result;
}

//
//  rebSpellWide: RL_API
//
REBWCHAR *RL_rebSpellWide(const void *p, va_list *vaptr)
 { return rebSpellWide_internal(FEED_MASK_DEFAULT, p, vaptr); }

//
//  rebSpellWideQ: RL_API
//
REBWCHAR *RL_rebSpellWideQ(const void *p, va_list *vaptr)
 { return rebSpellWide_internal(FLAG_QUOTING_BYTE(1), p, vaptr); }


//
//  rebBytesIntoQ_internal: RL_API
//
// Extract binary data from a BINARY!
//
// !!! Caller must allocate a buffer of the returned size + 1.  It's not clear
// if this is a good idea; but this is based on a longstanding convention of
// zero termination of Rebol series, including binaries.  Review.
//
size_t RL_rebBytesIntoQ_internal(
    unsigned char *buf,  // parameters besides p,vaptr throw off wrapper code
    size_t buf_size,
    const REBVAL *binary,
    const void *end
){
    if (Detect_Rebol_Pointer(end) != DETECTED_AS_END)
        fail ("rebBytesInto() doesn't support more than one value yet");

    if (not IS_BINARY(binary))
        fail ("rebBytesInto() only works on BINARY!");

    REBCNT size = VAL_LEN_AT(binary);

    if (not buf) {
        assert(buf_size == 0);
        return size; // currently, caller must allocate a buffer of size + 1
    }

    REBCNT limit = MIN(buf_size, size);
    memcpy(s_cast(buf), cs_cast(VAL_BIN_AT(binary)), limit);
    buf[limit] = '\0';
    return size;
}


//=//// rebBytes + rebBytesQ //////////////////////////////////////////////=//
//
// Can be used to get the bytes of a BINARY! and its size, or the UTF-8
// encoding of an ANY-STRING! or ANY-WORD! and that size in bytes.  (Hence,
// for strings it is like rebSpell() except telling you how many bytes.)
//
// !!! This may wind up being a generic TO BINARY! converter, so you might
// be able to get the byte conversion for any type.
//
unsigned char *rebBytes_internal(
    REBFLGS flags,
    size_t *size_out, // !!! Enforce non-null, to ensure type safety?
    const void *p, va_list *vaptr
){
    DECLARE_LOCAL (series);
    Run_Va_May_Fail(series, flags, p, vaptr);  // calls va_end()

    if (IS_NULLED(series)) {
        *size_out = 0;
        return nullptr; // NULL is passed through, for opting out
    }

    if (ANY_WORD(series) or ANY_STRING(series)) {
        *size_out = rebSpellIntoQ_internal(nullptr, 0, series, rebEND);
        char *result = rebAllocN(char, (*size_out + 1));
        size_t check = rebSpellIntoQ_internal(
            result,
            *size_out,
            series,
        rebEND);
        assert(check == *size_out);
        UNUSED(check);
        return cast(unsigned char*, result);
    }

    if (IS_BINARY(series)) {
        *size_out = rebBytesIntoQ_internal(nullptr, 0, series, rebEND);
        unsigned char *result = rebAllocN(REBYTE, (*size_out + 1));
        size_t check = rebBytesIntoQ_internal(
            result,
            *size_out,
            series,
        rebEND);
        assert(check == *size_out);
        UNUSED(check);
        return result;
    }

    fail ("rebBytes() only works with ANY-STRING!/ANY-WORD!/BINARY!");
}

//
//  rebBytes: RL_API
//
unsigned char *RL_rebBytes(
    size_t *size_out, // !!! Enforce non-null, to ensure type safety?
    const void *p, va_list *vaptr
){
    return rebBytes_internal(FEED_MASK_DEFAULT, size_out, p, vaptr);
}


//
//  rebSteal: RL_API
//
// The difference between rebSteal and rebBytes is that it will actually
// give the memory used by the Rebol series to the user.  This will be done
// in such a way that it can also be rebRepossessed *back* into a BINARY!
// later.  To accomplish this, all dynamic binary series are nominally
// allocated with enough SER_BIAS at their head in order to hold a platform
// pointer, and each reallocation will try to maintain that again.
//
// !!! There is no guarantee about the alignment of the data returned, it
// is an arbitrary byte string.
//
// Sample usage:
//
//     size_t size;
//     REBYTE *bytes;
//
//     bytes = rebSteal(&size, "use [x] [take/part x: make binary! 100 50 x]", rebEND);
//     printf("%s", bytes);
//     fflush(stdout);
//     rebFree(bytes);
//
unsigned char *RL_rebSteal(
    size_t *size_out, // !!! Enforce non-null, to ensure type safety?
    const void *p, va_list *vaptr
){
    Enter_Api();

    DECLARE_LOCAL (series);
    if (Do_Va_Throws(series, p, vaptr)) // calls va_end()
        fail (Error_No_Catch_For_Throw(series));

    if (IS_NULLED(series)) {
        *size_out = 0;
        return nullptr; // NULL is passed through, for opting out
    }

    if (ANY_WORD(series) or ANY_STRING(series))
        fail ("rebSteal() support may be added for ANY-STRING!/ANY_WORD!");

    if (IS_BINARY(series)) {
        REBYTE *result;

        REBSER *bin = VAL_SERIES(series);
        if (not IS_SER_DYNAMIC(bin) or SER_BIAS(bin) < sizeof(REBSER*)) {
            //
            // Small series don't bother keeping a platform-pointer's worth
            // of content at their head, and if there isn't enough space
            // there's nothing that can be done.
            //
            *size_out = rebBytesInto(nullptr, 0, series);
            result = rebAllocN(REBYTE, (*size_out + 1));
            rebBytesInto(result, *size_out, series);

            Decay_Series(bin);
        }
        else {
            //
            // There are enough bytes to poke a series pointer in there, so
            // go ahead and put the pointer in...
            //
            REBSER *thief = Make_Series_Core(
                0, // length 0
                1, // byte-sized
                SERIES_FLAG_FIXED_SIZE
                    | SERIES_FLAG_DONT_RELOCATE
                    // using Make_Series() b/c want it in the manuals list
            );
            assert(not IS_SER_DYNAMIC(thief)); // going to overwrite

            *size_out = BIN_LEN(bin);
            result = SER_DATA_RAW(bin); // pointer before subtracting bias

            SER_SUB_BIAS(bin, sizeof(REBSER*));
            memcpy( // use memcpy as it may not be on pointer aligned boundary
                SER_DATA_RAW(bin), // adjusted head (moved back by bias)
                &thief, // address of the series stealing the data
                sizeof(REBSER*)
            );
            POISON_MEMORY(SER_DATA_RAW(bin), sizeof(REBSER*)); // see rebFree()

            thief->content = bin->content;

            LEN_BYTE_OR_255(bin) = 0; // non-dynamic, 0 size (unnecessary?)
            SET_SER_INFO(bin, SERIES_INFO_INACCESSIBLE);
        }

        return result;
    }

    fail ("rebSteal() only works with BINARY! (at the moment)");
}


//
//  rebBytesQ: RL_API
//
unsigned char *RL_rebBytesQ(
    size_t *size_out, // !!! Enforce non-null, to ensure type safety?
    const void *p, va_list *vaptr
){
    return rebBytes_internal(FLAG_QUOTING_BYTE(1), size_out, p, vaptr);
}


//=//// EXCEPTION HANDLING ////////////////////////////////////////////////=//
//
// There API is approaching exception handling with three different modes.
//
// One is to use setjmp()/longjmp(), which is extremely dodgy.  But it's what
// R3-Alpha used, and it's the only choice if one is sticking to ANSI C89-99:
//
// https://en.wikipedia.org/wiki/Setjmp.h#Exception_handling
//
// If one is willing to compile as C++ -and- link in the necessary support
// for exception handling, there are benefits to doing exception handling
// with throw()/catch().  One advantage is that most compilers can avoid
// paying for catch blocks unless a throw occurs ("zero-cost exceptions"):
//
// https://stackoverflow.com/q/15464891/ (description of the phenomenon)
// https://stackoverflow.com/q/38878999/ (note that it needs linker support)
//
// It also means that C++ API clients can use try/catch blocks without needing
// the rebRescue() abstraction, as well as have destructors run safely.
// (longjmp pulls the rug out from under execution, and doesn't stack unwind).
//
// The third exceptionmode is for JavaScript, where an emscripten build would
// have to painstakingly emulate setjmp/longjmp.  Using inline JavaScript to
// catch and throw is more efficient, and also provides the benefit of API
// clients being able to use normal try/catch of a RebolError instead of
// having to go through rebRescue().
//
// !!! Currently only the setjmp()/longjmp() form is emulated.  Clients must
// either explicitly TRAP errors within their Rebol code calls, or use the
// rebRescue() abstraction to catch the setjmp/longjmp failures.  Rebol
// THROW and CATCH cannot be thrown across an API call barrier--it will be
// handled as an uncaught throw and raised as an error.
//
//=////////////////////////////////////////////////////////////////////////=//

//
//  rebRescue: RL_API
//
// This API abstracts the mechanics by which exception-handling is done.
//
// Using rebRescue() internally to the core allows it to be compiled and run
// compatibly regardless of what .  It is named after Ruby's operation,
// which deals with the identical problem:
//
// http://silverhammermba.github.io/emberb/c/#rescue
//
// !!! As a first step, this only implements the setjmp/longjmp logic.
//
REBVAL *RL_rebRescue(
    REBDNG *dangerous, // !!! pure C function only if not using throw/catch!
    void *opaque
){
    struct Reb_State state;
    REBCTX *error_ctx;

    PUSH_TRAP(&error_ctx, &state);

    // We want allocations that occur in the body of the C function for the
    // rebRescue() to be automatically cleaned up in the case of an error.
    //
    // !!! This is currently done by knowing what frame an error occurred in
    // and marking any allocations while that frame was in effect as being
    // okay to "leak" (in the sense of leaking to be GC'd).  So we have to
    // make a dummy frame here, and unfortunately the frame must be reified
    // so it has to be an "action frame".  Improve mechanic later, but for
    // now pretend to be applying a dummy native.
    //
    DECLARE_END_FRAME (f, EVAL_MASK_DEFAULT);  // not FULLY_SPECIALIZED
    Push_Frame(nullptr, f);

    REBSTR *opt_label = NULL;

    Push_Action(f, PG_Dummy_Action, UNBOUND);
    Begin_Action(f, opt_label);
    assert(IS_END(f->arg));
    f->param = END_NODE; // signal all arguments gathered
    assert(f->refine == ORDINARY_ARG); // Begin_Action() sets
    f->arg = m_cast(REBVAL*, END_NODE);
    f->special = END_NODE;

    // The first time through the following code 'error' will be null, but...
    // `fail` can longjmp here, so 'error' won't be null *if* that happens!
    //
    if (error_ctx) {
        assert(f->varlist); // action must be running
        REBARR *stub = f->varlist; // will be stubbed, with info bits reset
        Drop_Action(f);
        SET_SERIES_FLAG(stub, VARLIST_FRAME_FAILED); // signal API leaks ok
        Abort_Frame(f);
        return Init_Error(Alloc_Value(), error_ctx);
    }

    REBVAL *result = (*dangerous)(opaque);

    Drop_Action(f);

    // !!! To abstract how the system deals with exception handling, the
    // rebRescue() routine started being used in lieu of PUSH_TRAP/DROP_TRAP
    // internally to the system.  Some of these system routines accumulate
    // stack state, so Drop_Frame_Unbalanced() must be used.
    //
    Drop_Frame_Unbalanced(f);

    DROP_TRAP_SAME_STACKLEVEL_AS_PUSH(&state);

    if (not result)
        return nullptr; // null is considered a legal result

    // Analogous to how TRAP works, if you don't have a handler for the
    // error case then you can't return an ERROR!, since all errors indicate
    // a failure.  Use KIND_BYTE() since R_THROWN or other special things can
    // be used internally, and literal errors don't count either.
    //
    if (KIND_BYTE(result) == REB_ERROR) {
        if (Is_Api_Value(result))
            rebRelease(result);
        return rebVoid();
    }

    if (not Is_Api_Value(result))
        return result; // no proxying needed

    assert(not IS_NULLED(result)); // leaked API nulled cell (not nullptr)

    // !!! We automatically proxy the ownership of any managed handles to the
    // caller.  Any other handles that leak out (e.g. via state) will not be
    // covered by this, and would have to be unmanaged.  Do another allocation
    // just for the sake of it.

    REBVAL *proxy = Move_Value(Alloc_Value(), result); // parent is not f
    rebRelease(result);
    return proxy;
}


//
//  rebRescueWith: RL_API
//
// Variant of rebRescue() with a handler hook (parallels TRAP/WITH, except
// for C code as the protected code and the handler).  More similar to
// Ruby's rescue2 operation.
//
REBVAL *RL_rebRescueWith(
    REBDNG *dangerous, // !!! pure C function only if not using throw/catch!
    REBRSC *rescuer, // errors in the rescuer function will *not* be caught
    void *opaque
){
    struct Reb_State state;
    REBCTX *error_ctx;

    PUSH_TRAP(&error_ctx, &state);

    // The first time through the following code 'error' will be null, but...
    // `fail` can longjmp here, so 'error' won't be null *if* that happens!
    //
    if (error_ctx) {
        REBVAL *error = Init_Error(Alloc_Value(), error_ctx);

        REBVAL *result = (*rescuer)(error, opaque); // *not* guarded by trap!

        rebRelease(error);
        return result; // no special handling, may be null
    }

    REBVAL *result = (*dangerous)(opaque); // guarded by trap
    assert(not IS_NULLED(result)); // nulled cells not exposed by API

    DROP_TRAP_SAME_STACKLEVEL_AS_PUSH(&state);

    return result; // no special handling, may be NULL
}


//
//  rebHalt: RL_API
//
// Signal that code evaluation needs to be interrupted.
//
// Returns:
//     nothing
// Notes:
//     This function sets a signal that is checked during evaluation
//     and will cause the interpreter to begin processing an escape
//     trap. Note that control must be passed back to REBOL for the
//     signal to be recognized and handled.
//
void RL_rebHalt(void)
{
    SET_SIGNAL(SIG_HALT);
}



//=//// API "INSTRUCTIONS" ////////////////////////////////////////////////=//
//
// The evaluator API takes further advantage of Detect_Rebol_Pointer() when
// processing variadic arguments to do things more efficiently.
//
// All instructions must be handed *directly* to an evaluator feed.  That
// feed is what guarantees that if a GC occurs that the variadic will be
// spooled forward and their contents guarded.
//
// NOTE THIS IS NOT LEGAL:
//
//     void *instruction = rebQ("stuff");  // not passed direct to evaluator
//     rebElide("print {Hi!}");  // a RECYCLE could be triggered here
//     rebRun(..., instruction, ...);  // the instruction may be corrupt now!
//
//=////////////////////////////////////////////////////////////////////////=//


// The rebQ instruction is designed to work so that `rebRun(rebQ(...))` would
// be the same as rebRunQ(...).  Hence it doesn't mean "quote", it means
// "quote any value splices in this section".  And if you turned around and
// said `rebRun(rebQ(rebU(...)))` that should undo your effect.  The two
// operations share a mostly common implementation.
//
// Note that `rebRun("print {One}", rebQ("print {Two}", ...), ...)` should not
// execute rebQ()'s code right when C runs it.  If it did, then `Two` would
// print before `One`.  It has to give back something that provides more than
// one value when the feed visits it.
//
// So what these operations produce is an array.  If it quotes a single value
// then it will just be a singular array (sizeof(REBSER)).  This array is not
// managed by the GC directly--which means it's cheap to allocate and then
// free as the feed passes it by.  which is one of the reasons that a GC has to
// force reification of outstanding variadic feeds)
//
// We lie and say the array is NODE_FLAG_MANAGED when we create it so it
// won't get manuals tracked.  Then clear the managed flag.  If the GC kicks
// in it will spool the va_list() to the end first and take care of it.  If
// it does not kick in, then the array will just be freed as it's passed.
//
// !!! It may be possible to create variations of this which are done in a
// way that would allow arbitrary spans, `rebU("[, value1), value2, "]"`.
// But those variants would have to be more sophisticated than this.
//
// !!! Formative discussion: https://forum.rebol.info/t/1050
//
static const void *rebSpliceQuoteAdjuster_internal(
    int delta,  // -1 to remove quote from splices, +1 to add quote to splices
    const void *p,
    va_list *vaptr
){
    REBDSP dsp_orig = DSP;

    REBARR *a;

    // In the general case, we need the feed, and all the magic it does for
    // deciphering its arguments (like UTF-8 strings).  But a common case is
    // just calling rebQ(value) to get a quote on a single value.  Sense
    // that situation and make it faster.
    //
    if (Detect_Rebol_Pointer(p) == DETECTED_AS_CELL) {
        const REBVAL *first = VAL(p);  // save pointer
        p = va_arg(*vaptr, const void*);  // advance next pointer (fast!)
        if (Detect_Rebol_Pointer(p) == DETECTED_AS_END) {
            a = Alloc_Singular(NODE_FLAG_MANAGED);
            CLEAR_SERIES_FLAG(a, MANAGED);  // see notes above on why we lied
            Move_Value(ARR_SINGLE(a), first);
        }
        else {
            Move_Value(DS_PUSH(), first);  // no shortcut, push and keep going
            goto no_shortcut;
        }
    }
    else {
      no_shortcut: ;

        REBFLGS feed_flags = FEED_MASK_DEFAULT;  // just get plain values
        DECLARE_VA_FEED (feed, p, vaptr, feed_flags);

        while (NOT_END(feed->value)) {
            Move_Value(DS_PUSH(), KNOWN(feed->value));
            Fetch_Next_In_Feed(feed, false);
        }

        a = Pop_Stack_Values_Core(dsp_orig, NODE_FLAG_MANAGED);
        CLEAR_SERIES_FLAG(a, MANAGED);  // see notes above on why we lied
    }

    // !!! Although you can do `rebU("[", a, b, "]"), you cannot do
    // `rebU(a, b)` at this time.  That's because the feed does not have a
    // way of holding a position inside of a nested array.  The only thing
    // it could do would be to reify the feed into an array--which it can
    // do, but the feature should be thought through more.
    //
    if (ARR_LEN(a) > 1)
        fail ("rebU() and rebQ() currently can't splice more than one value");

    SET_ARRAY_FLAG(a, INSTRUCTION_ADJUST_QUOTING);
    MISC(a).quoting_delta = delta;
    return a;
}

//
//  rebQUOTING: RL_API
//
// This is #defined as rebQ, with C89 shortcut rebQ1 => rebQ(v, rebEND)
//
const void *RL_rebQUOTING(const void *p, va_list *vaptr)
 { return rebSpliceQuoteAdjuster_internal(+1, p, vaptr); }

//
//  rebUNQUOTING: RL_API
//
// This is #defined as rebU, with C89 shortcut rebU1 => rebU(v, rebEND)
//
const void *RL_rebUNQUOTING(const void *p, va_list *vaptr)
 { return rebSpliceQuoteAdjuster_internal(-1, p, vaptr); }


//
//  rebRELEASING: RL_API
//
// Convenience tool for making "auto-release" form of values.  They will only
// exist for one API call.  They will be automatically rebRelease()'d when
// they are seen (or even if they are not seen, if there is a failure on that
// call it will still process the va_list in order to release these handles)
//
const void *RL_rebRELEASING(REBVAL *v)
{
    if (not Is_Api_Value(v))
        fail ("Cannot apply rebR() to non-API value");

    REBARR *a = Singular_From_Cell(v);
    if (GET_ARRAY_FLAG(a, SINGULAR_API_RELEASE))
        fail ("Cannot apply rebR() more than once to the same API value");

    SET_ARRAY_FLAG(a, SINGULAR_API_RELEASE);
    return a; // returned as const void* to discourage use outside variadics
}


//
//  rebManage: RL_API
//
// The "friendliest" default for the API is to assume you want handles to be
// tied to the lifetime of the frame they're in.  Long-running top-level
// processes like the C code running the console would eventually exhaust
// memory if that were the case...so there should be some options for metrics
// as a form of "leak detection" even so.
//
REBVAL *RL_rebManage(REBVAL *v)
{
    assert(Is_Api_Value(v));

    REBARR *a = Singular_From_Cell(v);
    assert(GET_SERIES_FLAG(a, ROOT));

    if (GET_SERIES_FLAG(a, MANAGED))
        fail ("Attempt to rebManage() a handle that's already managed.");

    SET_SERIES_FLAG(a, MANAGED);
    assert(not LINK(a).owner);
    LINK(a).owner = NOD(Context_For_Frame_May_Manage(FS_TOP));

    return v;
}


//
//  rebUnmanage: RL_API
//
// This converts an API handle value to indefinite lifetime.
//
void RL_rebUnmanage(void *p)
{
    REBNOD *nod = NOD(p);
    if (not (nod->header.bits & NODE_FLAG_CELL))
        fail ("rebUnmanage() not yet implemented for rebMalloc() data");

    REBVAL *v = cast(REBVAL*, nod);
    assert(Is_Api_Value(v));

    REBARR *a = Singular_From_Cell(v);
    assert(GET_SERIES_FLAG(a, ROOT));

    if (NOT_SERIES_FLAG(a, MANAGED))
        fail ("Attempt to rebUnmanage() a handle with indefinite lifetime.");

    // It's not safe to convert the average series that might be referred to
    // from managed to unmanaged, because you don't know how many references
    // might be in cells.  But the singular array holding API handles has
    // pointers to its cell being held by client C code only.  It's at their
    // own risk to do this, and not use those pointers after a free.
    //
    CLEAR_SERIES_FLAG(a, MANAGED);
    assert(GET_ARRAY_FLAG(LINK(a).owner, IS_VARLIST));
    LINK(a).owner = UNBOUND;
}


//
//  rebRelease: RL_API
//
// An API handle is only 4 platform pointers in size (plus some bookkeeping),
// but it still takes up some storage.  The intended default for API handles
// is that they live as long as the function frame they belong to, but there
// will be several lifetime management tricks to ease releasing them.
//
// !!! For the time being, we lean heavily on explicit release.  Near term
// leak avoidance will need to at least allow for GC of handles across errors
// for their associated frames.
//
void RL_rebRelease(const REBVAL *v)
{
    if (not v)
        return; // less rigorous, but makes life easier for C programmers

    if (not Is_Api_Value(v))
        panic ("Attempt to rebRelease() a non-API handle");

    Free_Value(m_cast(REBVAL*, v));
}


//
//  rebDeflateAlloc: RL_API
//
// Exposure of the deflate() of the built-in zlib.  Assumes no envelope.
//
// Uses zlib's recommended default for compression level.
//
// See rebRepossess() for the ability to mutate the result into a BINARY!
//
void *RL_rebDeflateAlloc(
    size_t *out_len,
    const void *input,
    size_t in_len
){
    REBSTR *envelope = Canon(SYM_NONE);
    return Compress_Alloc_Core(out_len, input, in_len, envelope);
}


//
//  rebZdeflateAlloc: RL_API
//
// Variant of rebDeflateAlloc() which adds a zlib envelope...which is a 2-byte
// header and 32-bit ADLER32 CRC at the tail.
//
void *RL_rebZdeflateAlloc(
    size_t *out_len,
    const void *input,
    size_t in_len
){
    REBSTR *envelope = Canon(SYM_ZLIB);
    return Compress_Alloc_Core(out_len, input, in_len, envelope);
}


//
//  rebGzipAlloc: RL_API
//
// Slight variant of deflate() which stores the uncompressed data's size
// implicitly in the returned data, and a CRC32 checksum.
//
void *RL_rebGzipAlloc(
    size_t *out_len,
    const void *input,
    size_t in_len
){
    REBSTR *envelope = nullptr; // see notes in Gunzip on why GZIP is default
    return Compress_Alloc_Core(out_len, input, in_len, envelope);
}


//
//  rebInflateAlloc: RL_API
//
// Exposure of the inflate() of the built-in zlib.  Assumes no envelope.
//
// Use max = -1 to guess decompressed size, or for best memory efficiency,
// specify `max` as the precise size of the original data.
//
// See rebRepossess() for the ability to mutate the result into a BINARY!
//
void *RL_rebInflateAlloc(
    size_t *len_out,
    const void *input,
    size_t len_in,
    int max
){
    REBSTR *envelope = Canon(SYM_NONE);
    return Decompress_Alloc_Core(len_out, input, len_in, max, envelope);
}


//
//  rebZinflateAlloc: RL_API
//
// Variant of rebInflateAlloc() which assumes a zlib envelope...checking for
// the 2-byte header and verifying the 32-bit ADLER32 CRC at the tail.
//
void *RL_rebZinflateAlloc(
    size_t *len_out,
    const void *input,
    size_t len_in,
    int max
){
    REBSTR *envelope = Canon(SYM_ZLIB);
    return Decompress_Alloc_Core(len_out, input, len_in, max, envelope);
}


//
//  rebGunzipAlloc: RL_API
//
// Slight variant of inflate() which is compatible with gzip, and checks its
// CRC32.  For data whose original size was < 2^32 bytes, the gzip envelope
// stored that size...so memory efficiency is achieved even if max = -1.
//
// Note: That size guarantee exists for data compressed with rebGzipAlloc() or
// adhering to the gzip standard.  However, archives created with the GNU
// gzip tool make streams with possible trailing zeros or concatenations:
//
// http://stackoverflow.com/a/9213826
//
void *RL_rebGunzipAlloc(
    size_t *len_out,
    const void *input,
    size_t len_in,
    int max
){
    // Note: Because GZIP is what Rebol uses for booting, `nullptr` means
    // use GZIP.  That's because symbols in %words.r haven't been loaded yet,
    // so a call to Canon(SYM_XXX) would fail.
    //
    REBSTR *envelope = nullptr; // GZIP is the default
    return Decompress_Alloc_Core(len_out, input, len_in, max, envelope);
}


//
//  rebDeflateDetectAlloc: RL_API
//
// Does DEFLATE with detection, and also ignores the size information in a
// gzip file, due to the reasoning here:
//
// http://stackoverflow.com/a/9213826
//
void *RL_rebDeflateDetectAlloc(
    size_t *len_out,
    const void *input,
    size_t len_in,
    int max
){
    REBSTR *envelope = Canon(SYM_DETECT);
    return Decompress_Alloc_Core(len_out, input, len_in, max, envelope);
}


// !!! Although it is very much the goal to get all OS-specific code out of
// the core (including the API), this particular hook is extremely useful to
// have available to all clients.  It might be done another way (e.g. by
// having hosts HIJACK the FAIL native with an adaptation that processes
// integer arguments).  But for now, stick it in the API just to get the
// wide availability.
//
#ifdef TO_WINDOWS
    #undef IS_ERROR // windows has its own meaning for this.
    #include <windows.h>
#else
    #include <errno.h>
    #define MAX_POSIX_ERROR_LEN 1024
#endif

//
//  rebFail_OS: RL_API [
//      #noreturn
//  ]
//
// Produce an error from an OS error code, by asking the OS for textual
// information it knows internally from its database of error strings.
//
// This function is called via a macro which adds DEAD_END; after it.
//
// Note that error codes coming from WSAGetLastError are the same as codes
// coming from GetLastError in 32-bit and above Windows:
//
// https://stackoverflow.com/q/15586224/
//
// !!! Should not be in core, but extensions need a way to trigger the
// common functionality one way or another.
//
void RL_rebFail_OS(int errnum)
{
    REBCTX *error;

  #ifdef TO_WINDOWS
    if (errnum == 0)
        errnum = GetLastError();

    WCHAR *lpMsgBuf; // FormatMessage writes allocated buffer address here

    // Specific errors have %1 %2 slots, and if you know the error ID and
    // that it's one of those then this lets you pass arguments to fill
    // those in.  But since this is a generic error, we have no more
    // parameterization (hence FORMAT_MESSAGE_IGNORE_INSERTS)
    //
    va_list *Arguments = nullptr;

    // Apparently FormatMessage can find its error strings in a variety of
    // DLLs, but we don't have any context here so just use the default.
    //
    LPCVOID lpSource = nullptr;

    DWORD ok = FormatMessage(
        FORMAT_MESSAGE_ALLOCATE_BUFFER // see lpMsgBuf
            | FORMAT_MESSAGE_FROM_SYSTEM // e.g. ignore lpSource
            | FORMAT_MESSAGE_IGNORE_INSERTS, // see Arguments
        lpSource,
        errnum, // message identifier
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // default language
        cast(WCHAR*, &lpMsgBuf), // allocated buffer address written here
        0, // buffer size (not used since FORMAT_MESSAGE_ALLOCATE_BUFFER)
        Arguments
    );

    if (ok == 0) {
        //
        // Might want to show the value of GetLastError() in this message,
        // but trying to FormatMessage() on *that* would be excessive.
        //
        error = Error_User("FormatMessage() gave no error description");
    }
    else {
        REBVAL *message = rebTextWide(lpMsgBuf);
        LocalFree(lpMsgBuf);

        error = Error(SYM_0, SYM_0, message, END_NODE);
    }
  #else
    // strerror() is not thread-safe, but strerror_r is. Unfortunately, at
    // least in glibc, there are two different protocols for strerror_r(),
    // depending on whether you are using the POSIX-compliant implementation
    // or the GNU implementation.
    //
    // The convoluted test below is the inversion of the actual test glibc
    // suggests to discern the version of strerror_r() provided. As other,
    // non-glibc implementations (such as OS X's libSystem) also provide the
    // POSIX-compliant version, we invert the test: explicitly use the
    // older GNU implementation when we are sure about it, and use the
    // more modern POSIX-compliant version otherwise. Finally, we only
    // attempt this feature detection when using glibc (__GNU_LIBRARY__),
    // as this particular combination of the (more widely standardised)
    // _POSIX_C_SOURCE and _XOPEN_SOURCE defines might mean something
    // completely different on non-glibc implementations.
    //
    // (Note that undefined pre-processor names arithmetically compare as 0,
    // which is used in the original glibc test; we are more explicit.)

    #ifdef USE_STRERROR_NOT_STRERROR_R
        char *shared = strerror(errnum);
        error = Error_User(shared);
    #elif defined(__GNU_LIBRARY__) \
            && (defined(_GNU_SOURCE) \
                || ((!defined(_POSIX_C_SOURCE) || _POSIX_C_SOURCE < 200112L) \
                    && (!defined(_XOPEN_SOURCE) || _XOPEN_SOURCE < 600)))

        // May return an immutable string instead of filling the buffer

        char buffer[MAX_POSIX_ERROR_LEN];
        char *maybe_str = strerror_r(errnum, buffer, MAX_POSIX_ERROR_LEN);
        if (maybe_str != buffer)
            strncpy(buffer, maybe_str, MAX_POSIX_ERROR_LEN);
        error = Error_User(buffer);
    #else
        // Quoting glibc's strerror_r manpage: "The XSI-compliant strerror_r()
        // function returns 0 on success. On error, a (positive) error number
        // is returned (since glibc 2.13), or -1 is returned and errno is set
        // to indicate the error (glibc versions before 2.13)."

        char buffer[MAX_POSIX_ERROR_LEN];
        int result = strerror_r(errnum, buffer, MAX_POSIX_ERROR_LEN);

        // Alert us to any problems in a debug build.
        assert(result == 0);

        if (result == 0)
            error = Error_User(buffer);
        else if (result == EINVAL)
            error = Error_User("EINVAL: bad errno passed to strerror_r()");
        else if (result == ERANGE)
            error = Error_User("ERANGE: insufficient buffer size for error");
        else
            error = Error_User("Unknown problem with strerror_r() message");
    #endif
  #endif

    DECLARE_LOCAL (temp);
    Init_Error(temp, error);
    rebJumps("fail", temp, rebEND);
}


//=//// TRANSITIONAL TOOLS FOR REBREQ/DEVICE MIGRATION ////////////////////=//
//
// !!! To do I/O, R3-Alpha had the concept of "simple" devices, which would
// represent abstractions of system services (Dev_Net would abstract the
// network layer, Dev_File the filesystem, etc.)
//
// There were a fixed list of commands these devices would handle (OPEN,
// CONNECT, READ, WRITE, CLOSE, QUERY).  Further parameterization was done
// with the fields of a specialized C structure called a REBREQ.
//
// This layer was code solely used by Rebol, and needed access to data
// resident in Rebol types.  For instance: if one is to ask to read from a
// file, it makes sense to use Rebol's FILE!.  And if one is reading into an
// existing BINARY! buffer, it makes sense to give the layer the BINARY!.
// But there was an uneasy situation of saying that these REBREQ could not
// speak in Rebol types, resulting in things like picking pointers out of
// the guts of Rebol cells and invoking unknown interactions with the GC by
// putting them into a C struct.
//
// Ren-C is shifting the idea to where a REBREQ is actually a REBARR, and
// able to hold full values (for starters, a REBSER* containing binary data
// of what used to be in a REBREQ...which is actually how PORT!s held a
// REBREQ in their state previously).  However, the way the device layer was
// written it does not have access to the core API.
//


//
//  rebMake_Rebreq: RL_API
//
// !!! Another transitional tool.
//
REBREQ *RL_rebMake_Rebreq(int device) {
    assert(device < RDI_MAX);

    REBDEV *dev = Devices[device];
    assert(dev != NULL);

    REBREQ *req = Make_Binary(dev->req_size);
    memset(BIN_HEAD(req), 0, dev->req_size);
    TERM_BIN_LEN(req, dev->req_size);

    SET_SERIES_INFO(req, LINK_IS_CUSTOM_NODE);
    LINK(req).custom.node = nullptr;

    SET_SERIES_INFO(req, MISC_IS_CUSTOM_NODE);
    MISC(req).custom.node = nullptr;

    Req(req)->device = device;

    return req;
}


#if defined(NDEBUG)
    #define ASSERT_REBREQ(req) \
        NOOP
#else
    inline static void ASSERT_REBREQ(REBREQ *req) {  // basic sanity check
        assert(BYTE_SIZE(req) and BIN_LEN(req) >= sizeof(struct rebol_devreq));
        assert(GET_SERIES_INFO(req, LINK_IS_CUSTOM_NODE));
        assert(GET_SERIES_INFO(req, MISC_IS_CUSTOM_NODE));
    }
#endif


//
//  rebReq: RL_API
//
// !!! Transitional - extract content pointer for REBREQ
//
void *RL_rebReq(REBREQ *req) {
    ASSERT_REBREQ(req);
    return BIN_HEAD(req);  // Req() casts this to `struct rebol_devreq*`
}


//
//  rebAddrOfNextReq: RL_API
//
// !!! Transitional - get `next_req` field hidden in REBSER structure LINK().
// Being in this spot (instead of inside the binary content of the request)
// means the chain of requests can be followed by GC.
//
void **RL_rebAddrOfNextReq(REBREQ *req) {
    ASSERT_REBREQ(req);
    return (void**)&LINK(req).custom.node;  // NextReq() dereferences
}


//
//  rebAddrOfReqPortCtx: RL_API
//
// !!! Transitional - get `port_ctx` field hidden in REBSER structure MISC().
// Being in this spot (instead of inside the binary content of the request)
// means the chain of requests can be followed by GC.
//
void **RL_rebAddrOfReqPortCtx(REBREQ *req) {
    ASSERT_REBREQ(req);
    return (void**)&MISC(req).custom.node;  // ReqPortCtx() dereferences
}


//
//  rebEnsure_Req_Managed: RL_API
//
// !!! Transitional - Lifetime management of REBREQ in R3-Alpha was somewhat
// unclear, with them being created sometimes on the stack, and sometimes
// linked into a pending list if a request turned out to be synchronous and
// not need the request to live longer.  To try and design for efficiency,
// Append_Request() currently is the only place that manages the request for
// asynchronous handling...other clients are expected to free.
//
// !!! Some requests get Append_Request()'d multiple times, apparently.
// Review the implications, but just going with making it legal to manage
// something multiple times for now.
//
void RL_rebEnsure_Req_Managed(REBREQ *req) {
    ASSERT_REBREQ(req);
    ENSURE_SERIES_MANAGED(req);
}


//
//  rebFree_Req: RL_API
//
// !!! Another transitional tool - see notes on rebManageReq()
//
void RL_rebFree_Req(REBREQ *req) {
    ASSERT_REBREQ(req);
    Free_Unmanaged_Series(req);
}


// We wish to define a table of the above functions to pass to clients.  To
// save on typing, the declaration of the table is autogenerated as a file we
// can include here.
//
// It doesn't make a lot of sense to expose this table to clients via an API
// that returns it, because that's a chicken-and-the-egg problem.  The reason
// a table is being used in the first place is because extensions can't link
// to an EXE (in a generic way).  So the table is passed to them, in that
// extension's DLL initialization function.
//
// !!! Note: if Rebol is built as a DLL or LIB, the story is different.
//
extern RL_LIB Ext_Lib;
#include "tmp-reb-lib-table.inc" // declares Ext_Lib
