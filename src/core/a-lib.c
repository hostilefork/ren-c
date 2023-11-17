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
// Copyright 2012-2021 Ren-C Open Source Contributors
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
// This is the "external" API, and %rebol.h contains its exported
// definitions.  That file (and %make-reb-lib.r which generates it) contains
// comments and notes which will help understand it.
//
// What characterizes the external API is that it is not necessary to #include
// the extensive definitions of `struct SeriesT` or the APIs for dealing with
// all the internal details (e.g. PUSH_GC_GUARD(), which are easy to get
// wrong).  Not only does this simplify the interface, but it also means that
// the C code using the library isn't competing as much for definitions in
// the global namespace.
//
// Also, due to the nature of the Node superclass (see %sys-node.h), it's
// possible to feed the scanner with a list of pointers that may be to UTF-8
// strings or to Rebol values.  The behavior is to "splice" in the values at
// the point in the scan that they occur, e.g.
//
//     REBVAL *item1 = ...;
//     REBVAL *item2 = ...;
//     REBVAL *item3 = ...;
//
//     REBVAL *result = rebValue(
//         "if not", item1, "[\n",
//             item2, "| print {Close brace separate from content}\n",
//         "] else [\n",
//             item3, "| print {Close brace with content}]\n",
//         rebEND  // optional in C99 and C++11
//     );
//
// (Note: rebEND is needed by the variadic processing, but C99-based macros or
// other language bindings can inject it automatically...only C89 has no way
// to work around it.)
//
// While the approach is flexible, any token must appear fully inside its
// UTF-8 string component.  So you can't--for instance--divide a scan up like
// ("{abc", "def", "ghi}") and get the TEXT! {abcdefghi}.  On that note,
// ("a", "/", "b") produces `a / b` and not the PATH! `a/b`.
//
//==//// EXPORT NOTES /////////////////////////////////////////////////////=//
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

#define REBOL_LEVEL_SHORTHAND_MACROS 0  // we include Windows.h for errors
#include "sys-core.h"

static bool PG_Api_Initialized = false;


// The API tolerates internal cells that are Is_Nulled(), but all handles that
// are Is_Api_Value() mustn't be nulled.  nullptr is the only currency exposed
// to the clients of the API for NULL.
//
inline static const REBVAL *NULLIFY_NULLED(const REBVAL *cell) {
  return Is_Nulled(cell)
      ? cast(REBVAL*, nullptr)  // C++98 ambiguous w/o cast
      : cell;
}

//
// ENTER_API macro
//
// For a time, this was done by the wrapping code...so that the APIs here
// would not have to remember to do it.  That made the header file look
// longer, and added function call overhead where it might not be needed.
// Given that the number of APIs is being kept somewhat limited, the macro
// is just included manually.
//
// !!! Review how much checking one wants to do when calling API routines,
// and what the balance should be of debug vs. release.  Right now, this helps
// in particular notice if the core tries to use an API function before the
// proper moment in the boot.
//
#define ENTER_API \
    if (not PG_Api_Initialized) \
        panic ("rebStartup() not called before API call");


//=//// SERIES-BACKED ALLOCATORS //////////////////////////////////////////=//
//
// These are replacements for malloc(), realloc(), and free() which use a
// byte-sized Series as the backing store for the data.
//
// One benefit of using a series is that it offers more options for automatic
// memory management (such as being freed in case of a fail(), vs. leaked as
// a malloc() would, or perhaps being GC'd when a particular FRAME! ends).
//
// It also has the benefit of helping interface with client code that has
// been stylized to use malloc()-ish hooks to produce data, when the eventual
// target of that data is a Rebol series.  It does this without exposing
// Series(*) internals to the external API, by allowing one to "rebRepossess()"
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
// * In order to make it possible to rebRepossess() the memory as a BINARY!
//   that is then safe to alias as text, it always has an extra 0 byte at
//   the end of the data area.
//
// * It tries to be like malloc() by giving back a pointer "suitably aligned
//   for the size of any fundamental type".  See notes on ALIGN_SIZE.
//
// !!! rebAlignedMalloc() could exist to take an alignment, which could save
// on wasted bytes when ALIGN_SIZE > sizeof(Series(*))...or work with "weird"
// large fundamental types that need more alignment than ALIGN_SIZE.
//
void *RL_rebMalloc(size_t size)
{
    ENTER_API;

    Binary(*) s = Make_Series(BinaryT,
        ALIGN_SIZE  // stores Series(*) (must be at least big enough for void*)
            + size  // for the actual data capacity (may be 0, see notes)
            + 1,  // for termination (AS TEXT! of rebRepossess(), see notes)
        FLAG_FLAVOR(BINARY)  // rebRepossess() only creates binary series ATM
            | SERIES_FLAG_DONT_RELOCATE  // direct data pointer handed back
            | SERIES_FLAG_DYNAMIC  // rebRepossess() needs bias field
    );

    Byte* ptr = Binary_Head(s) + ALIGN_SIZE;

    Series(*) *ps = (cast(Series(*)*, ptr) - 1);
    *ps = s;  // save self in bytes that appear immediately before the data
    Poison_Memory_If_Sanitize(ps, sizeof(Series(*)));  // catch underruns

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
    Term_Binary_Len(s, ALIGN_SIZE + size);

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
    ENTER_API;

    assert(new_size > 0);  // realloc() deprecated this as of C11 DR 400

    if (not ptr)  // C realloc() accepts null
        return rebMalloc(new_size);

    Binary(*) *ps = cast(Binary(*)*, ptr) - 1;
    Unpoison_Memory_If_Sanitize(ps, sizeof(Binary(*)));  // getting s underruns

    Binary(*) s = *ps;

    REBLEN old_size = Binary_Len(s) - ALIGN_SIZE;

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
    ENTER_API;

    if (not ptr)
        return;

    Binary(*) *ps = cast(Binary(*)*, ptr) - 1;
    Unpoison_Memory_If_Sanitize(ps, sizeof(Binary(*)));  // need to underrun to fetch `s`

    Binary(*) s = *ps;
    if (Is_Node_A_Cell(s)) {
        rebJumps(
            "panic [",
                "{rebFree() mismatched with allocator!}"
                "{Did you mean to use free() instead of rebFree()?}",
            "]"
        );
    }

    assert(Series_Wide(s) == 1);

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
// !!! It might seem tempting to use (Binary_Len(s) - ALIGN_SIZE).  However,
// some routines make allocations bigger than they ultimately need and do not
// realloc() before converting the memory to a series...rebInflate() and
// rebDeflate() do this.  So a version passing the size will be necessary,
// and since C does not have the size exposed in malloc() and you track it
// yourself, it seems fair to *always* ask the caller to pass in a size.
//
REBVAL *RL_rebRepossess(void *ptr, size_t size)
{
    ENTER_API;

    Binary(*) *ps = cast(Binary(*)*, ptr) - 1;
    Unpoison_Memory_If_Sanitize(ps, sizeof(Binary(*)));  // need to underrun to fetch `s`

    Binary(*) s = *ps;
    assert(Not_Series_Flag(s, MANAGED));

    if (size > Binary_Len(s) - ALIGN_SIZE)
        fail ("Attempt to rebRepossess() more than rebMalloc() capacity");

    assert(Get_Series_Flag(s, DONT_RELOCATE));
    Clear_Series_Flag(s, DONT_RELOCATE);

    if (Get_Series_Flag(s, DYNAMIC)) {
        //
        // Dynamic series have the concept of a "bias", which is unused
        // allocated capacity at the head of a series.  Bump the "bias" to
        // treat the embedded Series(*) (aligned to REBI64) as unused capacity.
        //
        Set_Series_Bias(s, ALIGN_SIZE);
        s->content.dynamic.data += ALIGN_SIZE;
        s->content.dynamic.rest -= ALIGN_SIZE;
    }
    else {
        // Data is in Series Stub itself, no bias.  Just slide the bytes down.
        //
        memmove(  // src overlaps destination, can't use memcpy()
            Binary_Head(s),
            Binary_Head(s) + ALIGN_SIZE,
            size
        );
    }

    Term_Binary_Len(s, size);
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
    assert(not PG_Api_Initialized);
    PG_Api_Initialized = true;
}


//
//  Shutdown_Api: C
//
// See remarks on Startup_Api() for the difference between this idea and
// rebShutdown.
//
void Shutdown_Api(void)
{
    assert(PG_Api_Initialized);
    PG_Api_Initialized = false;
}


//
//  rebStartup: RL_API
//
// This function will allocate and initialize all memory structures used by
// the REBOL interpreter. This is an extensive process that takes time.
//
void RL_rebStartup(void)
{
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
    ENTER_API;

  #if !defined(NDEBUG)
    //
    // The debug build does a clean Shutdown, Startup, and then shutdown again
    // to make sure we can do so in case a system wanted to uninitialize then
    // reinitialize.
    //
    Shutdown_Core(true);
    Startup_Core();
  #endif

    // Everything Shutdown_Core() does pertains to getting a no-leak state
    // for Valgrind/etc, but it shouldn't have any user-facing side-effects
    // besides that if you don't run it.
    //
    Shutdown_Core(clean);
}


//
//  rebTick: RL_API
//
// If the executable is built with tick counting, this will return the tick
// without requiring any Rebol code to run (which would disrupt the tick).
//
uintptr_t RL_rebTick(void)
{
    ENTER_API;

  #if DEBUG_COUNT_TICKS
    return cast(long, TG_tick);
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
//   `rebElide("print [", rebI(count), "]");` does not need to rebRelease()
//   the count variable because the evaluator frees it immediately after use.
//
//=////////////////////////////////////////////////////////////////////////=//


// rebNull is a #define of `(REBVAL*)0` in %rebol.h, so no API entry point
//
// See notes in %rebol.h about why NOT to use ordinary `#define NULL 0` !
// But C++ nullptr (or shim) should be used instead if available.


//
//  rebVoid: RL_API
//
REBVAL *RL_rebVoid(void)
{
    ENTER_API;

    REBVAL* v = Alloc_Value();  // just has NODE_FLAG_ROOT, counts as "fresh"
    Init_Void(v);
    return v;
}


//
//  rebNone: RL_API
//
REBVAL *RL_rebNone(void)
{
    ENTER_API;

    return Init_None(Alloc_Value());
}


//
//  rebBlank: RL_API
//
REBVAL *RL_rebBlank(void)
{
    ENTER_API;

    return Init_Blank(Alloc_Value());
}


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
REBVAL *RL_rebLogic(bool logic)
{
    ENTER_API;

    return Init_Logic(Alloc_Value(), did logic);
}


//
//  rebChar: RL_API
//
REBVAL *RL_rebChar(uint32_t codepoint)
{
    ENTER_API;

    REBVAL* v = Alloc_Value();
    Context(*) error = Maybe_Init_Char(v, codepoint);
    if (error) {
        rebRelease(v);
        fail (error);
    }
    return v;
}


//
//  rebInteger: RL_API
//
// !!! Should there be rebSigned() and rebUnsigned(), in order to catch cases
// of using out of range values?
//
REBVAL *RL_rebInteger(int64_t i)
{
    ENTER_API;

    return Init_Integer(Alloc_Value(), i);
}


//
//  rebDecimal: RL_API
//
REBVAL *RL_rebDecimal(double dec)
{
    ENTER_API;

    return Init_Decimal(Alloc_Value(), dec);
}


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
    ENTER_API;

    Binary(*) bin = Make_Binary(size);
    memcpy(Binary_Head(bin), bytes, size);
    Term_Binary_Len(bin, size);

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
    ENTER_API;

    Binary(*) bin = Make_Binary(size);

    // !!! Caution, unfilled bytes, access or molding may be *worse* than
    // random by the rules of C if they don't get written!  Must be filled
    // immediately by caller--before a GC or other operation.
    //
    Term_Binary_Len(bin, size);

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
    ENTER_API;

    return Binary_Head(VAL_BINARY_Known_Mutable(binary));
}


//
//  rebBinaryAt_internal: RL_API
//
unsigned char *RL_rebBinaryAt_internal(const REBVAL *binary)
{
    ENTER_API;

    return VAL_BINARY_AT_Known_Mutable(binary);
}


//
//  rebBinarySizeAt_internal: RL_API
//
unsigned int RL_rebBinarySizeAt_internal(const REBVAL *binary)
{
    ENTER_API;

    return VAL_LEN_AT(binary);
}


//
//  rebSizedText: RL_API
//
// If utf8 does not contain valid UTF-8 data, this may fail().
//
// !!! Should there be variants for Strict/Relaxed, e.g. a version that does
// not accept CR and one that does?
//
REBVAL *RL_rebSizedText(const char *utf8, size_t size)
{
    ENTER_API;

    return Init_Text(
        Alloc_Value(),
        Append_UTF8_May_Fail(nullptr, utf8, size, STRMODE_ALL_CODEPOINTS)
    );
}


//
//  rebText: RL_API
//
REBVAL *RL_rebText(const char *utf8)
{
    ENTER_API;

    return rebSizedText(utf8, strsize(utf8));
}


//
//  rebLengthedTextWide: RL_API
//
REBVAL *RL_rebLengthedTextWide(const REBWCHAR *wstr, unsigned int num_chars)
{
    ENTER_API;

    DECLARE_MOLD (mo);
    Push_Mold(mo);

    for (; num_chars != 0; --num_chars, ++wstr)
        Append_Codepoint(mo->series, *wstr);

    return Init_Text(Alloc_Value(), Pop_Molded_String(mo));
}


//
//  rebTextWide: RL_API
//
// Imports a TEXT! from UTF-16 (potentially multi-wchar-per-codepoint encoding)
//
REBVAL *RL_rebTextWide(const REBWCHAR *wstr)
{
    ENTER_API;

    DECLARE_MOLD (mo);
    Push_Mold(mo);

    while (*wstr != 0) {
        if (*wstr >= UNI_SUR_HIGH_START and *wstr <= UNI_SUR_HIGH_END) {
            if (not (
                *(wstr + 1) >= UNI_SUR_LOW_START
                and *(wstr + 1) <= UNI_SUR_LOW_END
            )){
                fail ("Invalid UTF-16 surrogate pair passed to rebTextWide()");
            }
            Append_Codepoint(mo->series, Decode_UTF16_Pair(wstr));
            wstr += 2;
        }
        else {
            Append_Codepoint(mo->series, *wstr);
            ++wstr;
        }
    }
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
REBVAL *RL_rebHandle(
    void *data,  // !!! What about `const void*`?  How to handle const?
    size_t length,
    CLEANUP_CFUNC *cleaner
){
    ENTER_API;

    return Init_Handle_Cdata_Managed(Alloc_Value(), data, length, cleaner);
}


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
    ENTER_API;

    Level(*) L = TOP_LEVEL;
    Phase(*) act = Level_Phase(L);

    // !!! Currently the JavaScript wrappers do not do the right thing for
    // taking just a `const char*`, so this falsely is a variadic to get the
    // JavaScript string proxying.
    //
    const char *name;
    const void *p2;
    if (vaptr) {
        name = cast(const char*, p);
        p2 = va_arg(*vaptr, const void*);
    }
    else {
        const void* const *packed = cast(const void* const*, p);
        name = cast(const char*, *packed++);
        p2 = *packed++;
    }
    if (Detect_Rebol_Pointer(p2) != DETECTED_AS_END)
        fail ("rebArg() isn't actually variadic, it's arity-1");

    Symbol(const*) symbol = Intern_UTF8_Managed(cb_cast(name), strsize(name));

    const REBKEY *tail;
    const REBKEY *key = ACT_KEYS(&tail, act);
    REBVAL *arg = Level_Args_Head(L);
    for (; key != tail; ++key, ++arg) {
        if (Are_Synonyms(KEY_SYMBOL(key), symbol))
            return NULLIFY_NULLED(arg);
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
    ENTER_API;

    const void* argR = RL_rebArgR(p, vaptr);
    if (not argR)
        return nullptr;

    const REBVAL *arg = cast(const REBVAL*, argR);  // sneaky, but we know!
    return Copy_Cell(Alloc_Value(), arg);  // don't give REBVAL* arg directly
}


//=//// EVALUATIVE EXTRACTORS /////////////////////////////////////////////=//
//
// The libRebol API evaluative routines are all variadic, and call the
// evaluator on multiple pointers.  Each pointer may be:
//
// - a REBVAL*
// - a UTF-8 string to be scanned as one or more values in the sequence
// - a Series(*) that represents an "API instruction"
//
// There isn't a separate concept of routines that perform evaluations and
// ones that extract C fundamental types out of Rebol values.  Hence you
// don't have to say:
//
//      REBVAL *value = rebValue("1 +", some_rebol_integer);
//      int sum = rebUnboxInteger(value);
//      rebRelease(value);
//
// You can just write:
//
//      int sum = rebUnboxInteger("1 +", some_rebol_integer);
//
// The default evaluators splice Rebol values "as-is" into the feed.  This
// means that any evaluator active types (like WORD!, ACTION!, GROUP!...)
// will run.  This can be mitigated with rebQ or "@"


//
//  Run_Va_Throws: C
//
// * Due to the nature of C va_lists you always have to have one non-variadic
//   parameter.  This turns out all right with Ren-C being able to even work
//   with something like `rebValue()`, because rebValue() is actually a macro
//   that throws in a rebEND to get `rebValue_inline(rebEND)`.
//
// * Every variadic entry point receives the non-optional pointer `p`, and
//   the captured va_list for the rest of the arguments in `vaptr`.
//
//  (va_list by pointer: http://stackoverflow.com/a/3369762/211160)
//
//   However, there's a special meaning given to `p` when vaptr is null.
//   In that case, p is no longer the first variadic argument but a pointer
//   to a packed array of `const void*`.  This method is preferred by the
//   C++ build, because using variadic templates it can recursively process
//   the arguments and pack them into that array...doing additional type
//   checking and conversions.
//
//  (The WebAssembly also uses this packed array format, as it does not
//   require delving into the compiler-specific details of how a va_list is
//   encoded...and can stick to the standardized layout of a pointer array.)
//
static bool Run_Va_Throws(
    Atom(*) out,
    bool interruptible,  // whether a HALT can cause a longjmp/throw
    Flags flags,
    const void *p,  // first pointer (may be END, nullptr means NULLED)
    va_list *vaptr  // va_end() handled by feed for all cases (throws, fails)
){
    // !!! Some kind of policy is needed to decide how to disable halting in
    // the API.  It uses the longjmp() mechanism as a "no catch for throw",
    // meaning that an error could be introduced at any moment in the code.
    // Recovery from a HALT is almost like recovering from a stack overflow
    // exception, in terms of how bad the program state could wind up (though
    // the intereter will be okay, it's like any line in your program could
    // have half-run.  Review a holistic answer.
    //
    Flags saved_sigmask = g_ts.eval_sigmask;
    if (interruptible)
        g_ts.eval_sigmask |= SIG_HALT;  // enable
    else
        g_ts.eval_sigmask &= ~SIG_HALT;  // disable

    Feed(*) feed = Make_Variadic_Feed(
        p, vaptr,
        Get_Context_From_Stack(),
        FEED_MASK_DEFAULT
    );

    Level(*) L = Make_Level(
        feed,
        LEVEL_FLAG_ALLOCATED_FEED | flags
    );
    L->executor = &Array_Executor;

    bool threw = Trampoline_Throws(out, L);

    if (not threw and (flags & LEVEL_FLAG_META_RESULT))
        assert(QUOTE_BYTE(out) >= QUASI_2);

    // (see also Reb_State->saved_sigmask RE: if a longjmp happens)
    g_ts.eval_sigmask = saved_sigmask;

    return threw;
}


// This version of the variadic run does not do isotope decay, because that
// errors on empty parameter packs ("none"), e.g. ~[]~ isotopes.  It would be
// deceptive to have that unpack and conflate as any other state (including
// void and null, which have specific meanings).  So it simply won't.
//
// An API like rebValue() or rebUnboxLogic() thus won't handle ~[]~, and you
// need to use rebMeta() to get the parameter pack and understand its nature,
// or use rebElide() and say it doesn't matter.  (or just find a way in the
// executed code passed to not have it evaluate to none)
//
inline static void Run_Va_Undecayed_May_Fail_Calls_Va_End(
    Atom(*) out,
    const void *p,  // first pointer (may be END, nullptr means NULLED)
    va_list *vaptr  // va_end() handled by feed for all cases (throws, fails)
){
    bool interruptible = false;
    if (Run_Va_Throws(out, interruptible, LEVEL_MASK_NONE, p, vaptr)) {
        //
        // !!! Being able to THROW across C stacks is necessary in the general
        // case (consider implementing QUIT or HALT).  Probably need to be
        // converted to a kind of error, and then re-converted into a THROW
        // to bubble up through Rebol stacks?  Development on this is ongoing.
        //
        fail (Error_No_Catch_For_Throw(TOP_LEVEL));
    }
}

// This is the decaying form.  Decaying is done for convenience in the basic
// APIs just as if you'd tried to do a SET-WORD!...so you don't have to worry
// about multi-return packs etc.
//
inline static void Run_Va_Decay_May_Fail_Calls_Va_End(
    Value(*) out,
    const void *p,  // first pointer (may be END, nullptr means NULLED)
    va_list *vaptr  // va_end() handled by feed for all cases (throws, fails)
){
    Run_Va_Undecayed_May_Fail_Calls_Va_End(out, p, vaptr);

    Decay_If_Unstable(cast(Atom(*), out));
}


//
//  rebRunCoreThrows: RL_API
//
// Most API routines (rebValue(), rebDid(), etc.) have no way of handling
// thrown values or invisibles.  They also run more than one step.
//
// This function runs one evaluation step, and allows you to say whether the
// step must consume all input or not (see EVAL_EXECUTOR_FLAG_NO_RESIDUE).
//
// The output is written into a cell that is provided--which is not something
// the API should do--especially considering that this can evaluate to a void
// cell.  But it's in the API file because we want the wrapping machinery
// that handles the variadics to be applied here.
//
// There is a rebRunThrows() macro that passes in the flags LEVEL_MASK_NONE
// and EVAL_EXECUTOR_FLAG_NO_RESIDUE defined in %sys-do.h
//
bool RL_rebRunCoreThrows(
    REBVAL *out,
    uintptr_t flags,  // Flags not exported in API
    const void *p, va_list *vaptr
){
    Feed(*) feed = Make_Variadic_Feed(
        p, vaptr,
        Get_Context_From_Stack(),
        FEED_MASK_DEFAULT
    );

    Level(*) L = Make_Level(
        feed,
        flags | LEVEL_FLAG_ALLOCATED_FEED
    );

    Push_Level(out, L);

    if (Trampoline_With_Top_As_Root_Throws()) {
        Drop_Level(L);
        return true;
    }

    bool too_many = (flags & EVAL_EXECUTOR_FLAG_NO_RESIDUE)
        and Not_Feed_At_End(feed);  // feed will be freed in Drop_Level()

    Drop_Level(L);  // will va_end() if not reified during evaluation

    if (too_many)
        fail (Error_Apply_Too_Many_Raw());

    Decay_If_Unstable(cast(Atom(*), out));
    return false;
}


//
//  rebValue: RL_API
//
// Most basic evaluator that returns a REBVAL*, which must be rebRelease()'d.
//
REBVAL *RL_rebValue(const void *p, va_list *vaptr)
{
    ENTER_API;

    REBVAL *result = Alloc_Value();
    Run_Va_Decay_May_Fail_Calls_Va_End(result, p, vaptr);

    if (Is_Nulled(result)) {
        rebRelease(result);
        return nullptr;  // No NULLED cells in API, see NULLIFY_NULLED()
    }

    return result;  // caller must rebRelease()
}


//
//  rebTranscodeInto: RL_API
//
// Just scans the source given into a BLOCK! without executing it.
//
// 1. This uses whatever context is currently considered "active" on the
//    stack, which is consistent with the general behavior of the API.  It's
//    all being juggled around right now, but see Scan_UTF8_Managed() for
//    a non-variadic entry point to scanning that is related.
//
//
REBVAL *RL_rebTranscodeInto(
    REBVAL *out,
    const void *p, va_list *vaptr
){
    ENTER_API;

    Feed(*) feed = Make_Variadic_Feed(
        p, vaptr,
        Get_Context_From_Stack(),  // No context parameter, see [1]
        FEED_MASK_DEFAULT
    );
    Sync_Feed_At_Cell_Or_End_May_Fail(feed);

    StackIndex base = TOP_INDEX;
    while (Not_Feed_At_End(feed)) {
        Derelativize(PUSH(), At_Feed(feed), FEED_SPECIFIER(feed));
        Set_Cell_Flag(TOP, UNEVALUATED);
        Fetch_Next_In_Feed(feed);
    }

    Free_Feed(feed);  // Note: exhausting feed should take care of the va_end()

    return Init_Block(
        out,
        Pop_Stack_Values_Core(base, SERIES_FLAG_MANAGED)
    );
}


//
//  rebPushContinuation: RL_API
//
// Helper for when variadic code wants to run as its own stack level.
//
// 1. We don't call `rebTranscodeInto()` here, because that would package
//    up an arbitrary number of variadic parameters that are meant to
//    be things like REBVAL* and string.  But we have exactly 3 parameters
//    in hand, and want to pass them directly to the implementation routine,
//    as they're encodings of variadic parameters--not the actual parameters!
//
void RL_rebPushContinuation(
    REBVAL *out,
    uintptr_t flags,
    const void *p, va_list *vaptr
){
    ENTER_API;

    DECLARE_LOCAL (block);
    RL_rebTranscodeInto(cast(REBVAL*, block), p, vaptr);  // use "RL_", see [1]

    Level(*) L = Make_Level_At(block, flags);
    Push_Level(out, L);
    L->executor = &Array_Executor;
}


//
//  rebMeta: RL_API
//
// Builds in a ^META operation to rebValue; shorthand that's more efficient.
//
//     rebMeta(...) => rebValue("meta", ...")
//
// Will return parameter packs as-is.
//
REBVAL *RL_rebMeta(const void *p, va_list *vaptr)
{
    ENTER_API;

    REBVAL *v = Alloc_Value();
    bool interruptible = false;
    if (Run_Va_Throws(v, interruptible, LEVEL_FLAG_META_RESULT, p, vaptr))
        fail (Error_No_Catch_For_Throw(TOP_LEVEL));  // panic?

    assert(not Is_Nulled(v));  // meta operations cannot produce NULL

    return v;  // caller must rebRelease()
}


//
//  rebEntrap: RL_API
//
// Builds in an ENTRAP operation to rebValue; shorthand that's more efficient.
//
//     rebEntrap(...) => rebValue("entrap [", ..., "]")
//
REBVAL *RL_rebEntrap(const void *p, va_list *vaptr)
{
    ENTER_API;

    REBVAL *v = Alloc_Value();
    bool interruptible = false;
    if (Run_Va_Throws(v, interruptible, LEVEL_FLAG_META_RESULT, p, vaptr)) {
        Init_Error(v, Error_No_Catch_For_Throw(TOP_LEVEL));
        return v;
    }

    if (Is_Meta_Of_Pack(v)) {
        Meta_Unquotify_Decayed(v);
        Meta_Quotify(v);
    }

    assert(not Is_Nulled(v));  // meta operations cannot produce NULL

    return v;  // caller must rebRelease()
}


//
//  rebEntrapInterruptible: RL_API
//
// !!! The core interruptible routine used is this one inside of console code.
// More will be needed, but this is made to quarantine the unfinished design
// points to one routine for now.
//
REBVAL *RL_rebEntrapInterruptible(
    const void *p,
    va_list *vaptr
){
    ENTER_API;

    REBVAL *v = Alloc_Value();
    bool interruptible = true;
    if (Run_Va_Throws(v, interruptible, LEVEL_FLAG_META_RESULT, p, vaptr)) {
        Init_Error(v, Error_No_Catch_For_Throw(TOP_LEVEL));
        return v;
    }

    if (Is_Meta_Of_Pack(v)) {
        Meta_Unquotify_Decayed(v);
        Meta_Quotify(v);
    }

    assert(not Is_Nulled(v));  // META operations can't return null

    return v;  // caller must rebRelease()
}


//
//  rebQuote: RL_API
//
// Variant of rebValue() that simply quotes its result.  So `rebQuote(...)` is
// equivalent to `rebValue("quote", ...)`, with the advantage of being faster
// and not depending on what the QUOTE word looks up to.
//
// (It also has the advantage of not showing QUOTE on the call stack.  That
// is important for the console when trapping its generated result, to be
// able to quote it without the backtrace showing a QUOTE stack frame.)
//
REBVAL *RL_rebQuote(const void *p, va_list *vaptr)
{
    ENTER_API;

    REBVAL *result = Alloc_Value();
    Run_Va_Decay_May_Fail_Calls_Va_End(result, p, vaptr);

    return Quotify(result, 1);  // nulled cells legal for API if quoted
}


//
//  rebElide: RL_API
//
// Variant of rebValue() which assumes you don't need the result.  This saves on
// allocating an API handle, or the caller needing to manage its lifetime.
//
// It also means that if the product is something like a ~[]~ isotope ("none")
// that is not an issue.
//
void RL_rebElide(const void *p, va_list *vaptr)
{
    ENTER_API;

    DECLARE_LOCAL (elided);
    Run_Va_Undecayed_May_Fail_Calls_Va_End(elided, p, vaptr);
}


//
//  rebJumps: RL_API [
//      #noreturn
//  ]
//
// rebJumps() is like rebElide, but has the noreturn attribute.  This helps
// inform the compiler that the routine is not expected to return.  Use it
// with things like `rebJumps("fail", ...)` or `rebJumps("THROW", ...)`.  If
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
void RL_rebJumps(const void *p, va_list *vaptr)
{
    ENTER_API;

    DECLARE_STABLE (dummy);
    Run_Va_Decay_May_Fail_Calls_Va_End(dummy, p, vaptr);

    // Note: If we just `fail()` here, then while MSVC compiles %a-lib.c at
    // higher optimization levels it can conclude that RL_rebJumps() never
    // returns.  Then it will give an error on the attempt to put a DEAD_END()
    // notification in the inline wrapper `rebJumps()`, which is needed to
    // suppress the warning when RL_rebJumps() is not available.  This Catch-22
    // of saying the DEAD_END() itself is unreachable code is annoying...but
    // it's best not to turn off the warning.  Throw in a runtime twist that
    // it can't guarantee won't happen (but won't) so it doesn't use special
    // knowledge that RL_rebJumps() does not return.
    //
    assert(p != nullptr);
    if (p == nullptr)
        return;

    fail ("rebJumps() was used to run code, but it didn't FAIL/QUIT/THROW!");
}


//
//  rebDid: RL_API
//
// Analogue of DID, which is an isotope-tolerant version of NON NULL?.
// e.g. "Would the supplied expression run a THEN"
// See DECLARE_NATIVE(did_1) for explanation.
//
// !!! This does not handle isotopic objects, an experimental concept where
// if the object supports a THEN method it would pass the DID test.  Review
// this in light of whether isotopic objects are going to be kept.
//
bool RL_rebDid(const void *p, va_list *vaptr)
{
    ENTER_API;

    DECLARE_LOCAL (condition);
    Run_Va_Undecayed_May_Fail_Calls_Va_End(condition, p, vaptr);

    return not Is_Nulled(condition) and not Is_Void(condition);
}


//
//  rebDidnt: RL_API
//
// Analogue of DIDN'T, which is an isotope-tolerant version of NULL?.
// e.g. "Would the supplied expression run an ELSE"
//
// !!! This does not handle isotopic objects, an experimental concept where
// if the object supports a ELSE method it would pass the DIDN'T test.  Review
// this in light of whether isotopic objects are going to be kept.
//
bool RL_rebDidnt(const void *p, va_list *vaptr)
{
    ENTER_API;

    DECLARE_LOCAL (condition);
    Run_Va_Undecayed_May_Fail_Calls_Va_End(condition, p, vaptr);

    return Is_Nulled(condition) or Is_Void(condition);
}

//
//  rebTruthy: RL_API
//
// Simply returns the logical result, with no returned handle to release.
//
// !!! The name is bad, but it's hard to think of a good name now that
// rebDid() is taken for other purposes.  Avoid this and use rebTruthy() if
// at all possible.
//
bool RL_rebTruthy(const void *p, va_list *vaptr)
{
    ENTER_API;

    DECLARE_STABLE (condition);
    Run_Va_Decay_May_Fail_Calls_Va_End(condition, p, vaptr);

    return Is_Truthy(condition);  // will fail() on (most) isotopes
}


//
//  rebNot: RL_API
//
// !!! If this were going to be a macro like (not (rebTruthy(...))) it
// would have to be a variadic macro.  Not worth it. use separate entry point.
//
bool RL_rebNot(const void *p, va_list *vaptr)
{
    ENTER_API;

    DECLARE_STABLE (condition);
    Run_Va_Decay_May_Fail_Calls_Va_End(condition, p, vaptr);

    return Is_Falsey(condition);  // will fail() on isotopes
}



//
//  rebUnbox: RL_API
//
// C++, JavaScript, and other languages can do some amount of intelligence
// with a generic `rebUnbox()` operation...either picking the type to return
// based on the target in static typing, or returning a dynamically typed
// value.  For convenience in C, make the generic unbox operation return
// an integer for INTEGER!, LOGIC!, CHAR!...assume it's most common so the
// short name is worth it.
//
intptr_t RL_rebUnbox(const void *p, va_list *vaptr)
{
    ENTER_API;

    DECLARE_STABLE (result);
    Run_Va_Decay_May_Fail_Calls_Va_End(result, p, vaptr);

    if (IS_LOGIC(result)) {
        return VAL_LOGIC(result) ? 1 : 0;
    }
    else switch (VAL_TYPE(result)) {
      case REB_INTEGER:
        return VAL_INT64(result);

      case REB_ISSUE:
        return VAL_CHAR(result);

      default:
        fail ("C-based rebUnbox() only supports INTEGER!, CHAR!, and LOGIC!");
    }
}


//
//  rebUnboxLogic: RL_API
//
bool RL_rebUnboxLogic(
    const void *p,
    va_list *vaptr
){
    ENTER_API;

    DECLARE_STABLE (result);
    Run_Va_Decay_May_Fail_Calls_Va_End(result, p, vaptr);

    if (not IS_LOGIC(result))
        fail ("rebUnboxLogic() called on non-LOGIC!");

    return VAL_LOGIC(result);
}


//
//  rebUnboxInteger: RL_API
//
intptr_t RL_rebUnboxInteger(
    const void *p,
    va_list *vaptr
){
    ENTER_API;

    DECLARE_STABLE (result);
    Run_Va_Decay_May_Fail_Calls_Va_End(result, p, vaptr);

    if (not IS_INTEGER(result))
        fail ("rebUnboxInteger() called on non-INTEGER!");

    return VAL_INT64(result);
}


//
//  rebUnboxDecimal: RL_API
//
double RL_rebUnboxDecimal(
    const void *p, va_list *vaptr
){
    ENTER_API;

    DECLARE_STABLE (result);
    Run_Va_Decay_May_Fail_Calls_Va_End(result, p, vaptr);

    if (IS_DECIMAL(result))
        return VAL_DECIMAL(result);

    if (IS_INTEGER(result))
        return cast(double, VAL_INT64(result));

    fail ("rebUnboxDecimal() called on non-DECIMAL! or non-INTEGER!");
}


//
//  rebUnboxChar: RL_API
//
uint32_t RL_rebUnboxChar(
    const void *p, va_list *vaptr
){
    ENTER_API;

    DECLARE_STABLE (result);
    Run_Va_Decay_May_Fail_Calls_Va_End(result, p, vaptr);

    if (not IS_CHAR(result))
        fail ("rebUnboxChar() called on non-CHAR");

    return VAL_CHAR(result);
}


//
//  rebUnboxHandle: RL_API
//
void *RL_rebUnboxHandle(
    size_t *size_out,
    const void *p, va_list *vaptr
){
    ENTER_API;

    DECLARE_STABLE (result);
    Run_Va_Decay_May_Fail_Calls_Va_End(result, p, vaptr);

    if (VAL_TYPE(result) != REB_HANDLE)
        fail ("rebUnboxHandle() called on non-HANDLE!");

    *size_out = VAL_HANDLE_LEN(result);
    return VAL_HANDLE_POINTER(void*, result);
}


// Helper function for `rebSpellInto()` and `rebSpell()`
//
static size_t Spell_Into(
    char *buf,
    size_t buf_size,  // number of bytes
    const REBVAL *v
){
    if (not ANY_UTF8(v))
        fail ("rebSpell() APIs require UTF-8 types (strings, words, tokens)");

    Size utf8_size;
    Utf8(const*) utf8 = VAL_UTF8_SIZE_AT(&utf8_size, v);

    if (not buf) {
        assert(buf_size == 0);
        return utf8_size;  // caller must allocate a buffer of size + 1
    }

    Size limit = MIN(buf_size, utf8_size);
    memcpy(buf, utf8, limit);
    buf[limit] = 0;
    return utf8_size;
}


//
//  rebSpellInto: RL_API
//
// Extract UTF-8 data from an ANY-STRING! or ANY-WORD!.
//
// API does not return the number of UTF-8 characters for a value, because
// the answer to that is always cached for any value position as LENGTH OF.
// The more immediate quantity of concern to return is the number of bytes.
//
size_t RL_rebSpellInto(
    char *buf,
    size_t buf_size,  // number of bytes
    const void *p, va_list *vaptr
){
    ENTER_API;

    DECLARE_STABLE (v);
    Run_Va_Decay_May_Fail_Calls_Va_End(v, p, vaptr);

    return Spell_Into(buf, buf_size, v);
}


//
//  rebTrySpell: RL_API
//
// This gives the spelling as UTF-8 bytes.  Length in codepoints should be
// extracted with LENGTH OF.  If size in bytes of the encoded UTF-8 is needed,
// use the binary extraction API (works on ANY-STRING! to get UTF-8)
//
// Can return nullptr.  Use rebSpell() if you want a failure instead.
//
char *RL_rebTrySpell(const void *p, va_list *vaptr)
{
    ENTER_API;

    DECLARE_STABLE (v);
    Run_Va_Decay_May_Fail_Calls_Va_End(v, p, vaptr);

    if (Is_Nulled(v))
        return nullptr;

    size_t size = Spell_Into(nullptr, 0, v);
    char *result = rebAllocN(char, size);  // no +1 for term needed...
    assert(result[size] == '\0');  // ...see rebRepossess() for why this is

    size_t check = Spell_Into(result, size, v);
    assert(check == size);
    UNUSED(check);

    return result;
}

//
//  rebSpell: RL_API
//
// Raises error on NULL
//
char *RL_rebSpell(const void *p, va_list *vaptr) {
    char* spell = RL_rebTrySpell(p, vaptr);
    if (spell == nullptr)
        fail ("rebSpell() does not take NULL, see rebTrySpell()");
    return spell;
}


// Helper function for `rebSpellIntoWide()` and `rebSpellWide()`
//
static unsigned int Spell_Into_Wide(
    REBWCHAR *buf,
    unsigned int buf_wchars,  // chars buf can hold (not including terminator)
    const REBVAL *v
){
    if (not ANY_UTF8(v))
        fail ("rebSpell() APIs require UTF-8 types (strings, words, tokens)");

    if (not buf)  // querying for size
        assert(buf_wchars == 0);

    unsigned int num_wchars = 0;  // some codepoints need 2 wchars

    Utf8(const*) cp = VAL_UTF8_AT(v);

    Codepoint c;
    cp = NEXT_CHR(&c, cp);

    REBLEN i = 0;
    while (c != '\0' and i < buf_wchars) {
        if (c <= 0xFFFF) {
            buf[i] = c;
            ++i;
            ++num_wchars;
        }
        else {  // !!! Should there be a UCS-2 version that fails here?
            if (i == buf_wchars - 1)
                break;  // not enough space for surrogate pair

            Encode_UTF16_Pair(c, &buf[i]);
            i += 2;
            num_wchars += 2;
        }
        cp = NEXT_CHR(&c, cp);
    }

    if (buf)
        buf[i] = 0;

    while (c != '\0') {  // count residual wchars there was no capacity for
        if (c <= 0xFFFF)
            num_wchars += 1;  // fits in one 16-bit wchar
        else
            num_wchars += 2;  // requires surrogate pair to represent

        cp = NEXT_CHR(&c, cp);
    }

    return num_wchars;  // if allocating, caller needs space for num_wchars + 1
}


//
//  rebSpellIntoWide: RL_API
//
// Extract UTF-16 data from an ANY-STRING! or ANY-WORD!.  Note this is *not*
// UCS-2, so codepoints that won't fit in one WCHAR will take up two WCHARs
// by means of a surrogate pair.  Hence the returned value is a count of
// wchar units...not *necesssarily* a length in codepoints.
//
unsigned int RL_rebSpellIntoWide(
    REBWCHAR *buf,
    unsigned int buf_chars,  // chars buf can hold (not including terminator)
    const void *p, va_list *vaptr
){
    ENTER_API;

    DECLARE_STABLE (v);
    Run_Va_Decay_May_Fail_Calls_Va_End(v, p, vaptr);

    return Spell_Into_Wide(buf, buf_chars, v);
}


//
//  rebTrySpellWide: RL_API
//
// Gives the spelling as WCHARs.  The result is UTF-16, so some codepoints
// won't fit in single WCHARs.
//
REBWCHAR *RL_rebTrySpellWide(const void *p, va_list *vaptr)
{
    ENTER_API;

    DECLARE_STABLE (v);
    Run_Va_Decay_May_Fail_Calls_Va_End(v, p, vaptr);

    if (Is_Nulled(v))
        return nullptr;

    REBLEN len = Spell_Into_Wide(nullptr, 0, v);
    REBWCHAR *result = cast(
        REBWCHAR*, rebMalloc(sizeof(REBWCHAR) * (len + 1))
    );

    REBLEN check = Spell_Into_Wide(result, len, v);
    assert(check == len);
    UNUSED(check);

    return result;
}


//
//  rebSpellWide: RL_API
//
// Raises error on NULL
//
REBWCHAR *RL_rebSpellWide(const void *p, va_list *vaptr)
{
    REBWCHAR* spell = RL_rebTrySpellWide(p, vaptr);
    if (spell == nullptr)
        fail ("rebSpellWide() does not take NULL, see rebTrySpellWide()");
    return spell;
}


// Helper function for `rebBytesInto()` and `rebBytes()`
//
// CHAR!, ANY-STRING!, and ANY-WORD! are allowed without an AS BINARY!.
//
// !!! How many types should be allowed to convert automatically?
//
static size_t Bytes_Into(
    unsigned char *buf,
    size_t buf_size,
    const REBVAL *v
){
    if (IS_BINARY(v)) {
        Size size;
        const Byte* data = VAL_BINARY_SIZE_AT(&size, v);
        if (buf == nullptr) {
            assert(buf_size == 0);
            return size;
        }

        Size limit = MIN(buf_size, size);
        memcpy(buf, data, limit);
        return size;
    }

    if (IS_CHAR(v)) {  // Note: CHAR! caches its UTF-8 encoding in the cell
        Size size = VAL_CHAR_ENCODED_SIZE(v);
        if (buf == nullptr) {
            assert(buf_size == 0);
            return size;
        }

        Size limit = MIN(buf_size, size);
        memcpy(buf, VAL_CHAR_ENCODED(v), limit);
        return size;
    }

    if (ANY_WORD(v) or ANY_STRING(v)) {
        Size size = Spell_Into(nullptr, 0, v);
        if (buf == nullptr) {
            assert(buf_size == 0);
            return size;
        }

        Size check = Spell_Into(s_cast(buf), buf_size, v);
        assert(check == size);
        UNUSED(check);

        return size;
    }

    fail ("rebBytes() only works with ANY-STRING!/ANY-WORD!/BINARY!/CHAR!");
}


//
//  rebBytesInto: RL_API
//
// Extract binary data from a BINARY!
//
// !!! Caller must allocate a buffer of the returned size + 1.  It's not clear
// if this is a good idea; but this is based on a longstanding convention of
// zero termination of Rebol series, including binaries.  Review.
//
size_t RL_rebBytesInto(
    unsigned char *buf,
    size_t buf_size,
    const void *p, va_list *vaptr
){
    ENTER_API;

    DECLARE_STABLE (v);
    Run_Va_Decay_May_Fail_Calls_Va_End(v, p, vaptr);

    return Bytes_Into(buf, buf_size, v);
}


//
//  rebTryBytes: RL_API
//
// Can be used to get the bytes of a BINARY! and its size, or the UTF-8
// encoding of an ANY-STRING! or ANY-WORD! and that size in bytes.  (Hence,
// for strings it is like rebSpell() except telling you how many bytes.)
//
unsigned char *RL_rebTryBytes(
    size_t *size_out,  // !!! Enforce non-null, to ensure type safety?
    const void *p, va_list *vaptr
){
    ENTER_API;

    DECLARE_STABLE (v);
    Run_Va_Decay_May_Fail_Calls_Va_End(v, p, vaptr);

    if (Is_Nulled(v)) {
        *size_out = 0;
        return nullptr;  // blank in, null out
    }

    Size size = Bytes_Into(nullptr, 0, v);

    unsigned char *result = rebAllocN(unsigned char, size);  // no +1 needed...
    assert(result[size] == '\0');  // ...see rebRepossess() for why
    Bytes_Into(result, size, v);

    *size_out = size;
    return cast(unsigned char*, result);
}


//
//  rebBytes: RL_API
//
// Raises error on NULL
//
unsigned char *RL_rebBytes(
    size_t *size_out,  // !!! Enforce non-null, to ensure type safety?
    const void *p, va_list *vaptr
){
    unsigned char* bytes = RL_rebTryBytes(size_out, p, vaptr);
    if (bytes == nullptr)
        fail ("rebBytes() does not take NULL, see rebTryBytes()");
    return bytes;
}


//=//// EXCEPTION HANDLING ////////////////////////////////////////////////=//
//
// Exception handling with the API is a work-in-progress.  A lot has changed
// in the implementation, in particular support for either compiling with
// setjmp/longjmp -or- try/catch as the mechanic managing abrupt failures.
//
// But also, with stackless processing, only one setjmp() or try{} is needed
// for each invocation of the trampoline.  This means that abrupt failure
// protection comes "for free" with every API call that invokes a new
// trampoline...and it's just a matter of deciding what to do at the
// interface level if the result is an unhandled raised error.
//
// It's a largely uncharted territory at this time...so the hope is that
// your code "just works".  The JavaScript ReplPad runs the console extension
// code which is already protected by SYS.UTIL.RESCUE, so this captures the
// failures in API calls in JS-NATIVEs pretty well...but other scenarios
// might be messier.
//
//=////////////////////////////////////////////////////////////////////////=//


//
//  rebRescue: RL_API
//
// This API was an early attempt at wrapping arbitrary API calls which might
// have failures in the code they evaluate.  It is named after Ruby's
// operation, which deals with the identical problem:
//
// http://silverhammermba.github.io/emberb/c/#rescue
//
// Unlike SYS.UTIL.RESCUE, it returns an ERROR! in case of failure, or the
// result of the code otherwise.  This creates conflation problems when
// the code actually returned an ERROR!.  SYS.UTIL.ENRESCUE solves this
// problem by returning a plain ERROR! in case of failure or a META result
// otherwise, so a plain error would appear quoted.
//
// !!! Redesign of this function at this time is probably not as good an
// investment as working more on interoperability of exceptions with
// JavaScript try/catch.  Code which is protecting against errors and
// knows it needs to can just use SYS.UTIL.RESCUE in the API call itself.
//
REBVAL *RL_rebRescue(
    REBDNG *dangerous,  // !!! pure C function if REBOL_FAIL_USES_LONGJMP
    void *opaque
){
    return RL_rebRescueWith(dangerous, nullptr, opaque);
}


//
//  rebRescueWith: RL_API
//
// Variant of rebRescue() with a handler, similar to Ruby's rescue2 operation.
//
// 1. We want API allocations via rebValue() or rebMalloc() that occur in the
//    body of the C function for the rebRescue() to be automatically cleaned
//    up in the case of an error.  There must be a frame to attach them to.
//
REBVAL *RL_rebRescueWith(
    REBDNG *dangerous,  // !!! pure C function only if not using throw/catch!
    REBRSC *rescuer,  // errors in the rescuer function will *not* be caught
    void *opaque
){
    ENTER_API;

    Level(*) dummy = Make_End_Level(LEVEL_MASK_NONE);
    Push_Level(nullptr, dummy);  // for owning API cells, see [1]

  RESCUE_SCOPE_IN_CASE_OF_ABRUPT_FAILURE {  //////////////////////////////////

    REBVAL *result = (*dangerous)(opaque);

    if (not result) {
        // null is considered a legal result
    }
    else if (
        rescuer == nullptr
        and QUOTE_BYTE(result) == UNQUOTED_1
        and HEART_BYTE(result) == REB_ERROR
    ){
        // If you don't have a handler for the error case then you can't
        // return an ERROR!, since all errors indicate a failure.  Use
        // HEART_BYTE() since BOUNCE_THROWN or other special things can be
        // used internally, and literal errors don't count either.
        //
        if (Is_Api_Value(result))
            rebRelease(result);

        Init_Word_Isotope(result, Canon(ERRORED));
        goto proxy_result;
    }
    else {
        if (not Is_Api_Value(result)) {
            // no proxying needed
        }
        else {
            assert(not Is_Nulled(result));  // leaked API nulled cell

            // !!! Automatically proxy the ownership of any managed handles
            // to the caller.  Any other handles that leak out (e.g. via
            // state) won't be covered by this, and must be unmanaged.

          proxy_result: {
            Array(*) a = Singular_From_Cell(result);
            Unlink_Api_Handle_From_Level(a);  // e.g. linked to f
            Link_Api_Handle_To_Level(a, dummy->prior);  // link to caller
          }
        }
    }

    Drop_Level(dummy);  // Drop_Level_Unbalanced() if for some internal uses

    CLEANUP_BEFORE_EXITING_RESCUE_SCOPE;
    return result;

} ON_ABRUPT_FAILURE(Context(*) e) {  /////////////////////////////////////////

    Drop_Level(dummy);

    REBVAL *error = Init_Error(Alloc_Value(), e);

    CLEANUP_BEFORE_EXITING_RESCUE_SCOPE;

    if (not rescuer)
        return error;  // plain rebRescue() behavior

    REBVAL *result = (*rescuer)(error, opaque);  // *not* guarded by RESCUE!
    rebRelease(error);
    return result;  // no special handling, may be null
}}


//
//  rebHalt: RL_API
//
// This function sets a signal that is checked during evaluation of code
// when it is run interruptibly.  Most API evaluations are not interruptible,
// because that would create unsafe situations.
//
// !!! Halting, exceptions, and stack overflows are all areas where the
// computing world in general doesn't have great answers.  Ren-C is nothing
// special in this regard, and more thought needs to be put into it!
//
void RL_rebHalt(void)
{
    ENTER_API;

    SET_SIGNAL(SIG_HALT);
}


//
//  rebWasHalting: RL_API
//
// Returns whether or not the halting signal is set, but clears it if set.
// Hence the question it answers is "was it halting" (previous to this call),
// because it never will be after it.
//
// Hence whoever checks this flag has erased the knowledge of a Ctrl-C signal,
// and bears the burden for propagating the signal up to something that does
// a HALT later--or it will be lost.
//
bool RL_rebWasHalting(void)
{
    ENTER_API;

    bool halting = GET_SIGNAL(SIG_HALT);
    CLR_SIGNAL(SIG_HALT);
    return halting;
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
//     rebValue(..., instruction, ...);  // the instruction may be corrupt now!
//
//=////////////////////////////////////////////////////////////////////////=//


//
//  rebQUOTING: RL_API
//
// This is #defined as rebQ.
//
// Note: This arity-1 version is pared back from a more complex variadic form:
// https://forum.rebol.info/t/1050/4
//
const REBINS *RL_rebQUOTING(const void *p)
{
    ENTER_API;

    if (p == nullptr)
        return FEED_NULL_SUBSTITUTE_CELL;  // precooked meta null

    Array(*) a;

    switch (Detect_Rebol_Pointer(p)) {
      case DETECTED_AS_SERIES: {
        a = m_cast(Array(*), cast(Array(const*), p));
        if (Not_Subclass_Flag(API, a, RELEASE))
            fail ("Can't quote instructions (besides rebR())");
        break; }

      case DETECTED_AS_CELL: {
        Value(const*) at = cast(Value(const*), p);
        if (Is_Nulled(at)) {
            assert(not Is_Api_Value(at));  // only internals use nulled cells
            return FEED_NULL_SUBSTITUTE_CELL;  // reified substitute
        }

        Value(*) v = Copy_Cell(Alloc_Value(), at);
        a = Singular_From_Cell(v);
        Set_Subclass_Flag(API, a, RELEASE);
        break; }

      default:
        fail ("Unknown pointer");
    }

    Value(*) v = VAL(Array_Single(a));
    Meta_Quotify(v);
    return a;
}


//
//  rebDERELATIVIZE: C
//
// This is stylized like an API, but actually not exported--it uses internal
// types.  This makes an cell that will last through a call and be freed.
//
const REBINS* rebDERELATIVIZE(Cell(const*) cell, REBSPC* specifier)
{
    Value(*) v = Derelativize(Alloc_Value(), cell, specifier);
    Array(*) a = Singular_From_Cell(v);
    Set_Subclass_Flag(API, a, RELEASE);
    return a;
}


//
//  rebUNQUOTING: RL_API
//
// This is #defined as rebU.
//
// Note: This arity-1 version is pared back from a more complex variadic form:
// https://forum.rebol.info/t/1050/4
//
const REBINS *RL_rebUNQUOTING(const void *p)
{
    ENTER_API;

    if (p == nullptr)
        fail ("Cannot unquote NULL");

    Array(*) a;

    switch (Detect_Rebol_Pointer(p)) {
      case DETECTED_AS_SERIES: {
        a = m_cast(Array(*), cast(Array(const*), p));
        if (Not_Subclass_Flag(API, a, RELEASE))
            fail ("Can't unquote instructions (besides rebR())");
        break; }

      case DETECTED_AS_CELL: {
        REBVAL *v = Copy_Cell(Alloc_Value(), cast(const REBVAL*, p));
        a = Singular_From_Cell(v);
        Set_Subclass_Flag(API, a, RELEASE);
        break; }

      default:
        fail ("Unknown pointer");
    }

    Cell(*) v = Array_Single(a);
    if (
        QUOTE_BYTE(v) == UNQUOTED_1
        or QUOTE_BYTE(v) == QUASI_2
        or QUOTE_BYTE(v) == ISOTOPE_0
    ){
        fail ("rebUNQUOTING()/rebU() can only unquote QUOTED! values");
    }

    Unquotify(v, 1);
    return a;
}


//
//  rebRELEASING: RL_API
//
// Convenience tool for making "auto-release" form of values.  They will only
// exist for one API call.  They will be automatically rebRelease()'d when
// they are seen (or even if they are not seen, if there is a failure on that
// call it will still process the va_list in order to release these handles)
//
const REBINS *RL_rebRELEASING(REBVAL *v)
{
    ENTER_API;

    if (v == nullptr)
        return nullptr;

    if (not Is_Api_Value(v))
        fail ("Cannot apply rebR() to non-API value");

    Array(*) a = Singular_From_Cell(v);
    if (Get_Subclass_Flag(API, a, RELEASE))
        fail ("Cannot apply rebR() more than once to the same API value");

    Set_Subclass_Flag(API, a, RELEASE);
    return cast(REBINS*, a);
}


//
//  rebINLINE: RL_API
//
// This will splice an array, single value, or no-op into the execution feed.
//
const void *RL_rebINLINE(const REBVAL *v)
{
    ENTER_API;

    Array(*) a = Alloc_Singular(
        FLAG_FLAVOR(INSTRUCTION_SPLICE) | NODE_FLAG_MANAGED
    );
    Clear_Series_Flag(a, MANAGED);  // lying avoided manuals tracking
    if (not (IS_BLOCK(v) or Is_Quoted(v) or IS_BLANK(v)))
        fail ("rebINLINE() requires argument to be a BLOCK!/QUOTED!/BLANK!");

    Copy_Cell(Array_Single(a), v);

    return cast(REBINS*, a);
}


//
//  rebRUN: RL_API
//
// If a REBVAL* holds an isotope, this will convert it to a regular action
// so that it runs inline.  If it were ^META'd then it would produce a
// QUASI!-action, that would just evaluate to an isotope.  Something like a
// rebREIFY would also work, but it would not do type checking.
//
const REBINS *RL_rebRUN(const void *p)
{
    ENTER_API;

    if (p == nullptr)
        fail ("rebRUN() received nullptr");

    Array(*) a;

    switch (Detect_Rebol_Pointer(p)) {
      case DETECTED_AS_SERIES: {
        a = m_cast(Array(*), cast(Array(const*), p));
        if (Not_Subclass_Flag(API, a, RELEASE))
            fail ("Can't quote instructions (besides rebR())");
        break; }

      case DETECTED_AS_CELL: {
        Value(const*) at = cast(Value(const*), p);
        if (Is_Nulled(at))
            fail ("rebRUN() received null cell");

        Value(*) v = Copy_Cell(Alloc_Value(), at);
        a = Singular_From_Cell(v);
        Set_Subclass_Flag(API, a, RELEASE);
        break; }

      default:
        fail ("Unknown pointer");
    }

    Value(*) v = VAL(Array_Single(a));
    if (Is_Activation(v))
        mutable_QUOTE_BYTE(v) = UNQUOTED_1;
    else if (not IS_FRAME(v))
        fail ("rebRUN() only accepts FRAME! or actions (aka FRAME! isotopes)");

    return a;
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
    ENTER_API;

    assert(Is_Api_Value(v));

    Array(*) a = Singular_From_Cell(v);
    assert(Get_Series_Flag(a, ROOT));

    if (Get_Series_Flag(a, MANAGED))
        fail ("Attempt to rebManage() a handle that's already managed.");

    Set_Series_Flag(a, MANAGED);
    Link_Api_Handle_To_Level(a, TOP_LEVEL);

    return v;
}


//
//  rebUnmanage: RL_API
//
// This converts an API handle value to indefinite lifetime.
//
void RL_rebUnmanage(void *p)
{
    ENTER_API;

    Node* n = NOD(p);
    if (Is_Node_A_Stub(n))
        fail ("rebUnmanage() not yet implemented for rebMalloc() data");

    REBVAL *v = cast(REBVAL*, n);
    assert(Is_Api_Value(v));

    Array(*) a = Singular_From_Cell(v);
    assert(Get_Series_Flag(a, ROOT));

    if (Not_Series_Flag(a, MANAGED))
        fail ("Attempt to rebUnmanage() a handle with indefinite lifetime.");

    // It's not safe to convert the average series that might be referred to
    // from managed to unmanaged, because you don't know how many references
    // might be in cells.  But the singular array holding API handles has
    // pointers to its cell being held by client C code only.  It's at their
    // own risk to do this, and not use those pointers after a free.
    //
    Clear_Series_Flag(a, MANAGED);
    Unlink_Api_Handle_From_Level(a);

    Trash_Pointer_If_Debug(a->link.trash);
    Trash_Pointer_If_Debug(a->misc.trash);
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
    ENTER_API;

    if (not v)
        return;  // less rigorous, but makes life easier for C programmers

    if (not Is_Api_Value(v))
        panic ("Attempt to rebRelease() a non-API handle");

    Free_Value(m_cast(REBVAL*, v));
}


//
//  rebZdeflateAlloc: RL_API
//
// Variant of rebDeflateAlloc() which adds a zlib envelope...which is a 2-byte
// header and 32-bit ADLER32 CRC at the tail.
//
// !!! TBD: Clients should be able to use a plain Rebol call to ZDEFLATE and
// be able to get the data back using something like rebRepossess.  That
// would eliminate this API.
//
void *RL_rebZdeflateAlloc(
    size_t *out_len,
    const void *input,
    size_t in_len
){
    ENTER_API;

    return Compress_Alloc_Core(out_len, input, in_len, SYM_ZLIB);
}


//
//  rebZinflateAlloc: RL_API
//
// Variant of rebInflateAlloc() which assumes a zlib envelope...checking for
// the 2-byte header and verifying the 32-bit ADLER32 CRC at the tail.
//
// !!! TBD: Clients should be able to use a plain Rebol call to ZINFLATE and
// be able to get the data back using something like rebRepossess.  That
// would eliminate this API.
//
void *RL_rebZinflateAlloc(
    size_t *len_out,
    const void *input,
    size_t len_in,
    int max
){
    ENTER_API;

    return Decompress_Alloc_Core(len_out, input, len_in, max, SYM_ZLIB);
}


// !!! Although it is very much the goal to get all OS-specific code out of
// the core (including the API), this particular hook is extremely useful to
// have available to all clients.  It might be done another way (e.g. by
// having hosts HIJACK the FAIL native with an adaptation that processes
// integer arguments).  But for now, stick it in the API just to get the
// wide availability.
//
#if TO_WINDOWS
    #undef IS_ERROR // windows has its own meaning for this.
    #define WIN32_LEAN_AND_MEAN  // trim down the Win32 headers
    #include <windows.h>
#else
    #include <errno.h>
    #define MAX_POSIX_ERROR_LEN 1024
#endif

//
//  rebError_OS: RL_API
//
// Produce an error from an OS error code, by asking the OS for textual
// information it knows internally from its database of error strings.
//
// Note that error codes coming from WSAGetLastError are the same as codes
// coming from GetLastError in 32-bit and above Windows:
//
// https://stackoverflow.com/q/15586224/
//
// !!! Should not be in core, but extensions need a way to trigger the
// common functionality one way or another.
//
REBVAL *RL_rebError_OS(int errnum)  // see also convenience macro rebFail_OS()
{
    ENTER_API;

    Context(*) error;

  #if TO_WINDOWS
    if (errnum == 0)
        errnum = GetLastError();

    WCHAR *lpMsgBuf;  // FormatMessage writes allocated buffer address here

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

        error = Error(SYM_0, SYM_0, message, rebEND);
        rebRelease(message);
    }
  #elif defined(USE_STRERROR_NOT_STRERROR_R)
    char *shared = strerror(errnum);  // not thread safe, deprecated
    error = Error_User(shared);
  #else
    // strerror() is not thread-safe, but strerror_r is. Unfortunately, at
    // least in glibc, there are two different protocols for strerror_r(),
    // depending on whether you are using the POSIX-compliant implementation
    // or the GNU implementation.
    //
    // It was once possible to tell the difference between which protocol
    // you were using based on this test:
    //
    //   The XSI-compliant version of strerror_r() is provided if:
    //   (_POSIX_C_SOURCE >= 200112L || _XOPEN_SOURCE >= 600) && ! _GNU_SOURCE
    //   Otherwise, the GNU-specific version is provided.
    //
    // Sadly, in GCC 9.3.0 using C99, _GNU_SOURCE is defined but the POSIX
    // definition (int returning) is in effect.  Other libraries like musl
    // seem to get this #define tapdance wrong as well.
    //
    // There are many attempted workarounds on the Internet (trying to use the
    // lower-level `sys_errlist` directly--which may not include all errors,
    // or using function overloading that only works on C++).  This takes a
    // different tactic in pure C by capturing either result cast to intptr_t.

    char buf[MAX_POSIX_ERROR_LEN];
    buf[0] = cast(char, 255);  // never valid in UTF-8 sequences
    int old_errno = errno;
    intptr_t r = cast(intptr_t, strerror_r(errnum, buf, MAX_POSIX_ERROR_LEN));

    // !!! TCC appears to use the `int` returning form of strerror_r().  But
    // it appears to return a random positive or negative value.  It simply
    // appears to be broken.  More research would be needed, but we can just
    // give up an go with strerror.  Leaving in the call to strerror_r() to
    // show that it's there...and it links in TCC.
    //
  #if defined(__TINYC__)
    r = cast(intptr_t, strerror(errnum));
  #endif

    int new_errno = errno;

    if (r == -1 or new_errno != old_errno) {
        //
        // errno was changed, so probably the return value is just -1 or
        // something else that doesn't provide info, and errno is the error.
        //
        assert(false);
        error = Error_User("Error during strerror_r call");  // w/new_errno?
    }
    else if (r == 0) {
        //
        // Quoting glibc's strerror_r manpage: "The XSI-compliant strerror_r()
        // function returns 0 on success. On error, a (positive) error number
        // is returned (since glibc 2.13), or -1 is returned and errno is set
        // to indicate the error (glibc versions before 2.13)."  GNU version
        // always succeds and should never return 0 (a null char*).
        //
        // Documentation isn't clear on whether the buffer is terminated if
        // the message is too long, or ERANGE always returned.  Terminate.
        //
        buf[MAX_POSIX_ERROR_LEN - 1] = '\0';
        error = Error_User(buf);
    }
    else if (r == EINVAL)  // documented result from POSIX strerror_r
        error = Error_User("EINVAL: bad errno passed to strerror_r()");
    else if (r == ERANGE)  // documented result from POSIX strerror_r
        error = Error_User("ERANGE: insufficient buffer size for error");
    else if (r == cast(intptr_t, buf)) {
        //
        // The POSIX version gives us our error back as a pointer if it
        // filled the buffer successfully.  Sanity check that's what happened.
        //
        if (buf[0] == cast(char, 255)) {
            assert(false);
            error = Error_User("Buffer not correctly updated by strerror_r");
        }
        else
            error = Error_User(buf);
    }
    else if (  // small + or - numbers very unlikely to be string buffer
        (r > 0 and r < 256)
        or (r < 0 and - r < 256)
    ){
        assert(false);
        error = Error_User("Unknown POSIX strerror_r error result code");
    }
    else {
        // The GNU version never fails, but may return an immutable string
        // instead of filling the buffer. Unknown errors get an
        // "unknown error" message.  The result is always null terminated.
        //
        // (This is the risky part, if `r` is not a valid pointer but some
        // weird large int return result from POSIX strerror_r.)
        //
        error = Error_User(cast(const char*, r));
    }
  #endif

    return Init_Error(Alloc_Value(), error);
}


//
//  api-transient: native [
//
//  {Produce an API handle pointer (returned via INTEGER!) for a value}
//
//      return: "Heap address of the autoreleasing (rebR()) API handle"
//          [integer!]
//      value [<opt> any-value!]
//  ]
//
DECLARE_NATIVE(api_transient)
{
    INCLUDE_PARAMS_OF_API_TRANSIENT;

    REBVAL *v = Copy_Cell(Alloc_Value(), ARG(value));
    rebUnmanage(v);  // has to survive the API-TRANSIENT's frame
    Array(*) a = Singular_From_Cell(v);
    Set_Subclass_Flag(API, a, RELEASE);

    // Regarding adddresses in WASM:
    //
    // "In wasm32, address operands and offset attributes have type i32"
    // "In wasm64, address operands and offsets have type i64"
    //
    // "Note that the value types i32 and i64 are not inherently signed or
    //  unsigned. The interpretation of these types is determined by
    //  individual operators."
    //
    // :-/  Well, which is it?  R3-Alpha integers were signed 64-bit, Ren-C is
    // targeting arbitrary precision...use signed as status quo for now.
    //
    return Init_Integer(level_->out, cast(intptr_t, a));  // or, `uintptr_t` ??
}


// There's no particularly ideal place to define the instance of the nullptr
// shim when building with C++98.  At time of writing (in 2021) this is needed
// for the Haiku build and it's ancient compiler (GCC 2.95, released 2001)
// which can build neither C99 nor C++11.
//
#if defined(REBOL_USE_NULLPTR_SHIM)
    const nullptr_t nullptr = {};  // global instance behind faked nullptr
#endif



#include "sys-ext.h"

//
//  rebCollateExtension_internal: RL_API
//
// This routine gathers information which can be called to bring an extension
// to life.  It does not itself decompress any of the data it is given, or run
// any startup code.  This allows extensions which are built into an
// executable to do deferred loading.
//
// !!! For starters, this just returns an array of the values...but this is
// the same array that would be used as the Phase_Details() of an action.  So
// it could return a generator ACTION!.
//
// !!! It may be desirable to separate out the module header and go ahead and
// get that loaded as part of this process, in order to allow queries of the
// dependencies and other information.  That might suggest returning a block
// with an OBJECT! header and an ACTION! to run to do the load?  Or maybe
// a HANDLE! which can be passed as a module body with a spec?
//
// !!! If a DLL gets loaded, it's possible these pointers could be unloaded
// if the information were not used immediately or it otherwise was not run.
// This has to be considered in the unloading mechanics.
//
REBVAL *RL_rebCollateExtension_internal(
    const unsigned char *script_compressed,
    size_t script_compressed_size,
    int script_num_codepoints,
    void *cfuncs,  // Dispatcher* or Intrinsic* (not in API either way)
    int cfuncs_len
){
    Array(*) a = Make_Array(IDX_COLLATOR_MAX);  // details
    Set_Series_Len(a, IDX_COLLATOR_MAX);

    Init_Handle_Cdata(
        Array_At(a, IDX_COLLATOR_SCRIPT),
        m_cast(Byte*, script_compressed),  // !!! by contract, don't change!
        script_compressed_size
    );
    Init_Integer(
        Array_At(a, IDX_COLLATOR_SCRIPT_NUM_CODEPOINTS),
        script_num_codepoints
    );
    Init_Handle_Cdata(
        Array_At(a, IDX_COLLATOR_CFUNCS),
        cfuncs,
        cfuncs_len
    );

    return Init_Block(Alloc_Value(), a);
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
