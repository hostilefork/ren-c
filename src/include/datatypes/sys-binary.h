//
//  File: %sys-binary.h
//  Summary: {Definitions for binary series}
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2019 Ren-C Open Source Contributors
// REBOL is a trademark of REBOL Technologies
//
// See README.md and CREDITS.md for more information
//
// Licensed under the Lesser GPL, Version 3.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// https://www.gnu.org/licenses/lgpl-3.0.html
//
//=////////////////////////////////////////////////////////////////////////=//
//
// A BINARY! value holds a byte-size series.  The bytes may be arbitrary, or
// if the series has SERIES_FLAG_IS_STRING then modifications are constrained
// to only allow valid UTF-8 data.  Such binary "views" are possible due to
// things like the AS operator (`as binary! "abc"`).
//
// R3-Alpha used a binary series to hold the data for BITSET!.  See notes in
// %sys-bitset.h regarding this usage (which has a "negated" bit in the
// MISC() field).
//
//=//// NOTES /////////////////////////////////////////////////////////////=//
//
// * Since strings use MISC() and LINK() for various features, and binaries
//   can be "views" on string series, this means that generally speaking a
//   binary series can't use MISC() and LINK() for its own purposes.  (For
//   the moment, typesets cannot be aliased, so you can't get into a situation
//   like `as text! as binary! make bitset! [...]`)


#if CPLUSPLUS_11  // !!! Make fancier checks, as with SER() and ARR()
    inline static Binary(*) BIN(void *p)
        { return reinterpret_cast<Binary(*)>(p); }
    inline static Binary(const*) BIN(const void *p)
        { return reinterpret_cast<Binary(const*)>(p); }
#else
    #define BIN(p) cast(Binary(*), (p))
#endif


//=//// BINARY! SERIES ////////////////////////////////////////////////////=//

inline static Byte* BIN_AT(const_if_c Binary(*) bin, REBLEN n)
  { return SER_AT(Byte, bin, n); }

inline static Byte* BIN_HEAD(const_if_c Binary(*) bin)
  { return SER_HEAD(Byte, bin); }

inline static Byte* BIN_TAIL(const_if_c Binary(*) bin)
  { return SER_TAIL(Byte, bin); }

inline static Byte* BIN_LAST(const_if_c Binary(*) bin)
  { return SER_LAST(Byte, bin); }

#if CPLUSPLUS_11
    inline static const Byte* BIN_AT(Binary(const*) bin, REBLEN n)
      { return SER_AT(const Byte, bin, n); }

    inline static const Byte* BIN_HEAD(Binary(const*) bin)
      { return SER_HEAD(const Byte, bin); }

    inline static const Byte* BIN_TAIL(Binary(const*) bin)
      { return SER_TAIL(const Byte, bin); }

    inline static const Byte* BIN_LAST(Binary(const*) bin)
      { return SER_LAST(const Byte, bin); }
#endif

inline static REBLEN BIN_LEN(Binary(const*) s) {
    assert(SER_WIDE(s) == 1);
    return SER_USED(s);
}

inline static void TERM_BIN(Binary(*) s) {
    *BIN_TAIL(s) = '\0';
}

inline static void TERM_BIN_LEN(Binary(*) s, REBLEN len) {
    assert(SER_WIDE(s) == 1);
    SET_SERIES_USED(s, len);
    *BIN_TAIL(s) = '\0';
}

// Make a byte series of length 0 with the given capacity (plus 1, to permit
// a '\0' terminator).  Binaries are given enough capacity to have a null
// terminator in case they are aliased as UTF-8 later, e.g. `as word! binary`,
// since it could be costly to give them that capacity after-the-fact.
//
inline static Binary(*) Make_Binary_Core(REBLEN capacity, Flags flags)
{
    assert(FLAVOR_BYTE(flags) == 0);  // shouldn't pass in a flavor

    Raw_Binary* s = Make_Series(Binary,
        capacity + 1,
        FLAG_FLAVOR(BINARY) | flags
    );
  #if DEBUG_POISON_SERIES_TAILS
    *SER_HEAD(Byte, s) = BINARY_BAD_UTF8_TAIL_BYTE;  // reserve for '\0'
  #endif
    return BIN(s);
}

#define Make_Binary(capacity) \
    Make_Binary_Core(capacity, SERIES_FLAGS_NONE)


//=//// BINARY! VALUES ////////////////////////////////////////////////////=//

inline static Binary(const*) VAL_BINARY(NoQuote(Cell(const*)) v) {
    assert(CELL_HEART(v) == REB_BINARY);
    return BIN(VAL_SERIES(v));
}

#define VAL_BINARY_ENSURE_MUTABLE(v) \
    m_cast(Binary(*), VAL_BINARY(ENSURE_MUTABLE(v)))

#define VAL_BINARY_KNOWN_MUTABLE(v) \
    m_cast(Binary(*), VAL_BINARY(KNOWN_MUTABLE(v)))


inline static const Byte* VAL_BINARY_SIZE_AT(
    Option(Size*) size_at_out,
    NoQuote(Cell(const*)) v
){
    Binary(const*) bin = VAL_BINARY(v);
    REBIDX i = VAL_INDEX_RAW(v);
    Size size = BIN_LEN(bin);
    if (i < 0 or i > cast(REBIDX, size))
        fail (Error_Index_Out_Of_Range_Raw());
    if (size_at_out)
        *unwrap(size_at_out) = size - i;
    return BIN_AT(bin, i);
}

#define VAL_BINARY_SIZE_AT_ENSURE_MUTABLE(size_out,v) \
    m_cast(Byte*, VAL_BINARY_SIZE_AT((size_out), ENSURE_MUTABLE(v)))

#define VAL_BINARY_AT(v) \
    VAL_BINARY_SIZE_AT(nullptr, (v))

#define VAL_BINARY_AT_ENSURE_MUTABLE(v) \
    m_cast(Byte*, VAL_BINARY_AT(ENSURE_MUTABLE(v)))

#define VAL_BINARY_AT_KNOWN_MUTABLE(v) \
    m_cast(Byte*, VAL_BINARY_AT(KNOWN_MUTABLE(v)))

#define Init_Binary(out,bin) \
    Init_Series_Cell((out), REB_BINARY, (bin))

#define Init_Binary_At(out,bin,offset) \
    Init_Series_Cell_At((out), REB_BINARY, (bin), (offset))


//=//// GLOBAL BINARIES //////////////////////////////////////////////////=//

#define EMPTY_BINARY \
    Root_Empty_Binary

#define BYTE_BUF TG_Byte_Buf
