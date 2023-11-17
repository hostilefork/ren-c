//
//  File: %sys-node.h
//  Summary: {Convenience routines for the Node "superclass" structure}
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2017 Ren-C Open Source Contributors
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
// This provides some convenience routines that require more definitions than
// are available when %sys-rebnod.h is being processed.  (e.g. Value(*),
// Series(*), Level(*)...)
//
// See %sys-rebnod.h for what a "Node" means in this context.
//


#define NODE_BYTE(p) \
    *cast(const Byte*, ensure(const Node*, p))

#ifdef NDEBUG
    #define Is_Free_Node(p) \
        (did (*cast(const Byte*, (p)) & NODE_BYTEMASK_0x40_STALE))
#else
    inline static bool Is_Free_Node(const void *p) {
        Byte first = *cast(const Byte*, p);  // NODE_BYTE asserts on free!

        if (not (first & NODE_BYTEMASK_0x40_STALE))
            return false;  // byte access defeats strict alias

        assert(first == FREED_SERIES_BYTE or first == END_SIGNAL_BYTE);
        return true;
    }
#endif


//=//// MEMORY ALLOCATION AND FREEING MACROS //////////////////////////////=//
//
// Rebol's internal memory management is done based on a pooled model, which
// use Try_Alloc_Mem() and Free_Mem() instead of calling malloc directly.
// (Comments on those routines explain why this was done--even in an age of
// modern thread-safe allocators--due to Rebol's ability to exploit extra
// data in its pool block when a series grows.)
//
// Since Free_Mem() requires callers to pass in the size of the memory being
// freed, it can be tricky.  These macros are modeled after C++'s new/delete
// and new[]/delete[], and allocations take either a type or a type and a
// length.  The size calculation is done automatically, and the result is cast
// to the appropriate type.  The deallocations also take a type and do the
// calculations.
//
// In a C++11 build, an extra check is done to ensure the type you pass in a
// FREE or FREE_N lines up with the type of pointer being freed.
//

#define TRY_ALLOC(t) \
    cast(t *, Try_Alloc_Mem(sizeof(t)))

#define TRY_ALLOC_ZEROFILL(t) \
    cast(t *, memset(ALLOC(t), '\0', sizeof(t)))

#define TRY_ALLOC_N(t,n) \
    cast(t *, Try_Alloc_Mem(sizeof(t) * (n)))

#define TRY_ALLOC_N_ZEROFILL(t,n) \
    cast(t *, memset(TRY_ALLOC_N(t, (n)), '\0', sizeof(t) * (n)))

#if CPLUSPLUS_11
    #define FREE(t,p) \
        do { \
            static_assert( \
                std::is_same<decltype(p), std::add_pointer<t>::type>::value, \
                "mismatched FREE type" \
            ); \
            Free_Mem(p, sizeof(t)); \
        } while (0)

    #define FREE_N(t,n,p)   \
        do { \
            static_assert( \
                std::is_same<decltype(p), std::add_pointer<t>::type>::value, \
                "mismatched FREE_N type" \
            ); \
            Free_Mem(p, sizeof(t) * (n)); \
        } while (0)
#else
    #define FREE(t,p) \
        Free_Mem((p), sizeof(t))

    #define FREE_N(t,n,p)   \
        Free_Mem((p), sizeof(t) * (n))
#endif


#define Is_Node_A_Cell(n) \
    (did (NODE_BYTE(n) & NODE_BYTEMASK_0x01_CELL))

#define Is_Node_A_Stub(n) \
    (not Is_Node_A_Cell(n))


// Allocate a node from a pool.  Returned node will not be zero-filled, but
// the header will have SERIES_FLAG_FREE set when it is returned (client is
// responsible for changing that if they plan to enumerate the pool and
// distinguish free nodes from non-free ones.)
//
// All nodes are 64-bit aligned.  This way, data allocated in nodes can be
// structured to know where legal 64-bit alignment points would be.  This
// is required for correct functioning of some types.  (See notes on
// alignment in %sys-rebval.h.)
//
inline static void *Try_Alloc_Pooled(PoolId pool_id)
{
    Pool* pool = &Mem_Pools[pool_id];
    if (not pool->first) {  // pool has run out of nodes
        if (not Try_Fill_Pool(pool))  // attempt to refill it
            return nullptr;
    }

  #if !defined(NDEBUG)
    if (PG_Fuzz_Factor != 0) {
        if (PG_Fuzz_Factor < 0) {
            ++PG_Fuzz_Factor;
            if (PG_Fuzz_Factor == 0)
                return nullptr;
        }
        else if ((TG_tick % 10000) <= cast(REBLEN, PG_Fuzz_Factor)) {
            PG_Fuzz_Factor = 0;
            return nullptr;
        }
    }
  #endif

    assert(pool->first);

    PoolUnit* unit = pool->first;

    pool->first = unit->next_if_free;
    if (unit == pool->last)
        pool->last = nullptr;

    pool->free--;

  #if DEBUG_MEMORY_ALIGN
    if (cast(uintptr_t, unit) % sizeof(REBI64) != 0) {
        printf(
            "Pool Unit address %p not aligned to %d bytes\n",
            cast(void*, unit),
            cast(int, sizeof(REBI64))
        );
        printf("Pool Unit address is %p and pool-first is %p\n",
            cast(void*, pool),
            cast(void*, pool->first)
        );
        panic (unit);
    }
  #endif

    // !!! Review this, as not all pools store "nodes".
    //
    assert(Is_Free_Node(cast(Node*, unit)));  // client must make non-free
    return cast(void*, unit);
}


inline static void *Alloc_Pooled(PoolId pool_id) {
    void *node = Try_Alloc_Pooled(pool_id);
    if (node)
        return node;

    Pool* pool = &Mem_Pools[pool_id];
    fail (Error_No_Memory(pool->wide * pool->num_units_per_segment));
}

#define Alloc_Stub() ( \
    (GC_Ballast -= sizeof(Stub)) <= 0 ? SET_SIGNAL(SIG_RECYCLE) : NOOP, \
    Alloc_Pooled(STUB_POOL))  // won't pass SER() yet, don't cast it


// Free a node, returning it to its pool.  Once it is freed, its header will
// have SERIES_FLAG_FREE...which will identify the node as not in use to anyone
// who enumerates the nodes in the pool (such as the garbage collector).
//
inline static void Free_Pooled(PoolId pool_id, void* p)
{
  #if DEBUG_MONITOR_SERIES
    if (p == PG_Monitor_Node_Debug) {
        printf(
            "Freeing series %p on tick #%d\n", p,
            cast(int, TG_tick)
        );
        fflush(stdout);
    }
  #endif

    PoolUnit* unit = cast(PoolUnit*, p);

    mutable_FIRST_BYTE(unit->headspot) = FREED_SERIES_BYTE;

    Pool* pool = &Mem_Pools[pool_id];

  #ifdef NDEBUG
    unit->next_if_free = pool->first;
    pool->first = unit;
  #else
    // !!! In R3-Alpha, the most recently freed node would become the first
    // node to hand out.  This is a simple and likely good strategy for
    // cache usage, but makes the "poisoning" nearly useless.
    //
    // This code was added to insert an empty segment, such that this node
    // won't be picked by the next Alloc_Pooled.  That enlongates the poisonous
    // time of this area to catch stale pointers.  But doing this in the
    // debug build only creates a source of variant behavior.

    bool out_of_memory = false;

    if (not pool->last) {  // Fill pool if empty
        if (not Try_Fill_Pool(pool))
            out_of_memory = true;
    }

    if (out_of_memory) {
        //
        // We don't want Free_Node to fail with an "out of memory" error, so
        // just fall back to the release build behavior in this case.
        //
        unit->next_if_free = pool->first;
        pool->first = unit;
    }
    else {
        assert(pool->last);

        pool->last->next_if_free = unit;
        pool->last = unit;
        unit->next_if_free = nullptr;
    }
  #endif

    pool->free++;
}


//=//// POINTER DETECTION (UTF-8, SERIES, FREED SERIES, END) //////////////=//
//
// Ren-C's "nodes" (Cell and Stub derivatives) all have a platform-pointer
// sized header of bits, which is constructed using byte-order-sensitive bit
// flags (see FLAG_LEFT_BIT and related definitions for how those work).
//
// The values for the bits were chosen carefully, so that the leading byte of
// Cell and Stub could be distinguished from the leading byte of a UTF-8
// string, as well as from each other.  This is taken advantage of in the API.
//
// During startup, Assert_Pointer_Detection_Working() checks invariants that
// make this routine able to work.
//

enum PointerDetectEnum {
    DETECTED_AS_UTF8 = 0,

    DETECTED_AS_END = 1,  // may be in a cell, or a rebEND signal (char* align)
    DETECTED_AS_SERIES = 2,
    DETECTED_AS_CELL = 3
};

typedef enum PointerDetectEnum PointerDetect;

inline static PointerDetect Detect_Rebol_Pointer(const void *p)
{
    const Byte* bp = cast(const Byte*, p);

    if (*bp == END_SIGNAL_BYTE) {  // reserved illegal UTF-8 byte 192
        assert(bp[1] == '\0');  // rebEND C string "\x81", terminates with '\0'
        return DETECTED_AS_END;
    }

    if (
        (*bp & (NODE_BYTEMASK_0x80_NODE | NODE_BYTEMASK_0x40_STALE))
        == NODE_BYTEMASK_0x80_NODE  // e.g. leading bit pattern is 10xxxxxx
    ){
        // In UTF-8 these are all continuation bytes, so not a legal way to
        // start a string.  We leverage that to distinguish cells and series.
        //
        if (*bp & NODE_BYTEMASK_0x01_CELL)
            return DETECTED_AS_CELL;

        // Clients of this function should not be passing in series in mid-GC.
        // (PROBE uses it, so that throws a wrench into this check.  Review.)
        //
        /*assert(not (*bp & NODE_BYTEMASK_0x10_MARKED));*/

        return DETECTED_AS_SERIES;
    }

    // Note: technically there are some internal states that overlap with UTF-8
    // range, e.g. when a cell is marked "stale" in the output location of
    // a level.  Such states are not supposed to be leaked to where clients of
    // this routine would be concerned about them.
    //
    return DETECTED_AS_UTF8;
}
