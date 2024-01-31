//
//  File: %struct-feed.h
//  Summary: "Feed structure definitions preceding %tmp-internals.h"
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012-2020 Ren-C Open Source Contributors
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
// This declares the structure used by feeds, for use in other structs.
// See %sys-feed.h for a higher-level description.
//


#define FEED_MASK_DEFAULT NODE_FLAG_NODE

#define FEED_FLAG_0_IS_TRUE \
    FLAG_LEFT_BIT(0)
STATIC_ASSERT(FEED_FLAG_0_IS_TRUE == NODE_FLAG_NODE);

#define FEED_FLAG_1_IS_FALSE \
    FLAG_LEFT_BIT(1)
STATIC_ASSERT(FEED_FLAG_1_IS_FALSE == NODE_FLAG_FREE);


//=//// FEED_FLAG_DEFERRING_ENFIX /////////////////////////////////////////=//
//
// Defer notes when there is a pending enfix operation that was seen while an
// argument was being gathered, that decided not to run yet.  It will run only
// if it turns out that was the last argument that was being gathered...
// otherwise it will error.
//
//    if 1 [2] then [3]     ; legal
//    if 1 then [2] [3]     ; **error**
//    if (1 then [2]) [3]   ; legal, arguments weren't being gathered
//
// This flag is marked on a parent frame by the argument fulfillment the
// first time it sees a left-deferring operation like a THEN or ELSE, and is
// used to decide whether to report an error or not.
//
// (At one point, mechanics were added to make the second case not an
// error.  However, this gave the evaluator complex properties of re-entry
// that made its behavior harder to characterize.  This means that only a
// flag is needed, vs complex marking of a parameter to re-enter eval with.)
//
#define FEED_FLAG_DEFERRING_ENFIX \
    FLAG_LEFT_BIT(2)


//=//// FEED_FLAG_3 ///////////////////////////////////////////////////////=//
//
#define FEED_FLAG_3 \
    FLAG_LEFT_BIT(3)


//=//// FEED_FLAG_NO_LOOKAHEAD ////////////////////////////////////////////=//
//
// Infix functions may (depending on the #tight or non-tight parameter
// acquisition modes) want to suppress further infix lookahead while getting
// a function argument.  This precedent was started in R3-Alpha, where with
// `1 + 2 * 3` it didn't want infix `+` to "look ahead" past the 2 to see the
// infix `*` when gathering its argument, that was saved until the `1 + 2`
// finished its processing.
//
#define FEED_FLAG_NO_LOOKAHEAD \
    FLAG_LEFT_BIT(4)


//=//// FEED_FLAG_5 ///////////////////////////////////////////////////////=//
//
#define FEED_FLAG_5 \
    FLAG_LEFT_BIT(5)


//=//// FEED_FLAG_TOOK_HOLD ///////////////////////////////////////////////=//
//
// If a feed takes SERIES_INFO_HOLD on an array it is enumerating, it has to
// remember that it did so it can release it when it is done processing.
// Note that this has to be a flag on the feed, not the level--as a feed can
// be shared among many levels.
//
// !!! This is undermined by work in stackless, where a single bit is not
// sufficient since the stacks do not cleanly unwind:
//
// https://forum.rebol.info/t/1317
//
#define FEED_FLAG_TOOK_HOLD \
    FLAG_LEFT_BIT(6)


//=//// FEED_FLAG_NEEDS_SYNC ///////////////////////////////////////////////=//
//
// Originally all feeds were "prefetched" and a value was sitting waiting to
// be used.  But with variadic feeds, requiring this meant that the scanner
// had to be run before the first fetch occurred--if the first variadic item
// was a string.  This was especially problematic because it meant an error
// could occur in the scanner before the level that would be receiving the
// feed was pushed.  Because that meant the error would happen before the
// exception handling in the Trampoline could be set up.
//
// So now this flag is set in Prep_Feed_Common(), and can be checked by
// accessors to make sure you don't use the pointer until you've called the
// fetch at least once.
//
#define FEED_FLAG_NEEDS_SYNC \
    FLAG_LEFT_BIT(7)


//=//// BITS 8...15 ARE CURRENTLY UNUSED ///////////////////////////////////=//

// These had once been used for a "quoting byte", but that feature was not
// used enough to justify its complexity:
//
// https://forum.rebol.info/t/1050/4



//=//// FEED_FLAG_CONST ///////////////////////////////////////////////////=//
//
// The user is able to flip the constness flag explicitly with the CONST and
// MUTABLE functions explicitly.  However, if a feed has FEED_FLAG_CONST,
// the system imposes it's own constness as part of the "wave of evaluation"
// it does.  While this wave starts out initially with levels demanding const
// marking, if it ever gets flipped, it will have to encounter an explicit
// CONST marking on a value before getting flipped back.
//
#define FEED_FLAG_CONST \
    FLAG_LEFT_BIT(30)
STATIC_ASSERT(FEED_FLAG_CONST == CELL_FLAG_CONST);


#define Get_Feed_Flag(f,name) \
    ((ensure(Feed*, (f))->flags.bits & FEED_FLAG_##name) != 0)

#define Not_Feed_Flag(f,name) \
    ((ensure(Feed*, (f))->flags.bits & FEED_FLAG_##name) == 0)

#define Set_Feed_Flag(f,name) \
    (ensure(Feed*, (f))->flags.bits |= FEED_FLAG_##name)

#define Clear_Feed_Flag(f,name) \
    ensure(Feed*, (f))->flags.bits &= ~FEED_FLAG_##name


#define CORRUPT_INDEX ((REBLEN)(-3))

typedef struct FeedStruct Feed;

struct FeedStruct {
    union HeaderUnion flags;  // quoting level included

    // This is the "prefetched" value being processed.  Entry points to the
    // evaluator must load a first value pointer into it...which for any
    // successive evaluations will be updated via Fetch_Next_In_Feed()--which
    // retrieves values from arrays or va_lists.  But having the caller pass
    // in the initial value gives the option of that value being out of band.
    //
    // (Hence if one has the series `[[a b c] [d e]]` it would be possible to
    // have an independent WORD! such as `append` and NOT insert it in the
    // series, yet get the effect of `append [a b c] [d e]`.  This only
    // works for one value, but is a convenient no-cost trick for apply-like
    // situations...as insertions usually have to "slide down" the values in
    // the series and may also need to perform alloc/free/copy to expand.
    // It also is helpful since in C, variadic functions must have at least
    // one non-variadic parameter...and one might want that non-variadic
    // parameter to be blended in with the variadics.)
    //
    // !!! Review impacts on debugging; e.g. a debug mode should hold onto
    // the initial value in order to display full error messages.
    //
    const void *p;  // nullptr if feed is finished (Is_Feed_At_End())
    // Note: The specifier comes from FEED_SPECIFIER()

    //=//// ^-- be sure above fields align cells below to 64-bits --v /////=//
    // (two intptr_t sized things should take care of it on both 32/64-bit) //

    // Sometimes the feed can be advanced without keeping track of the
    // last cell.  And sometimes the last cell lives in an array that is
    // being held onto and read only, so its pointer is guaranteed to still
    // be valid after a fetch.  But there are cases where values are being
    // read from transient sources that disappear as they go...if that is
    // the case, and lookback is needed, it is written into this cell.
    //
    Element lookback;

    // When feeding cells from a variadic, those cells may wish to mutate the
    // value in some way... e.g. to add a quoting level.  Rather than
    // complicate the evaluator itself with flags and switches, each feed
    // has a holding cell which can optionally be used as the pointer that
    // is returned by Fetch_Next_in_Feed(), where arbitrary mutations can
    // be applied without corrupting the value they operate on.
    //
    Element fetched;

    // Feed sources are expresesd as Stub-sized "splice" units.  This is big
    // enough for a REBVAL to hold an array and an index, but it also lets
    // you point to other singulars that can hold arrays and indices.
    //
    // If values are being sourced from an array, this holds the pointer to
    // that array.  By knowing the array it is possible for error and debug
    // messages to reach backwards and present more context of where the
    // error is located.  The index is of the *next* item in the array to
    // fetch for processing.
    //
    // If the feed is for a C va_list, the singular holds a pointer to that,
    // and there is no index.
    //
    // This is used for relatively bound words to be looked up to become
    // specific.  Typically the specifier is extracted from the payload of the
    // ANY-ARRAY! value that provided the source.array for the call to DO.
    // It may also be NULL if it is known that there are no relatively bound
    // words that will be encountered from the source--as in va_list calls.
    //
    Stub singular;

    // There is a lookahead step to see if the next item in an array is a
    // WORD!.  If so it is checked to see if that word is a "lookback word"
    // (e.g. one that refers to an ACTION! value set with SET/ENFIX).
    // Performing that lookup has the same cost as getting the variable value.
    // Considering that the value will need to be used anyway--infix or not--
    // the pointer is held in this field for WORD!s.
    //
    // However, reusing the work is not possible in the general case.  For
    // instance, this would cause a problem:
    //
    //     obj: make object! [x: 10]
    //     foo: does [append obj [y: 20]]
    //     do in obj [foo x]
    //                   ^-- consider the moment of lookahead, here
    //
    // Before foo is run, it will fetch x to ->gotten, and see that it is not
    // a lookback function.  But then when it runs foo, the memory location
    // where x had been found before may have moved due to expansion.
    //
    // Basically any function call invalidates ->gotten, as does obviously any
    // Fetch_Next_In_Feed (because the position changes).  So it has to be
    // nulled out fairly often, and checked for null before reuse.
    //
    // !!! Review how often gotten has hits vs. misses, and what the benefit
    // of the feature actually is.
    //
    Option(Value(const*)) gotten;

    // Feeds need to be freed when the last level reference is dropped.  This
    // doesn't go in a simple stacklike order, due to stack rearrangement
    // done by generators and tail calls etc.  Dropping the refcount to 0
    // should free it.
    //
    uintptr_t refcount;

  #if DEBUG_EXPIRED_LOOKBACK
    //
    // On each call to Fetch_Next_In_Feed, it's possible to ask it to give
    // a pointer to a cell with equivalent data to what was previously in
    // L->value, but that might not be L->value.  So for all practical
    // purposes, one is to assume that the L->value pointer died after the
    // fetch.  If clients are interested in doing "lookback" and examining
    // two values at the same time (or doing a GC and expecting to still
    // have the old L->current work), then they must not use the old L->value
    // but request the lookback pointer from Fetch_Next_In_Feed().
    //
    // To help stress this invariant, feeds will forcibly expire REBVAL
    // cells, handing out disposable lookback pointers on each eval.
    //
    // !!! Test currently leaks on shutdown, review how to not leak.
    //
    Cell* stress;
  #endif

  #if DEBUG_COUNT_TICKS
    Tick tick;
  #endif
};
