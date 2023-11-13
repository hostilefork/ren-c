//
//  File: %t-map.c
//  Summary: "map datatype"
//  Section: datatypes
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
// See %sys-map.h for an explanation of the map structure.
//

#include "sys-core.h"

//
//  CT_Map: C
//
// !!! Was never implemented in R3-Alpha; called into raw array comparison,
// which is clearly incorrect.  Needs to be written.
//
REBINT CT_Map(NoQuote(Cell(const*)) a, NoQuote(Cell(const*)) b, bool strict)
{
    UNUSED(a);
    UNUSED(b);
    UNUSED(strict);
    fail ("https://github.com/rebol/rebol-issues/issues/2340");
}


//
//  Make_Map: C
//
// Makes a MAP block (that holds both keys and values).
// Capacity is measured in key-value pairings.
// A hash series is also created.
//
REBMAP *Make_Map(REBLEN capacity)
{
    Array(*) pairlist = Make_Array_Core(capacity * 2, SERIES_MASK_PAIRLIST);
    mutable_LINK(Hashlist, pairlist) = Make_Hash_Series(capacity);

    return MAP(pairlist);
}


static Context(*) Error_Conflicting_Key(
    Cell(const*) key,
    REBSPC *specifier
){
    DECLARE_LOCAL (specific);
    Derelativize(specific, key, specifier);
    return Error_Conflicting_Key_Raw(specific);
}


//
//  Find_Key_Hashed: C
//
// Returns hash index (either the match or the new one).
// A return of zero is valid (as a hash index);
//
// Wide: width of record (normally 2, a key and a value).
//
// Modes:
//     0 - search, return hash if found or not
//     1 - search, return hash, else return -1 if not
//     2 - search, return hash, else append value and return -1
//
REBINT Find_Key_Hashed(
    Array(*) array,
    REBSER *hashlist,
    Cell(const*) key,  // !!! assumes ++key finds the values
    REBSPC *specifier,
    REBLEN wide,
    bool strict,
    Byte mode
){
    // Hashlists store a indexes into the actual data array, of where the
    // first key corresponding to that hash is.  There may be more keys
    // indicated by that hash, vying for the same slot.  So the collisions
    // add a skip amount and keep trying:
    //
    // https://en.wikipedia.org/wiki/Linear_probing
    //
    // Used and skip are co-primes, so is guaranteed that repeatedly
    // adding skip (and subtracting len when needed) all positions are
    // visited.  1 <= skip < len, and len is prime, so this is guaranteed.
    //
    REBLEN used = SER_USED(hashlist);
    REBLEN *indexes = SER_HEAD(REBLEN, hashlist);

    uint32_t hash = Hash_Value(key);
    REBLEN slot = hash % used;  // first slot to try for this hash
    REBLEN skip = hash % (used - 1) + 1;  // skip by how much each collision

    // Zombie slots are those which are left behind by removing items, with
    // void values that are illegal in maps, and indicate they can be reused.
    //
    REBINT zombie_slot = -1; // no zombies seen yet...

    // You can store information case-insensitively in a MAP!, and it will
    // overwrite the value for at most one other key.  Reading information
    // case-insensitively out of a map can only be done if there aren't two
    // keys with the same spelling.
    //
    REBINT synonym_slot = -1; // no synonyms seen yet...

    REBLEN n;
    while ((n = indexes[slot]) != 0) {
        Cell(*) k = ARR_AT(array, (n - 1) * wide); // stored key
        if (0 == Cmp_Value(k, key, true)) {
            if (strict)
                return slot; // don't need to check synonyms, stop looking
            goto found_synonym; // confirm exact match is the only match
        }

        if (not strict) {  // now do the non strict match (false)
            if (0 == Cmp_Value(k, key, false)) {

              found_synonym:;

                if (synonym_slot != -1) // another equivalent already matched
                    fail (Error_Conflicting_Key(key, specifier));
                synonym_slot = slot; // save and continue checking
            }
        }

        if (wide > 1 && Is_Void(k + 1) && zombie_slot == -1)
            zombie_slot = slot;

        slot += skip;
        if (slot >= used)
            slot -= used;
    }

    if (synonym_slot != -1) {
        assert(not strict);
        return synonym_slot; // there weren't other spellings of the same key
    }

    if (zombie_slot != -1) { // zombie encountered; overwrite with new key
        assert(mode == 0);
        slot = zombie_slot;
        Derelativize(
            ARR_AT(array, (indexes[slot] - 1) * wide),
            key,
            specifier
        );
    }

    if (mode > 1) { // append new value to the target series
        Cell(const*) src = key;
        indexes[slot] = (ARR_LEN(array) / wide) + 1;

        REBLEN index;
        for (index = 0; index < wide; ++src, ++index)
            Append_Value_Core(array, src, specifier);
    }

    return (mode > 0) ? -1 : cast(REBINT, slot);
}


//
//  Rehash_Map: C
//
// Recompute the entire hash table for a map. Table must be large enough.
//
static void Rehash_Map(REBMAP *map)
{
    REBSER *hashlist = MAP_HASHLIST(map);

    if (!hashlist) return;

    REBLEN *hashes = SER_HEAD(REBLEN, hashlist);
    Array(*) pairlist = MAP_PAIRLIST(map);

    REBVAL *key = SPECIFIC(ARR_HEAD(pairlist));
    REBLEN n;

    for (n = 0; n < ARR_LEN(pairlist); n += 2, key += 2) {
        const bool cased = true; // cased=true is always fine

        if (Is_Void(key + 1)) {
            //
            // It's a "zombie", move last key to overwrite it
            //
            Copy_Cell(
                key, SPECIFIC(ARR_AT(pairlist, ARR_LEN(pairlist) - 2))
            );
            Copy_Cell(
                &key[1], SPECIFIC(ARR_AT(pairlist, ARR_LEN(pairlist) - 1))
            );
            SET_SERIES_LEN(pairlist, ARR_LEN(pairlist) - 2);
        }

        REBLEN hash = Find_Key_Hashed(
            pairlist, hashlist, key, SPECIFIED, 2, cased, 0
        );
        hashes[hash] = n / 2 + 1;

        // discard zombies at end of pairlist
        //
        while (Is_Void(ARR_AT(pairlist, ARR_LEN(pairlist) - 1))) {
            SET_SERIES_LEN(pairlist, ARR_LEN(pairlist) - 2);
        }
    }
}


//
//  Expand_Hash: C
//
// Expand hash series. Clear it but set its tail.
//
void Expand_Hash(REBSER *ser)
{
    assert(not IS_SER_ARRAY(ser));

    REBINT prime = Get_Hash_Prime_May_Fail(SER_USED(ser) + 1);
    Remake_Series(
        ser,
        prime + 1,
        SERIES_FLAG_POWER_OF_2  // not(NODE_FLAG_NODE) => don't keep data
    );

    Clear_Series(ser);
    SET_SERIES_LEN(ser, prime);
}


//
//  Find_Map_Entry: C
//
// Try to find the entry in the map. If not found and val isn't nullptr,
// create the entry and store the key and val.
//
// RETURNS: the index to the VALUE or zero if there is none.
//
REBLEN Find_Map_Entry(
    REBMAP *map,
    Cell(const*) key,
    REBSPC *key_specifier,
    Cell(const*) val,
    REBSPC *val_specifier,
    bool strict
) {
    assert(not Is_Isotope(key));

    REBSER *hashlist = MAP_HASHLIST(map); // can be null
    Array(*) pairlist = MAP_PAIRLIST(map);

    assert(hashlist);

    // Get hash table, expand it if needed:
    if (ARR_LEN(pairlist) > SER_USED(hashlist) / 2) {
        Expand_Hash(hashlist); // modifies size value
        Rehash_Map(map);
    }

    const REBLEN wide = 2;
    const Byte mode = 0; // just search for key, don't add it
    REBLEN slot = Find_Key_Hashed(
        pairlist, hashlist, key, key_specifier, wide, strict, mode
    );

    REBLEN *indexes = SER_HEAD(REBLEN, hashlist);
    REBLEN n = indexes[slot];

    // n==0 or pairlist[(n-1)*]=~key

    if (val == NULL)
        return n; // was just fetching the value

    // If not just a GET, it may try to set the value in the map.  Which means
    // the key may need to be stored.  Since copies of keys are never made,
    // a SET must always be done with an immutable key...because if it were
    // changed, there'd be no notification to rehash the map.
    //
    Force_Value_Frozen_Deep_Blame(key, MAP_PAIRLIST(map));

    // Must set the value:
    if (n) {  // re-set it:
        Derelativize(
            ARR_AT(pairlist, ((n - 1) * 2) + 1),
            val,
            val_specifier
        );
        return n;
    }

    if (Is_Void(val)) return 0; // trying to remove non-existing key

    // Create new entry.  Note that it does not copy underlying series (e.g.
    // the data of a string), which is why the immutability test is necessary
    //
    Append_Value_Core(pairlist, key, key_specifier);
    Append_Value_Core(pairlist, val, val_specifier);

    return (indexes[slot] = (ARR_LEN(pairlist) / 2));
}


//
//  Append_Map: C
//
static void Append_Map(
    REBMAP *map,
    Cell(const*) head,
    Cell(const*) tail,
    REBSPC *specifier,
    REBLEN len
){
    Cell(const*) item = head;
    REBLEN n = 0;

    while (n < len and item != tail) {
        if (item + 1 == tail) {
            //
            // Keys with no value not allowed, e.g. `make map! [1 "foo" 2]`
            //
            fail (Error_Index_Out_Of_Range_Raw());
        }

        bool strict = true;
        Find_Map_Entry(
            map,
            item,
            specifier,
            item + 1,
            specifier,
            strict
        );

        item += 2;
        n += 2;
    }
}


//
//  MAKE_Map: C
//
Bounce MAKE_Map(
    Level(*) level_,
    enum Reb_Kind kind,
    Option(Value(const*)) parent,
    const REBVAL *arg
){
    if (parent)
        return RAISE(Error_Bad_Make_Parent(kind, unwrap(parent)));

    if (ANY_NUMBER(arg)) {
        return Init_Map(OUT, Make_Map(Int32s(arg, 0)));
    }
    else {
        // !!! R3-Alpha TO of MAP! was like MAKE but wouldn't accept just
        // being given a size.
        //
        return TO_Map(level_, kind, arg);
    }
}


inline static REBMAP *Copy_Map(const REBMAP *map, REBU64 types) {
    Array(*) copy = Copy_Array_Shallow_Flags(
        MAP_PAIRLIST(map),
        SPECIFIED,
        SERIES_MASK_PAIRLIST
    );

    // So long as the copied pairlist is the same array size as the original,
    // a literal copy of the hashlist can still be used, as a start (needs
    // its own copy so new map's hashes will reflect its own mutations)
    //
    REBSER *hashlist = Copy_Series_Core(
        MAP_HASHLIST(map),
        SERIES_FLAGS_NONE | FLAG_FLAVOR(HASHLIST)
            // ^-- !!! No NODE_FLAG_MANAGED?
    );
    mutable_LINK(Hashlist, copy) = hashlist;

    if (types == 0)
        return MAP(copy); // no types have deep copy requested, shallow is OK

    // Even if the type flags request deep copies of series, none of the keys
    // need to be copied deeply.  This is because they are immutable at the
    // time of insertion.
    //
    assert(ARR_LEN(copy) % 2 == 0); // should be [key value key value]...

    Cell(const*) tail = ARR_TAIL(copy);
    REBVAL *key = SPECIFIC(ARR_HEAD(copy));  // keys/vals specified
    for (; key != tail; key += 2) {
        assert(Is_Value_Frozen_Deep(key));  // immutable key

        REBVAL *v = key + 1;
        assert(v != tail);
        if (Is_Void(v))
            continue; // "zombie" map element (not present)

        Flags flags = NODE_FLAG_MANAGED;  // !!! Review
        Clonify(v, flags, types);
    }

    return MAP(copy);
}


//
//  TO_Map: C
//
Bounce TO_Map(Level(*) level_, enum Reb_Kind kind, const REBVAL *arg)
{
    assert(kind == REB_MAP);
    UNUSED(kind);

    if (IS_BLOCK(arg) || IS_GROUP(arg)) {
        //
        // make map! [word val word val]
        //
        REBLEN len = VAL_LEN_AT(arg);
        Cell(const*) tail;
        Cell(const*) at = VAL_ARRAY_AT(&tail, arg);
        REBSPC *specifier = VAL_SPECIFIER(arg);

        REBMAP *map = Make_Map(len / 2); // [key value key value...] + END
        Append_Map(map, at, tail, specifier, len);
        Rehash_Map(map);
        return Init_Map(OUT, map);
    }
    else if (IS_MAP(arg)) {
        //
        // Values are not copied deeply by default.
        //
        // !!! Is there really a use in allowing MAP! to be converted TO a
        // MAP! as opposed to having people COPY it?
        //
        REBU64 types = 0;

        return Init_Map(OUT, Copy_Map(VAL_MAP(arg), types));
    }

    return RAISE(arg);
}


//
//  Map_To_Array: C
//
// what: -1 - words, +1 - values, 0 -both
//
Array(*) Map_To_Array(const REBMAP *map, REBINT what)
{
    REBLEN count = Length_Map(map);
    Array(*) a = Make_Array(count * ((what == 0) ? 2 : 1));

    Cell(*) dest = ARR_HEAD(a);
    Cell(const*) val_tail = ARR_TAIL(MAP_PAIRLIST(map));
    Cell(const*) val = ARR_HEAD(MAP_PAIRLIST(map));
    for (; val != val_tail; val += 2) {
        if (Is_Void(val + 1))  // val + 1 can't be past tail
            continue;  // zombie key, e.g. not actually in map

        if (what <= 0) {
            Copy_Cell(dest, SPECIFIC(&val[0]));
            ++dest;
        }
        if (what >= 0) {
            Copy_Cell(dest, SPECIFIC(&val[1]));
            ++dest;
        }
    }

    SET_SERIES_LEN(a, dest - ARR_HEAD(a));
    return a;
}


//
//  Alloc_Context_From_Map: C
//
Context(*) Alloc_Context_From_Map(const REBMAP *map)
{
    // Doesn't use Length_Map because it only wants to consider words.
    //
    // !!! Should this fail() if any of the keys aren't words?  It seems
    // a bit haphazard to have `make object! make map! [x 10 <y> 20]` and
    // just throw out the <y> 20 case...

    REBLEN count = 0;

  blockscope {
    Cell(const*) mval_tail = ARR_TAIL(MAP_PAIRLIST(map));
    Cell(const*) mval = ARR_HEAD(MAP_PAIRLIST(map));
    for (; mval != mval_tail; mval += 2) {  // note mval must not be END
        if (ANY_WORD(mval) and not Is_Void(mval + 1))
            ++count;
    }
  }

    // See Alloc_Context() - cannot use it directly because no Collect_Words

    Context(*) c = Alloc_Context(REB_OBJECT, count);

    Cell(const*) mval_tail = ARR_TAIL(MAP_PAIRLIST(map));
    Cell(const*) mval = ARR_HEAD(MAP_PAIRLIST(map));

    for (; mval != mval_tail; mval += 2) {  // note mval must not be END
        if (ANY_WORD(mval) and not Is_Void(mval + 1)) {
            REBVAL *var = Append_Context(c, VAL_WORD_SYMBOL(mval));
            Copy_Cell(var, SPECIFIC(mval + 1));
        }
    }

    return c;
}


//
//  MF_Map: C
//
void MF_Map(REB_MOLD *mo, NoQuote(Cell(const*)) v, bool form)
{
    const REBMAP *m = VAL_MAP(v);

    // Prevent endless mold loop:
    if (Find_Pointer_In_Series(TG_Mold_Stack, m) != NOT_FOUND) {
        Append_Ascii(mo->series, "...]");
        return;
    }

    Push_Pointer_To_Series(TG_Mold_Stack, m);

    if (not form) {
        Pre_Mold(mo, v);
        Append_Codepoint(mo->series, '[');
    }

    // Mold all entries that are set.  As with contexts, void values are not
    // valid entries but indicate the absence of a value.
    //
    mo->indent++;

    Cell(const*) tail = ARR_TAIL(MAP_PAIRLIST(m));
    Cell(const*) key = ARR_HEAD(MAP_PAIRLIST(m));
    for (; key != tail; key += 2) {  // note value slot must not be END
        assert(key + 1 != tail);
        if (Is_Void(key + 1))
            continue;  // if value for this key is void, key has been removed

        if (not form)
            New_Indented_Line(mo);
        Mold_Value(mo, key);
        Append_Codepoint(mo->series, ' ');
        Mold_Value(mo, key + 1);
        if (form)
            Append_Codepoint(mo->series, '\n');
    }
    mo->indent--;

    if (not form) {
        New_Indented_Line(mo);
        Append_Codepoint(mo->series, ']');
    }

    End_Mold(mo);

    Drop_Pointer_From_Series(TG_Mold_Stack, m);
}


//
//  REBTYPE: C
//
REBTYPE(Map)
{
    REBVAL *map = D_ARG(1);

    switch (ID_OF_SYMBOL(verb)) {
      case SYM_REFLECT: {
        INCLUDE_PARAMS_OF_REFLECT;
        UNUSED(ARG(value));  // covered by `v`

        const REBMAP *m = VAL_MAP(map);

        REBVAL *property = ARG(property);
        switch (VAL_WORD_ID(property)) {
          case SYM_LENGTH:
            return Init_Integer(OUT, Length_Map(m));

          case SYM_VALUES:
            return Init_Block(OUT, Map_To_Array(m, 1));

          case SYM_WORDS:
            return Init_Block(OUT, Map_To_Array(m, -1));

          case SYM_BODY:
            return Init_Block(OUT, Map_To_Array(m, 0));

          case SYM_TAIL_Q:
            return Init_Logic(OUT, Length_Map(m) == 0);

          default:
            break;
        }
        fail (Error_Cannot_Reflect(REB_MAP, property)); }

      case SYM_SELECT: {
        INCLUDE_PARAMS_OF_SELECT;
        if (Is_Isotope(ARG(value)))
            fail (ARG(value));

        UNUSED(PARAM(series));  // covered by `v`
        UNUSED(PARAM(tail));  // returning tail not supported

        if (REF(part) or REF(skip) or REF(match))
            fail (Error_Bad_Refines_Raw());

        const REBMAP *m = VAL_MAP(map);

        REBINT n = Find_Map_Entry(
            m_cast(REBMAP*, VAL_MAP(map)),  // should not modify, see below
            ARG(value),
            SPECIFIED,
            nullptr,  // nullptr indicates it will only search, not modify
            SPECIFIED,
            REF(case)
        );

        if (n == 0)
            return nullptr;

        Copy_Cell(
            OUT,
            SPECIFIC(ARR_AT(MAP_PAIRLIST(m), ((n - 1) * 2) + 1))
        );

        return OUT; }

      case SYM_PUT: {
        INCLUDE_PARAMS_OF_PUT;
        UNUSED(ARG(series)); // extracted to `map`

        REBINT n = Find_Map_Entry(
            VAL_MAP_ENSURE_MUTABLE(map),
            ARG(key),
            SPECIFIED,
            ARG(value),  // non-null indicates it will modify, vs. just search
            SPECIFIED,
            REF(case)
        );
        UNUSED(n);

        return COPY(ARG(value)); }

      case SYM_INSERT:
      case SYM_APPEND: {
        INCLUDE_PARAMS_OF_INSERT;
        UNUSED(PARAM(series));

        REBVAL *value = ARG(value);
        if (Is_Void(value))
            return COPY(map);  // don't fail on read only if it would be a no-op

        if (not Is_Splice(value))
            fail ("Appending to MAP! only accepts a splice block of key/value");

        mutable_QUOTE_BYTE(value) = UNQUOTED_1;

        REBMAP *m = VAL_MAP_ENSURE_MUTABLE(map);

        if (REF(line) or REF(dup))
            fail (Error_Bad_Refines_Raw());

        REBLEN len = Part_Len_May_Modify_Index(value, ARG(part));
        Cell(const*) tail;
        Cell(const*) at = VAL_ARRAY_AT(&tail, value);  // w/modified index

        Append_Map(m, at, tail, VAL_SPECIFIER(value), len);

        return Init_Map(OUT, m); }

      case SYM_COPY: {
        INCLUDE_PARAMS_OF_COPY;
        UNUSED(PARAM(value));

        if (REF(part))
            fail (Error_Bad_Refines_Raw());

        REBU64 types = 0; // which types to copy non-"shallowly"

        if (REF(deep))
            types |= TS_CLONE;

        return Init_Map(OUT, Copy_Map(VAL_MAP(map), types)); }

      case SYM_CLEAR: {
        REBMAP *m = VAL_MAP_ENSURE_MUTABLE(map);

        Reset_Array(MAP_PAIRLIST(m));

        // !!! Review: should the space for the hashlist be reclaimed?  This
        // clears all the indices but doesn't scale back the size.
        //
        Clear_Series(MAP_HASHLIST(m));

        return Init_Map(OUT, m); }

    //=//// PICK* (see %sys-pick.h for explanation) ////////////////////////=//

      case SYM_PICK_P: {
        INCLUDE_PARAMS_OF_PICK_P;
        UNUSED(ARG(location));

        Cell(const*) picker = ARG(picker);  // isotope pickers not allowed
        if (Is_Isotope(picker))
            return RAISE(Error_Bad_Isotope(picker));

        bool strict = false;

        REBINT n = Find_Map_Entry(
            m_cast(REBMAP*, VAL_MAP(map)),  // not modified
            picker,
            SPECIFIED,
            nullptr,  // no value, so map not changed
            SPECIFIED,
            strict
        );

        if (n == 0)
            return nullptr;

        const REBVAL *val = SPECIFIC(
            ARR_AT(MAP_PAIRLIST(VAL_MAP(map)), ((n - 1) * 2) + 1)
        );
        if (Is_Void(val))  // zombie entry, means unused
            return nullptr;

        return Copy_Cell(OUT, val); }

    //=//// POKE* (see %sys-pick.h for explanation) ////////////////////////=//

      case SYM_POKE_P: {
        INCLUDE_PARAMS_OF_POKE_P;
        UNUSED(ARG(location));

        Cell(const*) picker = ARG(picker);  // isotope pickers not allowed
        if (Is_Isotope(picker))
            return RAISE(Error_Bad_Isotope(picker));

        // Fetching and setting with path-based access is case-preserving for
        // initial insertions.  However, the case-insensitivity means that all
        // writes after that to the same key will not be overriding the key,
        // it will just change the data value for the existing key.  SELECT and
        // the operation tentatively named PUT should be used if a map is to
        // distinguish multiple casings of the same key.
        //
        bool strict = false;

        REBVAL *setval = ARG(value);  // Note: VOID interpreted as remove key

        if (Is_Isotope(setval))  // isotopes not allowed as value in maps
            return RAISE(Error_Bad_Isotope(setval));

        REBINT n = Find_Map_Entry(
            VAL_MAP_ENSURE_MUTABLE(map),  // modified
            picker,
            SPECIFIED,
            setval,  // value to set (either ARG(value) or L->out)
            SPECIFIED,
            strict
        );

        assert(n != 0);
        UNUSED(n);

        return nullptr; }  // no upstream changes needed for REBMAP* reference

      default:
        break;
    }

    fail (UNHANDLED);
}
