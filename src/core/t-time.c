//
//  File: %t-time.c
//  Summary: "time datatype"
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

#include "sys-core.h"

//
//  Split_Time: C
//
void Split_Time(REBI64 t, REB_TIMEF *tf)
{
    // note: negative sign will be lost.
    REBI64 h, m, s, n, i;

    if (t < 0) t = -t;

    h = t / HR_SEC;
    i = t - (h * HR_SEC);
    m = i / MIN_SEC;
    i = i - (m * MIN_SEC);
    s = i / SEC_SEC;
    n = i - (s * SEC_SEC);

    tf->h = (REBLEN)h;
    tf->m = (REBLEN)m;
    tf->s = (REBLEN)s;
    tf->n = (REBLEN)n;
}

//
//  Join_Time: C
//
// !! A REB_TIMEF has lost the sign bit available on the REBI64
// used for times.  If you want to make it negative, you need
// pass in a flag here.  (Flag added to help document the
// issue, as previous code falsely tried to judge the sign
// of tf->h, which is always positive.)
//
REBI64 Join_Time(REB_TIMEF *tf, bool neg)
{
    REBI64 t;

    t = (tf->h * HR_SEC) + (tf->m * MIN_SEC) + (tf->s * SEC_SEC) + tf->n;
    return neg ? -t : t;
}

//
//  Scan_Time: C
//
// Scan string and convert to time.  Return zero if error.
//
const Byte* Scan_Time(Cell* out, const Byte* cp, REBLEN len)
{
    cast(void, len); // !!! should len be paid attention to?

    bool neg;
    if (*cp == '-') {
        ++cp;
        neg = true;
    }
    else if (*cp == '+') {
        ++cp;
        neg = false;
    }
    else
        neg = false;

    if (*cp == '-' || *cp == '+')
        return NULL; // small hole: --1:23

    // Can be:
    //    HH:MM       as part1:part2
    //    HH:MM:SS    as part1:part2:part3
    //    HH:MM:SS.DD as part1:part2:part3.part4
    //    MM:SS.DD    as part1:part2.part4

    REBINT part1 = -1;
    cp = Grab_Int(cp, &part1);
    if (part1 > MAX_HOUR)
        return NULL;

    if (*cp++ != ':')
        return NULL;

    const Byte* sp;

    REBINT part2 = -1;
    sp = Grab_Int(cp, &part2);
    if (part2 < 0 || sp == cp)
        return NULL;

    cp = sp;

    REBINT part3 = -1;
    if (*cp == ':') {   // optional seconds
        sp = cp + 1;
        cp = Grab_Int(sp, &part3);
        if (part3 < 0 || cp == sp)
            return NULL;
    }

    REBINT part4 = -1;
    if (*cp == '.' || *cp == ',') {
        sp = ++cp;
        cp = Grab_Int_Scale(sp, &part4, 9);
        if (part4 == 0)
            part4 = -1;
    }

    Byte merid;
    if (
        *cp != '\0'
        && (UP_CASE(*cp) == 'A' || UP_CASE(*cp) == 'P')
        && (cp[1] != '\0' and UP_CASE(cp[1]) == 'M')
    ){
        merid = cast(Byte, UP_CASE(*cp));
        cp += 2;
    }
    else
        merid = '\0';

    REBI64 nanoseconds;
    if (part3 >= 0 || part4 < 0) { // HH:MM mode
        if (merid != '\0') {
            if (part1 > 12)
                return nullptr;

            if (part1 == 12)
                part1 = 0;

            if (merid == 'P')
                part1 += 12;
        }

        if (part3 < 0)
            part3 = 0;

        nanoseconds =  HOUR_TIME(part1) + MIN_TIME(part2) + SEC_TIME(part3);
    }
    else { // MM:SS mode
        if (merid != '\0')
            return nullptr; // no AM/PM for minutes

        nanoseconds = MIN_TIME(part1) + SEC_TIME(part2);
    }

    if (part4 > 0)
        nanoseconds += part4;

    if (neg)
        nanoseconds = -nanoseconds;

    Init_Time_Nanoseconds(out, nanoseconds);
    return cp;
}


//
//  MF_Time: C
//
void MF_Time(REB_MOLD *mo, NoQuote(const Cell*) v, bool form)
{
    UNUSED(form);  // no difference between MOLD and FORM at this time

    if (VAL_NANO(v) < cast(REBI64, 0))  // account for the sign if present
        Append_Codepoint(mo->series, '-');

    REB_TIMEF tf;
    Split_Time(VAL_NANO(v), &tf);  // loses sign

    // "H:MM" (pad minutes to two digits, but not the hour)
    //
    Append_Int(mo->series, tf.h);
    Append_Codepoint(mo->series, ':');
    Append_Int_Pad(mo->series, tf.m, 2);

    // If seconds or nanoseconds nonzero, pad seconds to ":SS", else omit
    //
    if (tf.s != 0 or tf.n != 0) {
        Append_Codepoint(mo->series, ':');
        Append_Int_Pad(mo->series, tf.s, 2);
    }

    // If nanosecond component is present, present as a fractional amount...
    // trimming any trailing zeros.
    //
    if (tf.n > 0) {
        Append_Codepoint(mo->series, '.');
        Append_Int_Pad(mo->series, tf.n, -9);
        Trim_Tail(mo, '0');
    }
}


//
//  CT_Time: C
//
REBINT CT_Time(NoQuote(const Cell*) a, NoQuote(const Cell*) b, bool strict)
{
    UNUSED(strict);

    REBI64 t1 = VAL_NANO(a);
    REBI64 t2 = VAL_NANO(b);

    if (t2 == t1)
        return 0;
    if (t1 > t2)
        return 1;
    return -1;
}


//
//  MAKE_Time: C
//
Bounce MAKE_Time(
    Level* level_,
    enum Reb_Kind kind,
    Option(Value(const*)) parent,
    const REBVAL *arg
){
    assert(kind == REB_TIME);
    if (parent)
        fail (Error_Bad_Make_Parent(kind, unwrap(parent)));

    switch (VAL_TYPE(arg)) {
    case REB_TIME: // just copy it (?)
        return Copy_Cell(OUT, arg);

    case REB_TEXT: { // scan using same decoding as LOAD would
        Size size;
        const Byte* bp = Analyze_String_For_Scan(&size, arg, MAX_SCAN_TIME);

        if (Scan_Time(OUT, bp, size) == nullptr)
            goto bad_make;

        return OUT; }

    case REB_INTEGER: // interpret as seconds
        if (VAL_INT64(arg) < -MAX_SECONDS || VAL_INT64(arg) > MAX_SECONDS)
            fail (Error_Out_Of_Range(arg));

        return Init_Time_Nanoseconds(OUT, VAL_INT64(arg) * SEC_SEC);

    case REB_DECIMAL:
        if (
            VAL_DECIMAL(arg) < cast(REBDEC, -MAX_SECONDS)
            || VAL_DECIMAL(arg) > cast(REBDEC, MAX_SECONDS)
        ){
            fail (Error_Out_Of_Range(arg));
        }
        return Init_Time_Nanoseconds(OUT, DEC_TO_SECS(VAL_DECIMAL(arg)));

    case REB_BLOCK: { // [hh mm ss]
        const Cell* tail;
        const Cell* item = VAL_ARRAY_AT(&tail, arg);

        if (item == tail)
            goto bad_make;  // must have at least hours

        if (not Is_Integer(item))
            goto bad_make;  // hours must be integer

        bool neg;
        REBI64 i = Int32(item);
        if (i < 0) {
            i = -i;
            neg = true;
        }
        else
            neg = false;

        REBI64 secs = i * 3600;
        if (secs > MAX_SECONDS)
            goto bad_make;

        if (tail != ++item) {  // minutes
            if (not Is_Integer(item))
                goto bad_make;

            if ((i = Int32(item)) < 0)
                goto bad_make;

            secs += i * 60;
            if (secs > MAX_SECONDS)
                goto bad_make;
        }

        if (item != tail and tail != ++item) {  // seconds
            if (Is_Integer(item)) {
                if ((i = Int32(item)) < 0)
                    goto bad_make;

                secs += i;
                if (secs > MAX_SECONDS)
                    goto bad_make;
            }
            else if (Is_Decimal(item)) {
                if (
                    secs + cast(REBI64, VAL_DECIMAL(item)) + 1
                    > MAX_SECONDS
                ){
                    goto bad_make;
                }

                // added in below
            }
            else
                goto bad_make;
        }

        REBI64 nano = secs * SEC_SEC;

        if (item != tail and tail != ++item) {
            if (not Is_Decimal(item))
                goto bad_make;

            nano += DEC_TO_SECS(VAL_DECIMAL(item));
        }

        if (item != tail and ++item != tail)
            goto bad_make;  // more than 4 items of initialization

        if (neg)
            nano = -nano;

        return Init_Time_Nanoseconds(OUT, nano); }

      default:
        goto bad_make;
    }

  bad_make:

    return RAISE(Error_Bad_Make(REB_TIME, arg));
}


//
//  TO_Time: C
//
Bounce TO_Time(Level* level_, enum Reb_Kind kind, const REBVAL *arg)
{
    return MAKE_Time(level_, kind, nullptr, arg);
}


//
//  Pick_Time: C
//
void Pick_Time(Sink(Value(*)) out, const Cell* value, const Cell* picker)
{
    REBINT i;
    if (Is_Word(picker)) {
        switch (VAL_WORD_ID(picker)) {
        case SYM_HOUR:   i = 0; break;
        case SYM_MINUTE: i = 1; break;
        case SYM_SECOND: i = 2; break;
        default:
            fail (picker);
        }
    }
    else if (Is_Integer(picker))
        i = VAL_INT32(picker) - 1;
    else
        fail (picker);

    REB_TIMEF tf;
    Split_Time(VAL_NANO(value), &tf); // loses sign

    switch(i) {
    case 0: // hours
        Init_Integer(out, tf.h);
        break;
    case 1: // minutes
        Init_Integer(out, tf.m);
        break;
    case 2: // seconds
        if (tf.n == 0)
            Init_Integer(out, tf.s);
        else
            Init_Decimal(out, cast(REBDEC, tf.s) + (tf.n * NANO));
        break;
    default:
        Init_Nulled(out); // "out of range" behavior for pick
    }
}


//
//  Poke_Time_Immediate: C
//
void Poke_Time_Immediate(
    REBVAL *value,
    const Cell* picker,
    const REBVAL *poke
) {
    REBINT i;
    if (Is_Word(picker)) {
        switch (VAL_WORD_ID(picker)) {
        case SYM_HOUR:   i = 0; break;
        case SYM_MINUTE: i = 1; break;
        case SYM_SECOND: i = 2; break;
        default:
            fail (picker);
        }
    }
    else if (Is_Integer(picker))
        i = VAL_INT32(picker) - 1;
    else
        fail (picker);

    REB_TIMEF tf;
    Split_Time(VAL_NANO(value), &tf); // loses sign

    REBINT n;
    if (Is_Integer(poke) || Is_Decimal(poke))
        n = Int32s(poke, 0);
    else if (Is_Blank(poke))
        n = 0;
    else
        fail (poke);

    switch(i) {
    case 0:
        tf.h = n;
        break;
    case 1:
        tf.m = n;
        break;
    case 2:
        if (Is_Decimal(poke)) {
            REBDEC f = VAL_DECIMAL(poke);
            if (f < 0.0)
                fail (Error_Out_Of_Range(poke));

            tf.s = cast(REBINT, f);
            tf.n = cast(REBINT, (f - tf.s) * SEC_SEC);
        }
        else {
            tf.s = n;
            tf.n = 0;
        }
        break;

    default:
        fail (picker);
    }

    PAYLOAD(Time, value).nanoseconds = Join_Time(&tf, false);
}


//
//  REBTYPE: C
//
REBTYPE(Time)
{
    REBVAL *time = D_ARG(1);

    REBI64 secs = VAL_NANO(time);

    Option(SymId) id = ID_OF_SYMBOL(verb);

    if (id == SYM_PICK_P) {

    //=//// PICK* (see %sys-pick.h for explanation) ////////////////////////=//

        INCLUDE_PARAMS_OF_PICK_P;
        UNUSED(ARG(location));

        const Cell* picker = ARG(picker);

        Pick_Time(OUT, time, picker);
        return OUT;
    }
    else if (id == SYM_POKE_P) {

    //=//// POKE* (see %sys-pick.h for explanation) ////////////////////////=//

        INCLUDE_PARAMS_OF_POKE_P;
        UNUSED(ARG(location));

        const Cell* picker = ARG(picker);

        REBVAL *setval = ARG(value);

        Poke_Time_Immediate(time, picker, setval);
        return COPY(time);  // caller needs to update their time bits
    }

    if (
        id == SYM_ADD
        or id == SYM_SUBTRACT
        or id == SYM_MULTIPLY
        or id == SYM_DIVIDE
        or id == SYM_REMAINDER
    ){
        REBVAL *arg = D_ARG(2);
        REBINT type = VAL_TYPE(arg);

        if (type == REB_TIME) {     // handle TIME - TIME cases
            REBI64 secs2 = VAL_NANO(arg);

            switch (id) {
              case SYM_ADD:
                secs = Add_Max(REB_TIME, secs, secs2, MAX_TIME);
                return Init_Time_Nanoseconds(OUT, secs);

              case SYM_SUBTRACT:
                secs = Add_Max(REB_TIME, secs, -secs2, MAX_TIME);
                return Init_Time_Nanoseconds(OUT, secs);

              case SYM_DIVIDE:
                if (secs2 == 0)
                    fail (Error_Zero_Divide_Raw());
                return Init_Decimal(
                    OUT,
                    cast(REBDEC, secs) / cast(REBDEC, secs2)
                );

              case SYM_REMAINDER:
                if (secs2 == 0)
                    fail (Error_Zero_Divide_Raw());
                secs %= secs2;
                return Init_Time_Nanoseconds(OUT, secs);

              default:
                fail (Error_Math_Args(REB_TIME, verb));
            }
        }
        else if (type == REB_INTEGER) {     // handle TIME - INTEGER cases
            REBI64 num = VAL_INT64(arg);

            switch (id) {
              case SYM_ADD:
                secs = Add_Max(REB_TIME, secs, num * SEC_SEC, MAX_TIME);
                return Init_Time_Nanoseconds(OUT, secs);

              case SYM_SUBTRACT:
                secs = Add_Max(REB_TIME, secs, num * -SEC_SEC, MAX_TIME);
                return Init_Time_Nanoseconds(OUT, secs);

              case SYM_MULTIPLY:
                secs *= num;
                if (secs < -MAX_TIME || secs > MAX_TIME)
                    fail (Error_Type_Limit_Raw(Datatype_From_Kind(REB_TIME)));
                return Init_Time_Nanoseconds(OUT, secs);

              case SYM_DIVIDE:
                if (num == 0)
                    fail (Error_Zero_Divide_Raw());
                secs /= num;
                Init_Integer(OUT, secs);
                return Init_Time_Nanoseconds(OUT, secs);

              case SYM_REMAINDER:
                if (num == 0)
                    fail (Error_Zero_Divide_Raw());
                secs %= num;
                return Init_Time_Nanoseconds(OUT, secs);

              default:
                fail (Error_Math_Args(REB_TIME, verb));
            }
        }
        else if (type == REB_DECIMAL) {     // handle TIME - DECIMAL cases
            REBDEC dec = VAL_DECIMAL(arg);

            switch (id) {
              case SYM_ADD:
                secs = Add_Max(
                    REB_TIME,
                    secs,
                    cast(int64_t, dec * SEC_SEC),
                    MAX_TIME
                );
                return Init_Time_Nanoseconds(OUT, secs);

              case SYM_SUBTRACT:
                secs = Add_Max(
                    REB_TIME,
                    secs,
                    cast(int64_t, dec * -SEC_SEC),
                    MAX_TIME
                );
                return Init_Time_Nanoseconds(OUT, secs);

              case SYM_MULTIPLY:
                secs = cast(int64_t, secs * dec);
                return Init_Time_Nanoseconds(OUT, secs);

              case SYM_DIVIDE:
                if (dec == 0.0)
                    fail (Error_Zero_Divide_Raw());
                secs = cast(int64_t, secs / dec);
                return Init_Time_Nanoseconds(OUT, secs);

              /*  // !!! Was commented out, why?
             case SYM_REMAINDER:
               ld = fmod(ld, VAL_DECIMAL(arg));
               goto decTime; */

              default:
                fail (Error_Math_Args(REB_TIME, verb));
            }
        }
        else if (type == REB_DATE and id == SYM_ADD) {
            //
            // We're adding a time and a date, code for which exists in the
            // date dispatcher already.  Instead of repeating the code here in
            // the time dispatcher, swap the arguments and call DATE's version.
            //
            Move_Cell(SPARE, D_ARG(1));
            Move_Cell(D_ARG(1), arg);
            Move_Cell(D_ARG(2), SPARE);
            return T_Date(level_, verb);
        }
        fail (Error_Math_Args(REB_TIME, verb));
    }
    else {
        // unary actions
        switch (id) {
          case SYM_COPY:
            return COPY(time);  // immediate type, just copy bits

          case SYM_ODD_Q:
            return Init_Logic(OUT, (SECS_FROM_NANO(secs) & 1) != 0);

          case SYM_EVEN_Q:
            return Init_Logic(OUT, (SECS_FROM_NANO(secs) & 1) == 0);

          case SYM_NEGATE:
            secs = -secs;
            return Init_Time_Nanoseconds(OUT, secs);

          case SYM_ABSOLUTE:
            if (secs < 0) secs = -secs;
            return Init_Time_Nanoseconds(OUT, secs);

          case SYM_ROUND: {
            INCLUDE_PARAMS_OF_ROUND;
            USED(ARG(value));  // aliased as v, others are passed via level_
            USED(ARG(even)); USED(ARG(down)); USED(ARG(half_down));
            USED(ARG(floor)); USED(ARG(ceiling)); USED(ARG(half_ceiling));

            if (not REF(to)) {
                Init_True(ARG(to));  // by default make it /TO seconds
                secs = Round_Int(secs, level_, SEC_SEC);
                return Init_Time_Nanoseconds(OUT, secs);
            }

            REBVAL *to = ARG(to);
            if (Is_Time(to)) {
                secs = Round_Int(secs, level_, VAL_NANO(to));
                return Init_Time_Nanoseconds(OUT, secs);
            }
            else if (Is_Decimal(to)) {
                VAL_DECIMAL(to) = Round_Dec(
                    cast(REBDEC, secs),
                    level_,
                    Dec64(to) * SEC_SEC
                );
                VAL_DECIMAL(to) /= SEC_SEC;
                return COPY(to);
            }
            else if (Is_Integer(to)) {
                mutable_VAL_INT64(to)
                    = Round_Int(secs, level_, Int32(to) * SEC_SEC) / SEC_SEC;
                return COPY(to);
            }

            fail (PARAM(to)); }

          case SYM_RANDOM: {
            INCLUDE_PARAMS_OF_RANDOM;

            UNUSED(PARAM(value));

            if (REF(only))
                fail (Error_Bad_Refines_Raw());

            if (REF(seed)) {
                Set_Random(secs);
                return nullptr;
            }
            secs = Random_Range(secs / SEC_SEC, REF(secure)) * SEC_SEC;
            return Init_Time_Nanoseconds(OUT, secs); }

          default:
            break;
        }
    }

    fail (UNHANDLED);
}
