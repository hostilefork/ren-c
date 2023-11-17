//
//  File: %t-typeset.c
//  Summary: "typeset datatype"
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
//  CT_Parameter: C
//
REBINT CT_Parameter(NoQuote(Cell(const*)) a, NoQuote(Cell(const*)) b, bool strict)
{
    UNUSED(strict);

    assert(CELL_HEART(a) == REB_PARAMETER);
    assert(CELL_HEART(b) == REB_PARAMETER);
    UNUSED(a);
    UNUSED(b);

    fail ("Parameter equality test currently disabled");
}


//
//  Startup_Typesets: C
//
// Create typeset variables that are defined above.
// For example: NUMBER is both integer and decimal.
// Add the new variables to the system context.
//
void Startup_Typesets(void)
{
    REBINT id;
    for (id = SYM_ANY_VALUE_Q; id != SYM_DATATYPES; id += 2) {
        REBINT n = (id - SYM_ANY_VALUE_Q) / 2;  // means Typesets[n]

        // We want the forms like ANY-VALUE? to be typechecker functions that
        // act on Typesets[n].
        //
        DECLARE_STABLE (typeset_index);
        Init_Integer(typeset_index, n);
        Phase* typechecker = Make_Typechecker(typeset_index);

        Init_Activation(
            Force_Lib_Var(cast(SymId, id)),
            typechecker,
            Canon_Symbol(cast(SymId, id)),  // cached symbol for function
            UNBOUND
        );

        // Make e.g. ANY-VALUE! a TYPE-GROUP! with the bound question mark
        // form inside it, e.g. any-value!: &(any-value?)
        //
        Array* a = Alloc_Singular(NODE_FLAG_MANAGED);
        Init_Any_Word_Bound(
            Array_Single(a),
            REB_WORD,
            Canon_Symbol(cast(SymId, id)),
            Lib_Context,
            INDEX_ATTACHED
        );
        Init_Array_Cell(Force_Lib_Var(cast(SymId, id + 1)), REB_TYPE_GROUP, a);
    }

    Index last = (cast(int, SYM_DATATYPES) - SYM_ANY_VALUE_Q) / 2;
    assert(Typesets[last] == 0);  // table ends in zero
    UNUSED(last);

    // Make the NULL! type checker
  {
    Array* a = Alloc_Singular(NODE_FLAG_MANAGED);
    Init_Any_Word_Bound(
        Array_Single(a),
        REB_WORD,
        Canon(NULL_Q),
        Lib_Context,
        INDEX_ATTACHED
    );
    Init_Array_Cell(Force_Lib_Var(SYM_NULL_X), REB_TYPE_GROUP, a);
  }

    // Make the ACTIVATION! type checker
  {
    Array* a = Alloc_Singular(NODE_FLAG_MANAGED);
    Init_Any_Word_Bound(
        Array_Single(a),
        REB_WORD,
        Canon(ACTIVATION_Q),
        Lib_Context,
        INDEX_ATTACHED
    );
    Init_Array_Cell(Force_Lib_Var(SYM_ACTIVATION_X), REB_TYPE_GROUP, a);
  }

    // Make the ANY-MATCHER! type checker
  {
    Array* a = Alloc_Singular(NODE_FLAG_MANAGED);
    Init_Any_Word_Bound(
        Array_Single(a),
        REB_WORD,
        Canon(ANY_MATCHER_Q),
        Lib_Context,
        INDEX_ATTACHED
    );
    Init_Array_Cell(Force_Lib_Var(SYM_ANY_MATCHER_X), REB_TYPE_GROUP, a);
  }
}


//
//  Shutdown_Typesets: C
//
void Shutdown_Typesets(void)
{
}


//
//  Add_Parameter_Bits_Core: C
//
// This sets the bits in a bitset according to a type spec.  These accelerate
// the checking of tags when functions are called to not require string
// comparisons for things like <opt> or <skip>.
//
// Because this uses the stack, it cannot take the Param being built on the
// stack as input.  As a workaround the parameter class and flags to write
// are passed in.
//
// !!! R3-Alpha supported fixed word symbols for datatypes and typesets.
// Confusingly, this means that if you have said `word!: integer!` and use
// WORD!, you will get the integer type... but if WORD! is unbound then it
// will act as WORD!.  Also, is essentially having "keywords" and should be
// reviewed to see if anything actually used it.
//
Array* Add_Parameter_Bits_Core(
    Flags* flags,
    enum Reb_Param_Class pclass,
    Cell(const*) head,
    Cell(const*) tail,
    REBSPC *specifier
){
    StackIndex base = TOP_INDEX;
    *flags = 0;

    Cell(const*) item = head;
    for (; item != tail; ++item) {
        if (IS_TAG(item)) {
            bool strict = false;

            if (
                0 == CT_String(item, Root_Variadic_Tag, strict)
            ){
                // !!! The actual final notation for variadics is not decided
                // on, so there is compatibility for now with the <...> form
                // from when that was a TAG! vs. a 5-element TUPLE!  While
                // core sources were changed to `<variadic>`, asking users
                // to shuffle should only be done once (when final is known).
                //
                *flags |= PARAM_FLAG_VARIADIC;
            }
            else if (0 == CT_String(item, Root_End_Tag, strict)) {
                *flags |= PARAM_FLAG_ENDABLE;
                Init_Quasi_Word(PUSH(), Canon(NULL));

            }
            else if (0 == CT_String(item, Root_Maybe_Tag, strict)) {
                *flags |= PARAM_FLAG_NOOP_IF_VOID;
            }
            else if (0 == CT_String(item, Root_Opt_Tag, strict)) {
                Init_Quasi_Word(PUSH(), Canon(NULL));
            }
            else if (0 == CT_String(item, Root_Void_Tag, strict)) {
                Init_Any_Word_Bound(
                    PUSH(),
                    REB_WORD,
                    Canon(VOID_Q),
                    Lib_Context,
                    INDEX_ATTACHED
                );
            }
            else if (0 == CT_String(item, Root_Skip_Tag, strict)) {
                if (pclass != PARAM_CLASS_HARD)
                    fail ("Only hard-quoted parameters are <skip>-able");

                *flags |= PARAM_FLAG_SKIPPABLE;
                *flags |= PARAM_FLAG_ENDABLE; // skip => null
                Init_Quasi_Word(PUSH(), Canon(NULL));
            }
            else if (0 == CT_String(item, Root_Const_Tag, strict)) {
                *flags |= PARAM_FLAG_CONST;
            }
            else if (0 == CT_String(item, Root_None_Tag, strict)) {
                Init_Quasi_Void(PUSH());
            }
            else if (0 == CT_String(item, Root_Unrun_Tag, strict)) {
                // !!! Currently just commentary, degrading happens due
                // to type checking.  Review this.
            }
            else {
                fail (item);
            }
        }
        else {
            Derelativize(PUSH(), item, specifier);
            Clear_Cell_Flag(TOP, NEWLINE_BEFORE);
        }

        // !!! Review erroring policy--should probably not just be ignoring
        // things that aren't recognized here (!)
    }

    return Pop_Stack_Values_Core(base, NODE_FLAG_MANAGED);
}


//
//  MAKE_Parameter: C
//
Bounce MAKE_Parameter(
    Level(*) level_,
    enum Reb_Kind kind,
    Option(Value(const*)) parent,
    const REBVAL *arg
){
    UNUSED(kind);
    UNUSED(parent);
    return RAISE(Error_Bad_Make(REB_PARAMETER, arg));
}


//
//  TO_Parameter: C
//
Bounce TO_Parameter(Level(*) level_, enum Reb_Kind kind, const REBVAL *arg)
{
    return MAKE_Parameter(level_, kind, nullptr, arg);
}


//
//  MF_Parameter: C
//
void MF_Parameter(REB_MOLD *mo, NoQuote(Cell(const*)) v, bool form)
{
    if (not form) {
        Pre_Mold(mo, v);  // #[parameter! or make parameter!
    }

    DECLARE_LOCAL(temp);
    Option(const Array*) param_array = VAL_PARAMETER_ARRAY(v);
    if (param_array)
        Init_Block(temp, unwrap(param_array));
    else
        Init_Block(temp, EMPTY_ARRAY);

    Push_GC_Guard(temp);
    Mold_Or_Form_Value(mo, temp, form);
    Drop_GC_Guard(temp);

    if (not form) {
        End_Mold(mo);
    }
}


//
//  REBTYPE: C
//
REBTYPE(Parameter)
{
    UNUSED(level_);
    UNUSED(verb);

    fail (UNHANDLED);
}
