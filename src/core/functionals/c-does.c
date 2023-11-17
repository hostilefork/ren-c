//
//  File: %c-does.c
//  Summary: "Expedient generator for 0-argument function specializations"
//  Section: datatypes
//  Project: "Ren-C Language Interpreter and Run-time Environment"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2018-2020 Ren-C Open Source Contributors
//
// See README.md and CREDITS.md for more information.
//
// Licensed under the GNU Lesser General Public License (LGPL), Version 3.0.
// You may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// https://www.gnu.org/licenses/lgpl-3.0.en.html
//
//=////////////////////////////////////////////////////////////////////////=//
//
// DOES in historical Rebol was simply a specialization of FUNC which assumed
// an empty spec block as a convenience.  It was thus in most other respects
// like a FUNC... e.g. it would catch throws of a RETURN signal.
//
// Ren-C experimentally pushes DOES a bit further.  Not only does it take
// blocks, but it can take any other data type that DO will accept...such as
// a FILE! or URL!:
//
//     >> d: does https://example.com/some-script.reb
//
//     >> d
//     ; Will act like `do https://example/some-script.reb`
//
// If it takes a block, it will treat it in a relativized way (as with FUNC)
// but it will not catch returns.  This means RETURN will be left bound as is.
// (Those who prefer getting RETURNs can just do `FUNC [] [...]`, this offers
// a unique alternative to that.)
//
//=//// NOTES ////////////////////////////////////////////////////////////=//
//
// * One experimental feature was removed, to allow specialization by example.
//   For instance `c: does catch [throw <like-this>]`.  This was inspired by
//   code golf.  However, it altered the interface (to quote its argument and
//   be variadic) and it also brought in distracting complexity that is better
//   kept in the implementations of REFRAMER and POINTFREE.
//

#include "sys-core.h"

enum {
    IDX_DOES_BLOCK = 1,  // Special case of BLOCK! to be executed
    IDX_DOES_MAX
};


//
//  Block_Dispatcher: C
//
// There are no arguments or locals to worry about in a DOES, nor does it
// heed any definitional RETURN.  This means that in many common cases we
// don't need to do anything special to a BLOCK! passed to DO...no copying
// or otherwise.  Just run it when the function gets called.
//
// Yet `does [...]` isn't *quite* like `specialize :do [source: [...]]`.  The
// difference is subtle, but important when interacting with bindings to
// fields in derived objects.  That interaction cannot currently resolve such
// bindings without a copy, so it is made on demand.
//
// (Luckily these copies are often not needed, such as when the DOES is not
// used in a method... -AND- it only needs to be made once.)
//
Bounce Block_Dispatcher(Level(*) const L)
{
    USE_LEVEL_SHORTHANDS (L);

    Details(*) details = Phase_Details(PHASE);
    assert(Array_Len(details) == IDX_DOES_MAX);

    Cell(*) block = Array_At(details, IDX_DOES_BLOCK);
        // ^-- note not a `Cell(const*) block`, may get updated!
    assert(IS_BLOCK(block) and VAL_INDEX(block) == 0);

    Array(const*) body = VAL_ARRAY(block);

    if (IS_SPECIFIC(block)) {
        if (Level_Binding(L) == UNBOUND)
            return DELEGATE(OUT, block);

        // Until "virtual binding" is implemented, we would lose L->binding's
        // ability to influence any variable lookups in the block if we did
        // not relativize it to this frame.  This is the only current way to
        // "beam down" influence of the binding for cases like:
        //
        // What forces us to copy the block are cases like this:
        //
        //     o1: make object! [a: 10 b: does [if true [a]]]
        //     o2: make o1 [a: 20]
        //     o2.b = 20
        //
        // While o2/b's ACTION! has a ->binding to o2, the only way for the
        // [a] block to get the memo is if it is relative to o2/b.  It won't
        // be relative to o2/b if it didn't have its existing relativism
        // Derelativize()'d out to make it specific, and then re-relativized
        // through a copy on behalf of o2/b.

        Array(*) relativized = Copy_And_Bind_Relative_Deep_Managed(
            SPECIFIC(block),
            PHASE,
            VAR_VISIBILITY_INPUTS  // no locals, does not matter
        );

        // Preserve file and line information from the original, if present.
        //
        if (Get_Array_Flag(body, HAS_FILE_LINE_UNMASKED)) {
            mutable_LINK(Filename, relativized) = LINK(Filename, body);
            relativized->misc.line = body->misc.line;
            Set_Array_Flag(relativized, HAS_FILE_LINE_UNMASKED);
        }

        // Update block cell as a relativized copy (we won't do this again).
        //
        Init_Relative_Block(block, PHASE, relativized);
    }

    assert(IS_RELATIVE(block));

    return DELEGATE_CORE(
        OUT,  // output
        LEVEL_MASK_NONE,
        SPC(L->varlist),  // branch specifier
        block  // branch
    );
}


//
//  surprise: native [
//
//  {Generate a surprise ANY-VALUE!}
//
//      return: [<opt> <void> any-value!]
//  ]
//
DECLARE_NATIVE(surprise)
//
// !!! DOES needed a specialization for block that had no arguments and no
// constraint on the return value.  That's a pretty weird function spec, and
// the only thing I could think of was something that just surprises you with
// a random value.  Neat idea, but writing it isn't the purpose...getting the
// spec was, so it's punted on for now.  See uses of Lib(SURPRISE)
{
    INCLUDE_PARAMS_OF_SURPRISE;
    return nullptr;  // It just returned null.  Surprised?
}


//
//  does: native [
//
//  {Make action that will DO a value (more optimized than SPECIALIZE :DO)}
//
//      return: [activation?]
//      source "Note: Will LOCK source if a BLOCK! (review behavior)"
//          [any-value!]
//  ]
//
DECLARE_NATIVE(does)
//
// !!! Note: `does [...]` and `does [do [...]]` are not exactly the same.  The
// generated ACTION! of the first form uses Block_Dispatcher() and does
// on-demand relativization, so it's "kind of like" a `func []` in forwarding
// references to members of derived objects.  Also, it is optimized to not run
// the block with the DO native...hence a HIJACK of DO won't be triggered by
// invocations of the first form.
//
// !!! There were more experimental behaviors that were culled, like quoting
// the first argument and acting on it to get `foo: does append block <foo>`
// notation without a block.  This concept from code golf was thought to
// possibly be useful and "natural" for other cases.  However, that idea
// has been generalized through REFRAMER, so those who wish to experiemnt
// with it can do it that way.
{
    INCLUDE_PARAMS_OF_DOES;

    REBVAL *source = ARG(source);

    if (IS_BLOCK(source)) {
        Phase(*) doer = Make_Action(
            ACT_PARAMLIST(VAL_ACTION(Lib(SURPRISE))),  // same, no args
            nullptr,  // no partials
            &Block_Dispatcher,  // **SEE COMMENTS**, not quite like plain DO!
            IDX_DOES_MAX  // details array capacity
        );

        // Block_Dispatcher() *may* copy at an indeterminate time, so to keep
        // things invariant we have to lock it.
        //
        Cell(*) body = Array_At(Phase_Details(doer), IDX_DOES_BLOCK);
        Force_Value_Frozen_Deep(source);
        Copy_Cell(body, source);

        return Init_Activation(OUT, doer, ANONYMOUS, UNBOUND);
    }

    // On all other types, we just make it act like a specialized call to
    // DO for that value.

    Context(*) exemplar = Make_Context_For_Action(
        Lib(DO),
        TOP_INDEX,  // lower dsp would be if we wanted to add refinements
        nullptr  // don't set up a binder; just poke specializee in frame
    );
    assert(Get_Series_Flag(CTX_VARLIST(exemplar), MANAGED));

    // Put argument into DO's *second* frame slot (first is RETURN)
    //
    assert(KEY_SYM(CTX_KEY(exemplar, 1)) == SYM_RETURN);
    Copy_Cell(CTX_VAR(exemplar, 2), source);

    Symbol(const*) label = Canon(DO);  // !!! Better answer?

    Phase(*) doer = Make_Action_From_Exemplar(exemplar, label);
    return Init_Activation(OUT, doer, label, UNBOUND);
}
