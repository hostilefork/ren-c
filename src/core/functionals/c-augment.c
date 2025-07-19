//
//  file: %c-augment.c
//  summary: "Function generator for expanding the frame of an ACTION!"
//  section: datatypes
//  project: "Ren-C Language Interpreter and Run-time Environment"
//  homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2019-2025 Ren-C Open Source Contributors
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
// AUGMENT is designed to create a version of a function with an expanded
// frame, adding new parameters.  It does so without affecting the execution:
//
//     >> foo-x: func [x [integer!]] [print ["x is" x]]
//     >> foo-xy: augment foo-x/ [y [integer!]]
//
//     >> foo-x 10
//     x is 10
//
//     >> foo-xy 10
//     ** Error: foo-xy is missing its y argument
//
//     >> foo-xy 10 20
//     x is 10
//
// The original function doesn't know about the added parameters, so this is
// is only useful when combined with something like ADAPT or ENCLOSE... to
// inject in phases of code at a higher level that see these parameters:
//
//     >> foo-xy: adapt (augment foo-x/ [y [integer!]]) [print ["y is" y]]
//
//     >> foo-xy 10 20
//     y is 20
//     x is 10
//
//=//// NOTES /////////////////////////////////////////////////////////////=//
//
// * AUGMENT in historical Redbol would have been complicated by the idea
//   that refinements could span multiple arguments.  So if a function's spec
//   was `[arg1 /refine refarg1 refarg2]` then augmenting it would look like
//   `[arg1 /refine refarg1 refarg2 arg2]` and seem like it was adding an
//   argument to the refinement.  Since Ren-C refinements are the name of
//   the single argument they represent, this is not a problem.
//

#include "sys-core.h"


//
//  augment: native [
//
//  "Create an action variant that acts the same, but has added parameters"
//
//      return: [action!]
//      original "Frame whose implementation is to be augmented"
//          [<unrun> frame!]
//      spec "Spec dialect for words to add to the derived function"
//          [block!]
//  ]
//
DECLARE_NATIVE(AUGMENT)
{
    INCLUDE_PARAMS_OF_AUGMENT;

    Element* spec = Element_ARG(SPEC);
    Element* original = Element_ARG(ORIGINAL);

    Option(const Symbol*) label = Cell_Frame_Label_Deep(original);
    Option(VarList*) coupling = Cell_Frame_Coupling(original);

    Phase* augmentee = Frame_Phase(original);

    VarList* adjunct = nullptr;

  copy_augmentee_parameters: { ////////////////////////////////////////////=//

    // We reuse the process from Trap_Make_Paramlist_Managed(), which pushes
    // WORD! and PARAMETER! pairs for each argument.
    //
    // 1. For any specialized (including local) parameters in the paramlist
    //    we are copying, we want to "seal" them from view.  We wouldn't have
    //    access to them if we were an ADAPT and not making a copy (since the
    //    action in the exemplar would not match the phase).  So making a copy
    //    should not suddenly subvert the access.

    const Key* key_tail;
    const Key* key = Phase_Keys(&key_tail, augmentee);
    const Param* param = Phase_Params_Head(augmentee);
    for (; key != key_tail; ++key, ++param) {
        Init_Word(PUSH(), *key);
        Copy_Cell(PUSH(), param);

        if (Is_Specialized(param))
            Set_Cell_Flag(TOP, STACK_NOTE_SEALED);  // seal parameters [1]
    }

} add_parameters_from_spec: { /////////////////////////////////////////////=//

    Option(Error*) e = Trap_Push_Keys_And_Params(
        &adjunct,
        spec,
        MKF_PARAMETER_SEEN,  // don't assume description string
        SYM_0  // if original had no return, we don't add
    );
    if (e)
        return PANIC(unwrap e);

} pop_paramlist_and_init_action: { /////////////////////////////////////////=//

    // The augmented action adds parameters but doesn't add any new behavior.
    // Hence we don't need a new Details, and can get away with patching the
    // augmentee's action information (phase and coupling) into the paramlist.

    Phase* prior = Frame_Phase(ARG(ORIGINAL));
    Option(VarList*) prior_coupling = Cell_Frame_Coupling(ARG(ORIGINAL));

    ParamList* paramlist;
    Option(Error*) e = Trap_Pop_Paramlist(  // checks for duplicates
        &paramlist, STACK_BASE, prior, prior_coupling
    );
    if (e)
        return PANIC(unwrap e);

    assert(Misc_Phase_Adjunct(paramlist) == nullptr);
    Tweak_Misc_Phase_Adjunct(paramlist, adjunct);

    Init_Action(OUT, paramlist, label, coupling);
    return UNSURPRISING(OUT);
}}
