//
//  File: %c-augment.c
//  Summary: "Function generator for expanding the frame of an ACTION!"
//  Section: datatypes
//  Project: "Ren-C Language Interpreter and Run-time Environment"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2019-2021 Ren-C Open Source Contributors
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
//     >> foo-xy: augment ^foo-x [y [integer!]]
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
//     >> foo-xy: adapt (augment ^foo-x [y [integer!]]) [print ["y is" y]]
//
//     >> foo-xy 10 20
//     y is 20
//     x is 10
//
// AUGMENT leverages Ren-C's concept of "refinements are their own arguments"
// in order to allow normal parameters to be added to the frame after a
// refinement already has appeared.
//

#include "sys-core.h"

// See notes why the Augmenter gets away with reusing Specializer_Dispatcher
//
#define Augmenter_Dispatcher Specializer_Dispatcher
#define IDX_AUGMENTER_MAX 1


//
//  /augment: native [
//
//  "Create an action variant that acts the same, but has added parameters"
//
//      return: [action?]
//      original "Frame whose implementation is to be augmented"
//          [<unrun> frame!]
//      spec "Spec dialect for words to add to the derived function"
//          [block!]
//  ]
//
DECLARE_NATIVE(augment)
{
    INCLUDE_PARAMS_OF_AUGMENT;

    Action* augmentee = VAL_ACTION(ARG(original));
    Option(const Symbol*) label = VAL_FRAME_LABEL(ARG(original));

    // We reuse the process from Make_Paramlist_Managed_May_Fail(), which
    // pushes descriptors to the stack in groups for each parameter.

    StackIndex base = TOP_INDEX;
    StackIndex return_stackindex = 0;

    Flags flags = MKF_MASK_NONE;
    if (ACT_HAS_RETURN(augmentee)) {
        flags |= MKF_RETURN;
        return_stackindex = TOP_INDEX + 1;
    }

    // For each parameter in the original function, push a "quad"
    //
  blockscope {
    const Key* key_tail;
    const Key* key = ACT_KEYS(&key_tail, augmentee);
    const Param* param = ACT_PARAMS_HEAD(augmentee);
    for (; key != key_tail; ++key, ++param) {
        Init_Word(PUSH(), *key);
        Copy_Cell(PUSH(), param);

        // For any specialized (incl. local) parameters in the paramlist we are
        // copying, we want to "seal" them from view.  We wouldn't have access
        // to them if we were an ADAPT and not making a copy (since the action
        // in the exemplar would not match the phase).  So making a copy should
        // not suddenly subvert the access.
        //
        if (Is_Specialized(param))
            Set_Cell_Flag(TOP, STACK_NOTE_SEALED);
    }
  }

    VarList* adjunct = nullptr;

    // Now we reuse the spec analysis logic, which pushes more parameters to
    // the stack.  This may add duplicates--which will be detected when we
    // try to pop the stack into a paramlist.
    //
    Push_Keys_And_Parameters_May_Fail(
        &adjunct,
        ARG(spec),
        &flags,
        &return_stackindex
    );

    Array* paramlist = Pop_Paramlist_With_Adjunct_May_Fail(
        &adjunct,
        base,
        flags,
        return_stackindex
    );

    // Usually when you call Make_Action() on a freshly generated paramlist,
    // it notices that the rootvar is void and hasn't been filled in... so it
    // makes the frame the paramlist is the varlist of (the exemplar) have a
    // rootvar pointing to the phase of the newly generated action.
    //
    // But since AUGMENT itself doesn't add any new behavior, we can get away
    // with patching the augmentee's action information (phase and binding)
    // into the paramlist...and reusing the Specializer_Dispatcher.

    assert(Is_Unreadable(Flex_Head(Value, paramlist)));
    Tweak_Frame_Varlist_Rootvar(
        paramlist,
        ACT_IDENTITY(VAL_ACTION(ARG(original))),
        Cell_Frame_Coupling(ARG(original))
    );

    Phase* augmentated = Make_Action(
        paramlist,
        ACT_PARTIALS(augmentee),  // partials should still work
        &Augmenter_Dispatcher,
        IDX_AUGMENTER_MAX  // same as specialization, just 1 (for archetype)
    );

    assert(ACT_ADJUNCT(augmentated) == nullptr);
    mutable_ACT_ADJUNCT(augmentated) = adjunct;

    // Keep track that the derived keylist is related to the original, so
    // that it's possible to tell a frame built for the augmented function is
    // compatible with the original function (and its ancestors, too)
    //
    LINK(Ancestor, ACT_KEYLIST(augmentated)) = ACT_KEYLIST(augmentee);

    return Init_Action(OUT, augmentated, label, UNBOUND);
}
