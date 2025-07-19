//
//  file: %n-do.c
//  summary: "native functions for DO, EVAL, APPLY"
//  section: natives
//  project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  homepage: https://github.com/metaeducation/ren-c/
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
// Ren-C's philosophy of DO is that the argument to it represents a place to
// find source code.  Hence `DO 3` does not evaluate to the number 3, any
// more than `DO "print hello"` would evaluate to `"print hello"`.  If a
// generalized evaluator is needed, use the special-purpose REEVAL(UATE).
//
// Note that although the code for running blocks and frames is implemented
// here as C, the handler for processing STRING!, FILE!, TAG!, URL!, etc. is
// dispatched out to some Rebol code that implements DO.
//

#include "sys-core.h"


//
//  reeval: native [
//
//  "Process an evaluated argument *inline* as an evaluator step would"
//
//      return: [any-atom?]
//      value "BLOCK! passes-thru, ACTION! runs, SET-WORD! assigns..."
//          [element?]
//      expressions "Depending on VALUE, more expressions may be consumed"
//          [<opt> element? <variadic>]
//  ]
//
DECLARE_NATIVE(REEVAL)
{
    INCLUDE_PARAMS_OF_REEVAL;

    // REEVAL only *acts* variadic, but uses ST_STEPPER_REEVALUATING
    //
    UNUSED(ARG(EXPRESSIONS));

    Element* v = Element_ARG(VALUE);

    Flags flags = FLAG_STATE_BYTE(ST_STEPPER_REEVALUATING);

    Level* sub = Make_Level(&Stepper_Executor, level_->feed, flags);
    Copy_Cell(Evaluator_Level_Current(sub), v);  // evaluator's CURRENT
    Force_Invalidate_Gotten(&sub->u.eval.current_gotten);

    if (Trampoline_Throws(OUT, sub))  // review: rewrite stackless
        return THROWN;

    Clear_Cell_Flag(OUT, OUT_HINT_UNSURPRISING);
    return OUT;
}


//
//  shove: native [
//
//  "Shove a parameter into an ACTION! as its first argument"
//
//      return: [any-atom?]
//      'left "Hard literal, will be processed according to right's first arg"
//          [element?]
//      'right "Arbitrary variadic feed of expressions on the right"
//          [<variadic> <end> element?]
//  ]
//
DECLARE_NATIVE(SHOVE)
//
// PATH!s do not do infix lookup in Rebol, and there are good reasons for this
// in terms of both performance and semantics.  However, it is sometimes
// needed to dispatch via a path--for instance to call an infix function that
// lives in a context.
//
// The SHOVE operation is used to push values from the left to act as the
// first argument of an operation, e.g.:
//
//      >> 10 ->- lib/(print "Hi!" first [multiply]) 20
//      Hi!
//      200
//
// It's becoming more possible to write something like this in usermode, but
// it would be inefficient, and there are binding problems to worry about
// in macro-like code.
{
    INCLUDE_PARAMS_OF_SHOVE;

    Level* L;
    if (not Is_Level_Style_Varargs_May_Panic(&L, ARG(RIGHT)))
        return PANIC("SHOVE (>-) not implemented for MAKE VARARGS! [...] yet");

    Element* left = Element_ARG(LEFT);

    if (Is_Level_At_End(L))  // shouldn't be for WORD!/PATH! unless APPLY
        return COPY(ARG(LEFT));  // ...because evaluator wants `help <-` to work

  //=//// RESOLVE ACTION ON RIGHT (LOOKUP VAR, EVAL GROUP...) /////////////=//
  //
  // 1. At one point, it was allowed to shove into set-words etc:
  //
  //        >> 10 ->- x:
  //        >> x
  //        == 10
  //
  //    Is that useful enough to bother supporting?

    Value* shovee = ARG(RIGHT); // reuse variadic arg cell for the shoved-into
    Option(const Symbol*) label = nullptr;

    const Element* right = At_Level(L);
    if (
        Is_Word(right) or Is_Tuple(right)
        or Is_Path(right) or Is_Chain(right)
    ){
        Sink(Value) out = OUT;
        Option(Error*) e = Trap_Get_Var(
            out,  // can't eval directly into arg slot
            NO_STEPS,
            At_Level(L),
            Level_Binding(L)
        );
        if (e)
            return PANIC(unwrap e);

        Move_Cell(shovee, out);  // variable contents always stable
    }
    else if (Is_Group(right)) {
        if (Eval_Any_List_At_Throws(OUT, right, Level_Binding(L)))
            return THROWN;

        Move_Cell(shovee, Decay_If_Unstable(OUT));
    }
    else
        Copy_Cell(shovee, right);

    Deactivate_If_Action(shovee);  // allow ACTION! to be run

    Option(InfixMode) infix_mode;
    if (Is_Frame(shovee)) {
        if (not label)
            label = Cell_Frame_Label_Deep(shovee);
        infix_mode = Cell_Frame_Infix_Mode(shovee);
    }
    else {
        return PANIC(  // used to allow shoving into set-words, but... [1]
            "SHOVE's immediate right must be FRAME! at this time"
        );
    }

    Fetch_Next_In_Feed(L->feed);

  //=//// PROCESS LITERALLY-TAKEN LEFT FOR PARAMETER CONVENTION ///////////=//
  //
  // 1. Because the SHOVE operator takes the left hand side as a hard literal,
  //    evaluating that and shoving into a right hand infix function will
  //    out-prioritize an infix operation's completion on the left:
  //
  //        >> 1 + (1 + 1) * 3
  //        == 9  ; e.g. (1 + (1 + 1)) * 3
  //
  //        >> 1 + (1 + 1) ->- lib/* 3
  //        == 7  ; e.g. 1 + ((1 + 1) * 3)
  //
  //    So it's not a precise match for evaluative left hand side semantics.
  //    Offering any alternatives or workarounds besides "put your left hand
  //    side in a group" is more complicated than it's possibly worth.
  //
  // 2. It's considered a generally bad idea to allow functions to get access
  //    to the binding environment of the callsite.  That interferes with
  //    abstraction, so any binding should

    const Param* param = First_Unspecialized_Param(
        nullptr, Cell_Frame_Phase(shovee)
    );
    ParamClass pclass = Cell_Parameter_Class(param);

    switch (Cell_Parameter_Class(param)) {
      case PARAMCLASS_NORMAL:  // we can't *quite* match evaluative infix [1]
      case PARAMCLASS_META: {
        Flags flags = LEVEL_MASK_NONE;
        if (Eval_Element_Core_Throws(OUT, flags, left, Level_Binding(L)))
            return THROWN;
        if (pclass == PARAMCLASS_NORMAL)
            Decay_If_Unstable(OUT);
        else {
            // The infix fulfillment code will Liftify() OUT
        }
        break; }

      case PARAMCLASS_JUST:  // took the input as hard literal, so it's good
        Copy_Cell(OUT, left);
        break;

      case PARAMCLASS_THE:  // cheat and do something usermode can't ATM [2]
        Derelativize(OUT, left, Level_Binding(L));
        break;

      case PARAMCLASS_SOFT:  // !!! can we trust infix to just do this part?
        Derelativize(OUT, left, Level_Binding(L));
        break;

      default:
        assert(false);
        break;
    }

  //=//// DISPATCH WITH FIRST ARG IN OUT SLOT /////////////////////////////=//
  //
  // 1. This uses the infix mechanic regardless of whether the function we are
  //    shoving into is infix or not.  It's the easiest way to get the
  //    argument into the first slot of the function.
  //
  // 2. While the evaluator state may be geared to running infix parameter
  //    acquisition, we still pass in a flag to Begin_Action() so that it
  //    knows whether it was infix or not.  This makes a difference, e.g.:
  //
  //        >> 1 + 2 ->- negate * 3
  //

    Flags flags = FLAG_STATE_BYTE(ST_ACTION_INITIAL_ENTRY_INFIX);  // [1]

    Level* sub = Make_Level(&Action_Executor, level_->feed, flags);
    Push_Action(sub, shovee, infix_mode);  // can know if it's infix [2]

    Push_Level_Erase_Out_If_State_0(OUT, sub);
    return DELEGATE_SUBLEVEL(sub);
}


//
//  evaluate: native [
//
//  "Run a list through the evaluator iteratively, or take a single step"
//
//      return: "Evaluation product, or ~[position product]~ pack if :STEP"
//          [any-atom?]  ; :STEP changes primary return product [1]
//      source [
//          <opt-out>  ; useful for `evaluate opt ...` scenarios
//          any-list? get-group? set-group? get-block? set-block?  ; code
//          <unrun> frame!  ; invoke the frame (no arguments, see RUN)
//          warning!  ; panic on the error
//          varargs!  ; simulates as if frame! or block! is being executed
//      ]
//      :step "Do one step of evaluation (return null position if at tail)"
//  ]
//
DECLARE_NATIVE(EVALUATE)  // synonym as EVAL in mezzanine
//
// 1. When operating stepwise, the primary result shifts to be the position,
//    to be more useful for knowing if there are more steps to take.  It also
//    helps prevent misunderstandings if the first value of a multi-return
//    cannot itself be a multi-return pack:
//
//      https://forum.rebol.info/t/re-imagining-eval-next/767
//
// 2. PANIC is the preferred operation for raising divergent errors, as it has
//    a natural behavior for blocks passed to construct readable messages and
//    (PANIC X) more clearly communicates a panic than (EVAL X).  But EVAL of
//    an ERROR! would have to panic anyway, so it might as well use the one
//    it is given.
//
// 3. It might seem that since EVAL [] is VOID, that EVAL:STEP [] should make
//    a VOID.  But in practice, there's a dummy step at the end of every
//    enumeration, e.g. EVAL [1 + 2 10 + 20] goes through three steps, where
//    the third step is over [].  If we were to say that "step" produced
//    anything, it would be GHOST...because that step does not contribute to
//    the output (the result is 30).  But we want to distinguish the case of
//    a step that produced a GHOST from hitting the end, via nullptr.
{
    INCLUDE_PARAMS_OF_EVALUATE;

    Element* source = Element_ARG(SOURCE);

    enum {
        ST_EVALUATE_INITIAL_ENTRY = STATE_0,
        ST_EVALUATE_SINGLE_STEPPING,
        ST_EVALUATE_RUNNING_TO_END
    };

    switch (STATE) {
      case ST_EVALUATE_INITIAL_ENTRY: {
        Remember_Cell_Is_Lifeguard(source);  // may be only reference!

        if (Is_Chain(source)) {  // e.g. :(...) or [...]:
            Unchain(source);
            assert(Any_List(source));
            goto initial_entry_list;
        }
        if (Any_List(source))
            goto initial_entry_list;

        if (Is_Frame(source))
            goto initial_entry_frame;

        if (Is_Varargs(source))
            goto initial_entry_varargs;

        assert(Is_Warning(source));
        return PANIC(Cell_Error(source)); }  // would panic anyway [2]

      case ST_EVALUATE_SINGLE_STEPPING:
        if (Is_Endlike_Tripwire(OUT)) {
            Drop_Level(SUBLEVEL);
            return nullptr;  // no result, not even GHOST [3]
        }
        goto single_step_dual_in_out;


      default: assert(false);
    }

  initial_entry_list: {  /////////////////////////////////////////////////////

    // 1. !!! Right now all EVALUATE calls treat ANY-LIST? the same.  (e.g.
    //    ^[1 + 2] just does the same thing as [1 + 2] and gives 3, not '3
    //    or any other variation.  Should lists vary their behavior?  It
    //    has been considered to make GROUP!s be ghostable and BLOCK!s not
    //    so that it's guided by the type, but that's likely not useful.
    //
    // 2. EVAL can return any result, including GHOST!.  We don't want to be
    //    too casual about ghosts vanishing, e.g.:
    //
    //        ^result: (mode: <processing> eval code)
    //
    //    If that EVAL returns a GHOST!, we wouldn't want result to get the
    //    <processing> tag as its value.  Hence the ghosts that EVAL returns
    //    must be "surprising" so Evaluator_Executor() doesn't erase them.
    //
    //    (We can't count on the RETURN: type check to do this, because
    //    natives do not run typechecking in release builds.)

    Flags flags = LEVEL_FLAG_FORCE_SURPRISING;

    Level* sub = Make_Level_At(
        Bool_ARG(STEP) ? &Stepper_Executor : &Evaluator_Executor,
        source,  // all lists treated the same [1]
        flags
    );
    if (not Bool_ARG(STEP))
        Init_Surprising_Ghost(Evaluator_Primed_Cell(sub));  // don't vanish [2]
    Push_Level_Erase_Out_If_State_0(OUT, sub);

    if (not Bool_ARG(STEP))  // plain evaluation to end, maybe void/ghost
        return DELEGATE_SUBLEVEL(sub);

    Set_Level_Flag(sub, TRAMPOLINE_KEEPALIVE);  // to ask how far it got

    STATE = ST_EVALUATE_SINGLE_STEPPING;
    return CONTINUE_SUBLEVEL(sub);

} initial_entry_frame: { /////////////////////////////////////////////////////

    // 1. It's an open question of whether something like a BLOCK! is a good
    //    enough encoder of the evaluator state to be the result of an
    //    operation like EVAL:STEP, or if something like a FRAME! would be
    //    a better way to abstract things like "accumulated LETs".  It may
    //    evolve that EVAL:STEP on a BLOCK! actually produces a FRAME!...

    if (Bool_ARG(STEP))  // !!! may be legal (or mandatory) in the future [1]
        return PANIC(":STEP not implemented for FRAME! in EVALUATE");

    if (Not_Base_Readable(CELL_FRAME_PAYLOAD_1_PHASE(source)))
        return PANIC(Error_Series_Data_Freed_Raw());

    Option(const Atom*) with = nullptr;
    Push_Frame_Continuation(
        OUT,
        LEVEL_MASK_NONE,
        source,
        with
    );
    return BOUNCE_DELEGATE;

} initial_entry_varargs: { ///////////////////////////////////////////////////

    // 1. We can execute the array, but we must "consume" elements out of it
    //    (e.g. advance the index shared across all instances)
    //
    //    !!! If any VARARGS! op does not honor the "locked" flag on the
    //    array during execution, there will be problems if it is TAKE'n
    //    or EVAL'd while this operation is in progress.
    //
    // 2. A BLOCK! varargs doesn't technically need to "go bad" on a throw,
    //    since the block is still around.  But a FRAME! varargs does.
    //
    // 3. By definition, we are in the middle of a function call in the level
    //    the varargs came from.  It's still on the stack, and we don't want
    //    to disrupt its state.  Use a sublevel.

    if (Bool_ARG(STEP))
        return PANIC(":STEP not implemented for VARARGS! in EVALUATE");

    Element* position;
    if (Is_Block_Style_Varargs(&position, source)) {  // must consume [1]
        if (Eval_Any_List_At_Throws(OUT, position, SPECIFIED)) {
            Init_Unreadable(position);  // "goes bad" for consistency [2]
            return THROWN;
        }

        Erase_Cell(position); // convention for shared data at endpoint

        return OUT;
    }

    Level* L;
    if (not Is_Level_Style_Varargs_May_Panic(&L, source))
        crash (source); // Frame is the only other type

    if (Is_Level_At_End(L))
        return VOID;

    Level* sub = Make_Level(  // need to do evaluation in a sublevel [3]
        &Evaluator_Executor, L->feed, LEVEL_MASK_NONE
    );
    Push_Level_Erase_Out_If_State_0(OUT, sub);
    return DELEGATE_SUBLEVEL(sub);

} single_step_dual_in_out: {  ////////////////////////////////////////////////

    // 1. There may have been a LET statement in the code.  If there was, we
    //    have to incorporate the binding it added into the reported state
    //    *somehow*.  Right now we add it to the block we give back...this
    //    gives rise to questionable properties, such as if the user goes
    //    backward in the block and were to evaluate it again:
    //
    //      https://forum.rebol.info/t/1496
    //
    //    Right now we can politely ask "don't do that".  But better would
    //    probably be to make EVALUATE return something with more limited
    //    privileges... more like a FRAME!/VARARGS!.

    assert(Bool_ARG(STEP));

    Forget_Cell_Was_Lifeguard(source);  // unprotect so we can edit for return

    Context* binding = Level_Binding(SUBLEVEL);
    VAL_INDEX_UNBOUNDED(source) = Level_Array_Index(SUBLEVEL);  // new index
    Drop_Level(SUBLEVEL);

    Tweak_Cell_Binding(source, binding);  // integrate LETs [1]

    if (Bool_ARG(STEP)) {
        Source* pack = Make_Source_Managed(2);
        Set_Flex_Len(pack, 2);
        Copy_Lifted_Cell(Array_At(pack, 0), source);  // pack wants META values
        Move_Lifted_Atom(Array_At(pack, 1), OUT);  // may be ERROR!

        Init_Pack(OUT, pack);
    }

    return OUT;
}}


//
//  eval-free: native [
//
//  "Optimized version of EVAL that frees its target frame"
//
//      return: [any-atom?]
//      frame [frame!]
//  ]
//
DECLARE_NATIVE(EVAL_FREE)
{
    INCLUDE_PARAMS_OF_EVAL_FREE;

    Value* frame = ARG(FRAME);

    enum {
        ST_EVAL_FREE_INITIAL_ENTRY = STATE_0,
        ST_EVAL_FREE_EVALUATING
    };

    switch (STATE) {
      case ST_EVAL_FREE_INITIAL_ENTRY: goto initial_entry;
      case ST_EVAL_FREE_EVALUATING: goto result_in_out;
      default: assert(false);
    }

  initial_entry: { ///////////////////////////////////////////////////////////

    if (Not_Base_Readable(CELL_FRAME_PAYLOAD_1_PHASE(frame)))
        return PANIC(Error_Series_Data_Freed_Raw());

    if (Is_Stub_Details(Cell_Frame_Phase(frame)))
        panic ("Can't currently EVAL-FREE a Details-based Stub");

    VarList* varlist = Cell_Varlist(frame);

    if (Level_Of_Varlist_If_Running(varlist))
        panic ("Use REDO to restart a running FRAME! (not EVAL-FREE)");

    Level* L = Make_End_Level(
        &Action_Executor,
        FLAG_STATE_BYTE(ST_ACTION_TYPECHECKING)
    );

    Set_Action_Level_Label(L, Cell_Frame_Label_Deep(frame));

    L->varlist = Varlist_Array(varlist);
    L->rootvar = Rootvar_Of_Varlist(varlist);
    if (MISC_VARLIST_ADJUNCT(varlist) != nullptr)  // might have adjunct
        assert(Get_Stub_Flag(varlist, MISC_NEEDS_MARK));
    Clear_Stub_Flag(varlist, MISC_NEEDS_MARK);
    Tweak_Misc_Runlevel(varlist, L);  // wipes out any adjunct

    Phase* phase = Level_Phase(L);
    assert(phase == Cell_Frame_Phase(
        Phase_Archetype(cast(ParamList*, varlist)))
    );
    Tweak_Level_Coupling(L, Cell_Frame_Coupling(frame));

    L->u.action.original = phase;

    L->u.action.key = Phase_Keys(&L->u.action.key, phase);
    L->u.action.param = Phase_Params_Head(phase);
    L->u.action.arg = L->rootvar + 1;

    Begin_Action(L, PREFIX_0);

    Push_Level_Erase_Out_If_State_0(OUT, L);

    STATE = ST_EVAL_FREE_EVALUATING;
    return CONTINUE_SUBLEVEL(L);

} result_in_out: { ///////////////////////////////////////////////////////////

    /*Diminish_Stub(Cell_Frame_Phase(frame));  // the "FREE" of EVAL-FREE*/
    // fix

    return OUT;
}}


//
//  applique: native [
//
//  "Invoke an ACTION! with all required arguments specified"
//
//      return: [any-atom?]
//      operation [<unrun> frame!]
//      def "Frame definition block (will be bound and evaluated)"
//          [block!]
//      <local> frame  ; GC-safe cell for frame
//  ]
//
DECLARE_NATIVE(APPLIQUE)
//
// 1. Make a FRAME! for the ACTION!, weaving in the ordered refinements
//    collected on the stack (if any).  Any refinements that are used in any
//    specialization level will be pushed as well, which makes them out
//    prioritize (e.g. higher-ordered) than any used in a PATH! that were
//    pushed during the Get of the ACTION!.
{
    INCLUDE_PARAMS_OF_APPLIQUE;

    Element* op = Element_ARG(OPERATION);
    Element* def = Element_ARG(DEF);

    enum {
        ST_APPLIQUE_INITIAL_ENTRY = STATE_0,
        ST_APPLIQUE_RUNNING_DEF_BLOCK
    };

    switch (STATE) {
      case ST_APPLIQUE_INITIAL_ENTRY :
        goto initial_entry;

      case ST_APPLIQUE_RUNNING_DEF_BLOCK :
        goto definition_result_in_spare;

      default: assert(false);
    }

  initial_entry: {  //////////////////////////////////////////////////////////

    ParamList* exemplar = Make_Varlist_For_Action_Push_Partials(  // [1]
        op,
        STACK_BASE,  // lowest_stackindex of refinements to weave in
        nullptr,  // no binder needed
        g_tripwire  // fill all slots with nothing to start
    );
    Manage_Flex(exemplar);
    Init_Lensed_Frame(
        LOCAL(FRAME),
        exemplar,
        Cell_Frame_Phase(op),
        Cell_Frame_Coupling(op)
    );

    Drop_Data_Stack_To(STACK_BASE);  // refinement order unimportant

    Use* use = Alloc_Use_Inherits_Core(
        USE_FLAG_SET_WORDS_ONLY,
        Cell_List_Binding(def)
    );
    Copy_Cell(Stub_Cell(use), Element_LOCAL(FRAME));

    Tweak_Cell_Binding(def, use);

    STATE = ST_APPLIQUE_RUNNING_DEF_BLOCK;
    return CONTINUE(SPARE, def);  // first run block bound to frame

} definition_result_in_spare: {  /////////////////////////////////////////////

    return DELEGATE(OUT, Element_LOCAL(FRAME));  // now run the frame
}}


//
//  Native_Frame_Filler_Core: C
//
// This extracts the code for turning a BLOCK! into a partially (or fully)
// filled FRAME!.  It's shared between SPECIALIZE and APPLY.
//
Bounce Native_Frame_Filler_Core(Level* level_)
{
    INCLUDE_PARAMS_OF_APPLY;

    Element* op = Element_ARG(OPERATION);
    Element* args = Element_ARG(ARGS);

    Element* frame;
    Element* iterator;

    Value* var;  // may come from evars iterator or found by index
    Param* param;  // (same)

    if (STATE != ST_FRAME_FILLER_INITIAL_ENTRY)
        goto not_initial_entry;

  initial_entry: {  //////////////////////////////////////////////////////////

  // 1. Make a FRAME! for the ACTION!, weaving in the ordered refinements
  //    collected on the stack (if any).  Any refinements that are used in any
  //    specialization level will be pushed, which makes them out-prioritize
  //    (e.g. higher-ordered) than any used in a CHAIN! that were pushed
  //    during the Get of the ACTION!.
  //
  // 2. Binders cannot be held across evaluations at this time.  Do slow
  //    lookups for refinements, but this is something that needs rethinking.
  //
  // 3. Varlist_Archetype(exemplar) is phased, sees locals

    ParamList* exemplar = Make_Varlist_For_Action_Push_Partials(  // [1]
        op,
        STACK_BASE,  // lowest_stackindex of refinements to weave in
        nullptr,  // doesn't use a Binder [2]
        nullptr  // leave unspecialized slots as antiform parameter!
    );
    Manage_Flex(exemplar); // Putting into a frame
    frame = Init_Frame(
        LOCAL(FRAME),
        exemplar,
        Cell_Frame_Label(op),
        Cell_Frame_Coupling(op)
    );
    Remember_Cell_Is_Lifeguard(frame);

    Drop_Data_Stack_To(STACK_BASE);  // partials ordering unimportant

    Level* L = Make_Level_At(
        &Stepper_Executor,
        args,
        LEVEL_FLAG_TRAMPOLINE_KEEPALIVE
    );
    Push_Level_Erase_Out_If_State_0(SPARE, L);

    EVARS *e = Try_Alloc_Memory(EVARS);
    Init_Evars(e, frame);  // sees locals [3]

    iterator = Init_Handle_Cdata(LOCAL(ITERATOR), e, sizeof(EVARS));
    STATE = ST_FRAME_FILLER_INITIALIZED_ITERATOR;
    Enable_Dispatcher_Catching_Of_Throws(LEVEL);  // finalize_maybe_throwing

    goto handle_next_item;

} not_initial_entry: { ///////////////////////////////////////////////////////

    // After the initial entry, we can take for granted that the FRAME and
    // ITERATOR locals are initialized.

    frame = Element_LOCAL(FRAME);
    iterator = Element_LOCAL(ITERATOR);

    switch (STATE) {
      case ST_FRAME_FILLER_INITIALIZED_ITERATOR:
        assert(Is_Throwing_Panic(LEVEL));  // this dispatcher panic()'d
        goto finalize_maybe_throwing;

      case ST_FRAME_FILLER_LABELED_EVAL_STEP:
        if (THROWING)
            goto finalize_maybe_throwing;
        goto labeled_step_dual_in_spare;

      case ST_FRAME_FILLER_UNLABELED_EVAL_STEP:
        if (THROWING)
            goto finalize_maybe_throwing;
        if (Not_Cell_Readable(iterator)) {
            assert(Bool_ARG(RELAX));
            goto handle_next_item;
        }
        goto unlabeled_step_dual_in_spare;

      default : assert(false);
    }

} handle_next_item: {  ///////////////////////////////////////////////////////

    Level* L = SUBLEVEL;

    if (Is_Level_At_End(L))
        goto finalize_maybe_throwing;

    const Element* at = At_Level(L);

    if (Is_Comma(at)) {
        Fetch_Next_In_Feed(L->feed);
        goto handle_next_item;
    }

  #if RUNTIME_CHECKS
    Corrupt_Pointer_If_Debug(param);
  #endif

    Option(SingleHeart) single;
    if (
        not Is_Chain(at)
        or not (single = Try_Get_Sequence_Singleheart(at))
        or not (Singleheart_Has_Trailing_Space(unwrap single))
    ){
        if (Not_Cell_Readable(iterator))
            goto handle_discarded_item;

        goto handle_unlabeled_item;
    }

  handle_labeled_item: {  // REFINEMENT: names next arg

  // 1. We could do (negate // [('number): 10]) or (negate // [1: 10]) etc.
  //    Not a priority at the moment--higher priority is to share this code
  //    with SPECIALIZE.
  //
  // 2. Two argument-name labels in a row is not legal...treat it like the
  //    next refinement is reaching a comma or end of block.  (Though this
  //    could be treated as an <end> case?)

    if (single != TRAILING_SPACE_AND(WORD))  // more possibilities later [1]
        return PANIC("Only WORD!: labels handled in APPLY at this time");

    STATE = ST_FRAME_FILLER_LABELED_EVAL_STEP;

    const Symbol* symbol = Cell_Word_Symbol(At_Level(L));

    Option(Index) index = Find_Symbol_In_Context(frame, symbol, false);
    if (not index)
        return PANIC(Error_Bad_Parameter_Raw(at));

    var = Slot_Hack(Varlist_Slot(Cell_Varlist(frame), unwrap index));
    param = Phase_Param(Cell_Frame_Phase(op), unwrap index);

    if (not Is_Parameter(var))
        return PANIC(Error_Bad_Parameter_Raw(at));

    Sink(Value) lookback = SCRATCH;  // for error
    Copy_Cell(lookback, At_Level(L));
    Fetch_Next_In_Feed(L->feed);
    at = Try_At_Level(L);

    if (at == nullptr or Is_Comma(at))
        return PANIC(Error_Need_Non_End_Raw(lookback));

    if (  // catch e.g. DUP: LINE: [2]
        Is_Chain(at)
        and (single = Try_Get_Sequence_Singleheart(at))
        and Singleheart_Has_Trailing_Space(unwrap single)
    ){
        return PANIC(Error_Need_Non_End_Raw(lookback));
    }

    Init_Integer(ARG(INDEX), unwrap index);
    goto eval_step_maybe_labeled;

}} handle_unlabeled_item: { //////////////////////////////////////////////////

    STATE = ST_FRAME_FILLER_UNLABELED_EVAL_STEP;

    EVARS *e = Cell_Handle_Pointer(EVARS, iterator);

    while (true) {
        if (not Try_Advance_Evars(e)) {
            if (not Bool_ARG(RELAX))
                return PANIC(Error_Apply_Too_Many_Raw());

            Shutdown_Evars(e);
            Free_Memory(EVARS, e);
            Init_Unreadable(iterator);
            param = nullptr;  // we're throwing away the evaluated product
            break;
        }

        if (Get_Parameter_Flag(e->param, REFINEMENT))
            continue;

        if (Is_Parameter(Slot_Hack(e->slot))) {
            param = e->param;
            break;
        }
    }

    goto eval_step_maybe_labeled;

} handle_discarded_item: { ///////////////////////////////////////////////////

    STATE = ST_FRAME_FILLER_UNLABELED_EVAL_STEP;
    param = nullptr;  // throw away result
    goto eval_step_maybe_labeled;

} eval_step_maybe_labeled: { /////////////////////////////////////////////////

    assert(
        STATE == ST_FRAME_FILLER_LABELED_EVAL_STEP
        or STATE == ST_FRAME_FILLER_UNLABELED_EVAL_STEP
    );

    assert(not Is_Pointer_Corrupt_Debug(param));  // nullptr means toss result

    Reset_Evaluator_Erase_Out(SUBLEVEL);
    return CONTINUE_SUBLEVEL(SUBLEVEL);

} labeled_step_dual_in_spare: {  /////////////////////////////////////////////

    REBLEN index = VAL_UINT32(ARG(INDEX));

    var = Slot_Hack(Varlist_Slot(Cell_Varlist(frame), index));
    param = Phase_Param(Cell_Frame_Phase(op), index);

    goto copy_dual_spare_to_var_in_frame;

} unlabeled_step_dual_in_spare: {  ///////////////////////////////////////////

    EVARS *e = Cell_Handle_Pointer(EVARS, iterator);

    var = Slot_Hack(e->slot);
    param = e->param;

    goto copy_dual_spare_to_var_in_frame;

} copy_dual_spare_to_var_in_frame: {  ////////////////////////////////////////

    if (/* param and */ Cell_Parameter_Class(param) == PARAMCLASS_META) {
        Move_Cell(var, Liftify(SPARE));
    }
    else {
        Value* spare = Decay_If_Unstable(SPARE);
        Move_Cell(var, spare);
    }

    goto handle_next_item;

} finalize_maybe_throwing: { /////////////////////////////////////////////////

  // 1. We don't want to get any further notifications of abrupt panics that
  //    happen after we have delegated to the function.  But should DELEGATE()
  //    itself rule that out automatically?  It asserts for now.

    if (Not_Cell_Readable(iterator))
        assert(Bool_ARG(RELAX));
    else {
        EVARS *e = Cell_Handle_Pointer(EVARS, iterator);
        Shutdown_Evars(e);
        Free_Memory(EVARS, e);
        Init_Unreadable(iterator);
    }

    if (THROWING)
        return THROWN;

    Drop_Level(SUBLEVEL);

    Disable_Dispatcher_Catching_Of_Throws(LEVEL);  // no more finalize needed

    return BOUNCE_FRAME_FILLER_FINISHED;
}}


//
//  apply: native [  ; !!! MUST UPDATE SPEC FOR // NATIVE IF CHANGED [1]
//
//  "Invoke an action with all required arguments specified"
//
//      return: [any-atom?]
//      operation [<unrun> frame!]
//      args "Arguments and Refinements, e.g. [arg1 arg2 ref: refine1]"
//          [block!]
//      :relax "Don't worry about too many arguments to the APPLY"
//      <local> frame index iterator  ; update // native if this changes [1]
//  ]
//
DECLARE_NATIVE(APPLY)
//
// 1. For efficiency, the // infix version of APPLY is native, and just calls
//    right through to the apply code without going through any "Bounce"
//    or specialization code.  But that means the frame pushed for // must
//    be directly usable by APPLY.  Keep them in sync.
{
    INCLUDE_PARAMS_OF_APPLY;

    USED(ARG(OPERATION));
    USED(ARG(ARGS));
    USED(ARG(RELAX));
    // FRAME used below
    USED(LOCAL(INDEX));
    USED(LOCAL(ITERATOR));

    Bounce b = Native_Frame_Filler_Core(LEVEL);
    if (b != BOUNCE_FRAME_FILLER_FINISHED) {
        possibly(THROWING);
        return b;
    }

    return DELEGATE(OUT, Element_LOCAL(FRAME));
}


#define LEVEL_FLAG__S_S_DELEGATING  LEVEL_FLAG_MISCELLANEOUS


//
//  //: infix native [  ; !!! MUST UPDATE SPEC FOR APPLY NATIVE IF CHANGED [1]
//
//  "Infix version of APPLY with name of thing to apply literally on left"
//
//      return: [any-atom?]
//      @(operation) [<unrun> word! tuple! chain! path! frame! action!]
//      args "Arguments and Refinements, e.g. [arg1 arg2 :ref refine1]"
//          [block!]
//      :relax "Don't worry about too many arguments to the APPLY"
//      <local> frame index iterator ; need frame compatibility with APPLY [1]
//  ]
//
DECLARE_NATIVE(_S_S)  // [_s]lash [_s]lash (see TO-C-NAME)
//
// 1. See notes on APPLY for the required frame compatibility.
{
    INCLUDE_PARAMS_OF_APPLY;  // needs to be frame-compatible [1]

    enum {
        ST__S_S_INITIAL_ENTRY = STATE_0,
        ST__S_S_GETTING_OPERATION
    };

    if (Get_Level_Flag(LEVEL, _S_S_DELEGATING)) {
        assert(STATE != STATE_0);  // re-entering, should not be initial entry
        goto delegate_to_apply;
    }

  fetch_action_for_operation: {

    Element* operation = Element_ARG(OPERATION);

    STATE = ST__S_S_GETTING_OPERATION;  // will be necessary in the future...

    Sink(Value) gotten = SPARE;
    Option(Error*) error = Trap_Get_Var(
        gotten, GROUPS_OK, operation, SPECIFIED
    );
    if (error)
        return PANIC(unwrap error);

    if (not Is_Action(gotten) and not Is_Frame(gotten))
        return PANIC(gotten);

    Deactivate_If_Action(gotten);  // APPLY has <unrun> on ARG(OPERATION)

    Copy_Cell(ARG(OPERATION), gotten);

    STATE = STATE_0;  // reset state for APPLY so it looks like initial entry
    Set_Level_Flag(LEVEL, _S_S_DELEGATING);  // [2]

} delegate_to_apply: { ///////////////////////////////////////////////////////

  // Once the operator has finished doing its prep work, we tunnel through
  // to APPLY for whatever it would do, reusing the same frame.

    assert(Get_Level_Flag(LEVEL, _S_S_DELEGATING));

    // OPERATION used above
    USED(ARG(RELAX));
    USED(ARG(ARGS));
    // FRAME used below
    USED(LOCAL(INDEX));
    USED(LOCAL(ITERATOR));

    Bounce b = Native_Frame_Filler_Core(LEVEL);
    if (b != BOUNCE_FRAME_FILLER_FINISHED) {
        possibly(THROWING);
        return b;
    }

    return DELEGATE(OUT, Element_LOCAL(FRAME));
}}


// From %c-eval.c -- decide if this should be shared or otherwise.
//
#define Make_Action_Sublevel(parent) \
    Make_Level(&Action_Executor, (parent)->feed, \
        ((parent)->flags.bits & EVAL_EXECUTOR_FLAG_DIDNT_LEFT_QUOTE_PATH))



//
//  run: native [
//
//  "Invoke code inline as if it had been invoked via a WORD!"
//
//      return: [any-atom?]
//      frame [<unrun> frame!]
//      args [any-value? <variadic>]
//  ]
//
DECLARE_NATIVE(RUN)
{
    INCLUDE_PARAMS_OF_RUN;

    Value* action = ARG(FRAME);
    UNUSED(ARG(ARGS));  // uses internal mechanisms to act variadic

    Level* sub = Make_Action_Sublevel(level_);
    Push_Level_Erase_Out_If_State_0(OUT, sub);
    Push_Action(sub, action, PREFIX_0);

    return DELEGATE_SUBLEVEL(sub);
}
