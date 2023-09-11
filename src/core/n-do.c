//
//  File: %n-do.c
//  Summary: "native functions for DO, EVAL, APPLY"
//  Section: natives
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
// Ren-C's philosophy of DO is that the argument to it represents a place to
// find source code.  Hence `DO 3` does not evaluate to the number 3, any
// more than `DO "print hello"` would evaluate to `"print hello"`.  If a
// generalized evaluator is needed, use the special-purpose REEVAL(UATE).
//
// Note that although the code for running blocks and frames is implemented
// here as C, the handler for processing STRING!, FILE!, TAG!, URL!, etc. is
// dispatched out to some Rebol code.  See `system/intrinsic/do*`.
//

#include "sys-core.h"


//
//  reeval: native [
//
//  {Process an evaluated argument *inline* as the evaluator loop would}
//
//      return: [<opt> <void> <nihil> any-value!]
//      value [element?]
//          {BLOCK! passes-thru, ACTION! runs, SET-WORD! assigns...}
//      expressions [<opt> element? <variadic>]
//          {Depending on VALUE, more expressions may be consumed}
//  ]
//
DECLARE_NATIVE(reeval)
{
    INCLUDE_PARAMS_OF_REEVAL;

    // REEVAL only *acts* variadic, but uses ST_EVALUATOR_REEVALUATING
    //
    UNUSED(ARG(expressions));

    REBVAL *v = ARG(value);

    bool enfix =
        IS_QUASI(v)
        and HEART_BYTE(v) == REB_ACTION
        and Get_Action_Flag(VAL_ACTION(v), ENFIXED);

    Flags flags = FRAME_MASK_NONE;

    if (Reevaluate_In_Subframe_Throws(
        OUT,  // reeval :comment "this should leave old input"
        frame_,
        ARG(value),
        flags,
        enfix
    )){
        return THROWN;
    }

    return OUT;
}


//
//  shove: native [
//
//  {Shove a parameter into an ACTION! as its first argument}
//
//      return: [<opt> any-value!]
//          "REVIEW: How might this handle shoving enfix invisibles?"
//      :left [<end> <opt> any-value!]
//          "Requests parameter convention based on enfixee's first argument"
//      'right [<variadic> <end> any-value!]
//          "(uses magic -- SHOVE can't be written easily in usermode yet)"
//      /prefix "Force either prefix or enfix behavior (vs. acting as is)"
//          [logic!]
//      /set "If left hand side is a SET-WORD! or SET-PATH!, shove and assign"
//  ]
//
DECLARE_NATIVE(shove)
//
// PATH!s do not do infix lookup in Rebol, and there are good reasons for this
// in terms of both performance and semantics.  However, it is sometimes
// needed to dispatch via a path--for instance to call an enfix function that
// lives in a context, or even to call one that has refinements.
//
// The SHOVE operation is used to push values from the left to act as the
// first argument of an operation, e.g.:
//
//      >> 10 >- lib.(print "Hi!" first [multiply]) 20
//      Hi!
//      200
//
// It's becoming more possible to write something like this in usermode, but
// it would be inefficient.  This version of shove is a light variation on
// the EVAL native, which retriggers the actual enfix machinery.
{
    INCLUDE_PARAMS_OF_SHOVE;

    Frame(*) f;
    if (not Is_Frame_Style_Varargs_May_Fail(&f, ARG(right)))
        fail ("SHOVE (>-) not implemented for MAKE VARARGS! [...] yet");

    REBVAL *left = ARG(left);

    if (Is_Frame_At_End(f))  // shouldn't be for WORD!/PATH! unless APPLY
        return COPY(ARG(left));  // ...because evaluator wants `help <-` to work

    // It's best for SHOVE to do type checking here, as opposed to setting
    // some kind of FRAME_FLAG_SHOVING and passing that into the evaluator, then
    // expecting it to notice if you shoved into an INTEGER! or something.
    //
    // !!! To get the feature working as a first cut, this doesn't try get too
    // fancy with apply-like mechanics and slipstream refinements on the
    // stack to enfix functions with refinements.  It specializes the ACTION!.
    // We can do better, but seeing as how you couldn't call enfix actions
    // with refinements *at all* before, this is a step up.

    REBVAL *shovee = ARG(right); // reuse arg cell for the shoved-into

    if (IS_WORD(At_Frame(f)) or IS_PATH(At_Frame(f)) or IS_TUPLE(At_Frame(f))) {
        Get_Var_May_Fail(
            OUT, // can't eval directly into arg slot
            At_Frame(f),
            f_specifier,
            false
        );
        Move_Cell(shovee, OUT);
    }
    else if (IS_GROUP(At_Frame(f))) {
        if (Do_Any_Array_At_Throws(OUT, At_Frame(f), f_specifier))
            return THROWN;

        Move_Cell(shovee, OUT);  // can't eval directly into arg slot
    }
    else
        Copy_Cell(shovee, SPECIFIC(At_Frame(f)));

    if (Is_Activation(shovee)) {
        Deactivate_If_Activation(shovee);  // allow ACTION! to be run
    }
    else if (Is_Isotope(shovee))
        fail (Error_Bad_Isotope(shovee));

    if (not IS_ACTION(shovee) and not ANY_SET_KIND(VAL_TYPE(shovee)))
        fail ("SHOVE's immediate right must be ACTION! or SET-XXX! type");

    // Basic operator `>-` will use the enfix status of the shovee.
    // `->-` will force enfix evaluator behavior even if shovee is prefix.
    // `>--` will force prefix evaluator behavior even if shovee is enfix.
    //
    bool enfix;
    if (REF(prefix))
        enfix = not VAL_LOGIC(ARG(prefix));
    else if (IS_ACTION(shovee))
        enfix = Get_Action_Flag(VAL_ACTION(shovee), ENFIXED);
    else
        enfix = false;

    Fetch_Next_Forget_Lookback(f);

    // Trying to EVAL a SET-WORD! or SET-PATH! with no args would be an error.
    // So interpret it specially...GET the value and SET it back.  Note this
    // is tricky stuff to do when a SET-PATH! has groups in it to avoid a
    // double evaluation--the API is used here for simplicity.
    //
    REBVAL *composed_set_path = nullptr;

    // Since we're simulating enfix dispatch, we need to move the first arg
    // where enfix gets it from...the frame output slot.
    //
    // We quoted the argument on the left, but the ACTION! we are feeding
    // into may want it evaluative.  (Enfix handling itself does soft quoting)
    //
    if (REF(set)) {
        if (IS_SET_WORD(left)) {
            Copy_Cell(OUT, Lookup_Word_May_Fail(left, SPECIFIED));
        }
        else if (IS_SET_PATH(left) or IS_SET_TUPLE(left)) {
            f->feed->gotten = nullptr;  // calling arbitrary code, may disrupt
            composed_set_path = rebValue("compose @", left);
            if (rebRunThrows(OUT, "get @", composed_set_path))
                return THROWN;
        }
        else
            fail ("Left hand side must be SET-WORD! or SET-PATH!");
    }
    else if (
        Get_Cell_Flag(left, UNEVALUATED)
        and not (
            IS_ACTION(shovee)
            and Get_Action_Flag(VAL_ACTION(shovee), QUOTES_FIRST)
        )
    ){
        if (Eval_Value_Throws(OUT, left, SPECIFIED))
            return THROWN;
    }
    else {
        Copy_Cell(OUT, left);
        if (Get_Cell_Flag(left, UNEVALUATED))
            Set_Cell_Flag(OUT, UNEVALUATED);
    }

    // !!! Originally this used a REEVAL technique to say that the frame's
    // feed got its "next arg from out".  That feed flag had many problematic
    // issues--you could skip checking for it, and it was speaking relative
    // to "some frame".  Since then, splicing experiments were made for
    // macros--and although those features are in flux, it's a better approach
    // for use in this kind of feature.
    //
    Array(*) a = Make_Array(2);
    SET_SERIES_USED(a, 2);
    if (enfix) {
        Quotify(Move_Cell(ARR_AT(a, 0), OUT), 1);
        Move_Cell(ARR_AT(a, 1), shovee);
    }
    else {
        Move_Cell(ARR_AT(a, 0), shovee);
        Quotify(Move_Cell(ARR_AT(a, 1), OUT), 1);
    }
    Init_Block(SPARE, a);
    Splice_Block_Into_Feed(frame_->feed, SPARE);

    Frame(*) sub = Make_Frame(frame_->feed, FRAME_MASK_NONE);

    if (Trampoline_Throws(OUT, sub)) {
        rebRelease(composed_set_path);  // ok if nullptr
        return THROWN;
    }

    if (REF(set)) {
        if (IS_SET_WORD(left)) {
            Decay_If_Unstable(OUT);
            Copy_Cell(Sink_Word_May_Fail(left, SPECIFIED), OUT);
        }
        else if (IS_SET_PATH(left) or IS_SET_TUPLE(left)) {
            f->feed->gotten = nullptr;  // calling arbitrary code, may disrupt
            rebElide("set @", composed_set_path, "@", OUT);
            rebRelease(composed_set_path);
        }
        else
            assert(false); // SET-WORD!/SET-PATH! was checked above
    }

    return OUT;
}


//
//  do: native [
//
//  {Evaluates source code (see also EVAL for stepwise or invisible evaluation)}
//
//      return: [<opt> <void> any-value!]
//      source "Block of code, or indirect specification to find/make it" [
//          <maybe>  ; opts out of the DO, returns null
//          block!  ; source code in block form (see EVALUATE for other kinds)
//          text!  ; source code in text form
//          binary!  ; treated as UTF-8
//          url!  ; load code from URL via protocol
//          file!  ; load code from file on local disk
//          tag!  ; load relative to system.script.name
//          the-word!  ; module name (URL! looked up from table)
//          error!  ; should use FAIL instead
//          action!  ; will only run arity 0 actions (avoids DO variadic)
//          frame!  ; acts like APPLY (voids are optionals, not unspecialized)
//          varargs!  ; simulates as if frame! or block! is being executed
//      ]
//      /args "Sets system.script.args if doing a script (usually a TEXT!)"
//          [any-value!]
//      /only "Don't catch QUIT (default behavior for BLOCK!)"
//  ]
//
DECLARE_NATIVE(do)
// 2. FAIL is the preferred operation for triggering errors, as it has a
//    natural behavior for blocks passed to construct readable messages and
//    "FAIL X" more clearly communicates a failure than "DO X".  But DO of an
//    ERROR! would have to raise an error anyway, so it might as well raise the
//    one it is given.
//
// 3. There's an error given if you try to run a continuation of an ACTION!
//    and it takes a parameter, but you specify END as the WITH.  But giving
//    a special error here--that can point people to "did you mean REEVALUATE"
//    is something that is probably helpful enough to add.
{
    INCLUDE_PARAMS_OF_DO;

    REBVAL *source = ARG(source);

    Tweak_Non_Const_To_Explicitly_Mutable(source);

  #if !defined(NDEBUG)
    Set_Cell_Flag(source, PROTECTED);  // maybe only GC reference, keep!
  #endif

    switch (VAL_TYPE(source)) {
      case REB_BLOCK :  // no REB_GROUP, etc...EVAL does that.  see [1]
        return DELEGATE(OUT, source);

      case REB_VARARGS : {
        REBVAL *position;
        if (Is_Block_Style_Varargs(&position, source)) {
            //
            // We can execute the array, but we must "consume" elements out
            // of it (e.g. advance the index shared across all instances)
            //
            // !!! If any VARARGS! op does not honor the "locked" flag on the
            // array during execution, there will be problems if it is TAKE'n
            // or DO'd while this operation is in progress.
            //
            if (Do_Any_Array_At_Throws(OUT, position, SPECIFIED)) {
                //
                // !!! A BLOCK! varargs doesn't technically need to "go bad"
                // on a throw, since the block is still around.  But a FRAME!
                // varargs does.  This will cause an assert if reused, and
                // having BLANK! mean "thrown" may evolve into a convention.
                //
                Init_Trash(position);
                return THROWN;
            }

            Erase_Cell(position); // convention for shared data at endpoint

            if (Is_Void(OUT))
                return VOID;
            return OUT;
        }

        Frame(*) f;
        if (not Is_Frame_Style_Varargs_May_Fail(&f, source))
            panic (source); // Frame is the only other type

        // By definition, we are in the middle of a function call in the frame
        // the varargs came from.  It's still on the stack, and we don't want
        // to disrupt its state.  Use a subframe.

        if (Is_Frame_At_End(f))
            return VOID;

        Frame(*) sub = Make_Frame(
            f->feed,
            FRAME_MASK_NONE
        );
        sub->executor = &Array_Executor;
        Push_Frame(OUT, sub);
        return DELEGATE_SUBFRAME(sub); }

      case REB_THE_WORD : goto do_helper;
      case REB_BINARY : goto do_helper;
      case REB_TEXT : goto do_helper;
      case REB_URL : goto do_helper;
      case REB_FILE : goto do_helper;
      case REB_TAG : goto do_helper;

      do_helper : {
        UNUSED(REF(args)); // detected via `value? :arg`

        rebPushContinuation(
            OUT,  // <-- output cell
            FRAME_MASK_NONE,
            rebRUN(SysUtil(DO_P)),
                source,
                rebQ(ARG(args)),
                REF(only) ? rebQ(Lib(TRUE)) : rebQ(Lib(FALSE))
        );
        return BOUNCE_DELEGATE; }

      case REB_ERROR :
        fail (VAL_CONTEXT(source));  // would fail anyway, see [2]

      case REB_ACTION :
        if (First_Unspecialized_Param(nullptr, VAL_ACTION(source)))
            fail (Error_Do_Arity_Non_Zero_Raw());  // specific error?  see [3]

        return DELEGATE(OUT, source);

      case REB_FRAME :
        return DELEGATE(OUT, source);

      default :
        break;
    }

    fail (Error_Do_Arity_Non_Zero_Raw());  // https://trello.com/c/YMAb89dv
}


//
//  evaluate: native [
//
//  {Perform a single evaluator step, returning the next source position}
//
//      return: "Value from the step"
//          [<opt> <void> <nihil> any-value!]
//      source [
//          <maybe>  ; useful for `evaluate try ...` scenarios when no match
//          any-array!  ; source code in block form
//          action!
//          frame!
//          varargs!  ; simulates as if frame! or block! is being executed
//      ]
//      /next "Do one step of evaluation"
//          [word! tuple!]  ; !!! does not use multi-return, see 1
//  ]
//
DECLARE_NATIVE(evaluate)
//
// 1. Having a function like EVALUATE itself be multi-return is a pain, as
//    it is trying to return a result that can itself be a multi-return.
//    This is the nature of anything that does proxying.  It's *technically*
//    possible for a caller to pick parameter packs out of parameter packs,
//    but inconvenient.  Especially considering that stepwise evaluation is
//    going to be done on some kind of "evaluator state"--not just a block,
//    that state should be updated.
//
// 2. We want EVALUATE to treat all ANY-ARRAY! the same.  (e.g. a ^[1 + 2] just
//    does the same thing as [1 + 2] and gives 3, not '3)  Rather than mutate
//    the cell to plain BLOCK! and pass it to CONTINUE_CORE(), we initialize
//    a feed from the array directly.
//
// 6. There may have been a LET statement in the code.  If there was, we have
//    to incorporate the binding it added into the reported state *somehow*.
//    Right now we add it to the block we give back...this gives rise to
//    questionable properties, such as if the user goes backward in the block
//    and were to evaluate it again:
//
//      https://forum.rebol.info/t/1496
//
//    Right now we can politely ask "don't do that", but better would probably
//    be to make EVALUATE return something with more limited privileges... more
//    like a FRAME!/VARARGS!.
{
    INCLUDE_PARAMS_OF_EVALUATE;

    REBVAL *rest_var = ARG(next);
    REBVAL *source = ARG(source);  // may be only GC reference, don't lose it!

    enum {
        ST_EVALUATE_INITIAL_ENTRY = STATE_0,
        ST_EVALUATE_SINGLE_STEPPING
    };

    switch (STATE) {
      case ST_EVALUATE_INITIAL_ENTRY :
        goto initial_entry;

      case ST_EVALUATE_SINGLE_STEPPING :
        goto single_step_result_in_out;

      default: assert(false);
    }

  initial_entry: {  //////////////////////////////////////////////////////////

    Tweak_Non_Const_To_Explicitly_Mutable(source);

  #if !defined(NDEBUG)
    Set_Cell_Flag(ARG(source), PROTECTED);
  #endif

    if (ANY_ARRAY(source)) {
        if (VAL_LEN_AT(source) == 0) {  // `evaluate []` is invisible intent
            if (REF(next))
                rebElide(Canon(SET), rebQ(rest_var), nullptr);

            Init_Nihil(OUT);  // !!! Callers not prepared for more ornery result
            return Proxy_Multi_Returns(frame_);
        }

        Feed(*) feed = Make_At_Feed_Core(  // use feed, see [2]
            source,
            SPECIFIED
        );
        assert(Not_Feed_At_End(feed));

        Flags flags = FRAME_FLAG_ALLOCATED_FEED;

        if (not REF(next)) {
            flags |= FLAG_STATE_BYTE(ST_ARRAY_PRELOADED_ENTRY);
            Init_Nihil(OUT);  // heeded by array executor
        }

        Frame(*) subframe = Make_Frame(feed, flags);
        Push_Frame(OUT, subframe);

        if (not REF(next)) {  // plain evaluation to end, maybe invisible
            subframe->executor = &Array_Executor;
            return DELEGATE_SUBFRAME(subframe);
        }

        Set_Frame_Flag(subframe, TRAMPOLINE_KEEPALIVE);  // to ask how far it got

        STATE = ST_EVALUATE_SINGLE_STEPPING;
        return CONTINUE_SUBFRAME(subframe);
    }
    else switch (VAL_TYPE(source)) {

      case REB_FRAME :
        //
        // !!! It is likely that the return result for the NEXT: will actually
        // be a FRAME! when the input to EVALUATE is a BLOCK!, so that the
        // LET bindings can be preserved.  Binding is still a mess when it
        // comes to questions like backtracking in blocks, so review.
        //
        if (REF(next))
            fail ("/NEXT Behavior not implemented for FRAME! in EVALUATE");

        return DELEGATE(OUT, source);

      case REB_ACTION: {
        if (First_Unspecialized_Param(nullptr, VAL_ACTION(source)))
            fail (Error_Do_Arity_Non_Zero_Raw());  // see notes in DO on error

        return DELEGATE(OUT, source); }

      case REB_VARARGS : {
        assert(IS_VARARGS(source));

        REBVAL *position;
        if (Is_Block_Style_Varargs(&position, source)) {
            //
            // We can execute the array, but we must "consume" elements out
            // of it (e.g. advance the index shared across all instances)
            //
            // !!! If any VARARGS! op does not honor the "locked" flag on the
            // array during execution, there will be problems if it is TAKE'n
            // or DO'd while this operation is in progress.
            //
            REBLEN index;
            if (Eval_Step_In_Any_Array_At_Throws(
                SPARE,
                &index,
                position,
                SPECIFIED,
                FRAME_MASK_NONE
            )){
                // !!! A BLOCK! varargs doesn't technically need to "go bad"
                // on a throw, since the block is still around.  But a FRAME!
                // varargs does.  This will cause an assert if reused, and
                // having BLANK! mean "thrown" may evolve into a convention.
                //
                Init_Trash(position);
                return THROWN;
            }

            VAL_INDEX_UNBOUNDED(position) = index;
        }
        else {
            Frame(*) f;
            if (not Is_Frame_Style_Varargs_May_Fail(&f, source))
                panic (source); // Frame is the only other type

            // By definition, we're in the middle of a function call in frame
            // the varargs came from.  It's still on the stack--we don't want
            // to disrupt its state (beyond feed advancing).  Use a subframe.

            if (Is_Frame_At_End(f))
                return nullptr;

            Flags flags = FRAME_MASK_NONE;
            if (Eval_Step_In_Subframe_Throws(SPARE, f, flags))
                return THROWN;
        }
        break; }

      default:
        fail (PARAM(source));
    }

    if (REF(next))
        rebElide(Canon(SET), rebQ(rest_var), source);

    return COPY(SPARE);

} single_step_result_in_out: {  //////////////////////////////////////////////

    REBSPC *specifier = FRM_SPECIFIER(SUBFRAME);
    VAL_INDEX_UNBOUNDED(source) = FRM_INDEX(SUBFRAME);  // new index
    Drop_Frame(SUBFRAME);

    INIT_BINDING_MAY_MANAGE(source, specifier);  // integrate LETs, see [6]

    if (REF(next))
        rebElide(Canon(SET), rebQ(rest_var), source);

    return OUT;
}}


//
//  redo: native [
//
//  {Restart a frame's action from the top with its current state}
//
//      return: []  ; !!! notation for divergent function?
//      restartee "Frame to restart, or bound word (e.g. REDO 'RETURN)"
//          [frame! any-word!]
//      /other "Restart in a frame-compatible function (sibling tail-call)"
//          [action!]
//  ]
//
DECLARE_NATIVE(redo)
//
// This can be used to implement tail-call recursion:
//
// https://en.wikipedia.org/wiki/Tail_call
//
{
    INCLUDE_PARAMS_OF_REDO;

    REBVAL *restartee = ARG(restartee);
    if (not IS_FRAME(restartee)) {
        if (not Did_Get_Binding_Of(OUT, restartee))
            fail ("No context found from restartee in REDO");

        if (not IS_FRAME(OUT))
            fail ("Context of restartee in REDO is not a FRAME!");

        Move_Cell(restartee, OUT);
    }

    Context(*) c = VAL_CONTEXT(restartee);

    Frame(*) f = CTX_FRAME_IF_ON_STACK(c);
    if (f == NULL)
        fail ("Use DO to start a not-currently running FRAME! (not REDO)");

    // If we were given a sibling to restart, make sure it is frame compatible
    // (e.g. the product of ADAPT-ing, CHAIN-ing, ENCLOSE-ing, HIJACK-ing a
    // common underlying function).
    //
    // !!! It is possible for functions to be frame-compatible even if they
    // don't come from the same heritage (e.g. two functions that take an
    // INTEGER! and have 2 locals).  Such compatibility may seem random to
    // users--e.g. not understanding why a function with 3 locals is not
    // compatible with one that has 2, and the test would be more expensive
    // than the established check for a common "ancestor".
    //
    if (REF(other)) {
        REBVAL *sibling = ARG(other);
        if (
            ACT_KEYLIST(f->u.action.original)
            != ACT_KEYLIST(VAL_ACTION(sibling))
        ){
            fail ("/OTHER function passed to REDO has incompatible FRAME!");
        }

        INIT_VAL_FRAME_PHASE(restartee, VAL_ACTION(sibling));
        INIT_VAL_FRAME_BINDING(restartee, VAL_ACTION_BINDING(sibling));
    }

    // We need to cooperatively throw a restart instruction up to the level
    // of the frame.  Use REDO as the throw label that Eval_Core() will
    // identify for that behavior.
    //
    Copy_Cell(SPARE, Lib(REDO));
    INIT_VAL_ACTION_BINDING(SPARE, c);

    // The FRAME! contains its ->phase and ->binding, which should be enough
    // to restart the phase at the point of parameter checking.  Make that
    // the actual value that Eval_Core() catches.
    //
    return Init_Thrown_With_Label(FRAME, restartee, SPARE);
}


//
//  applique: native [
//
//  {Invoke an ACTION! with all required arguments specified}
//
//      return: [<opt> <void> any-value!]
//      action [<unrun> action!]
//      def "Frame definition block (will be bound and evaluated)"
//          [block!]
//  ]
//
DECLARE_NATIVE(applique)
//
// 1. Make a FRAME! for the ACTION!, weaving in the ordered refinements
//    collected on the stack (if any).  Any refinements that are used in any
//    specialization level will be pushed as well, which makes them out
//    prioritize (e.g. higher-ordered) than any used in a PATH! that were
//    pushed during the Get of the ACTION!.
{
    INCLUDE_PARAMS_OF_APPLIQUE;

    Value(*) action = ARG(action);
    Value(*) def = ARG(def);

    Value(*) frame = ARG(return);  // reuse as GC-safe cell for FRAME!

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

    Context(*) exemplar = Make_Context_For_Action_Push_Partials(  // see [1]
        action,
        STACK_BASE,  // lowest_ordered_dsp of refinements to weave in
        nullptr,  // no binder needed
        NONE_CELL  // seen as unspecialized by ST_ACTION_TYPECHECKING
    );
    Manage_Series(CTX_VARLIST(exemplar));
    Init_Frame(frame, exemplar, VAL_ACTION_LABEL(action));

    Drop_Data_Stack_To(STACK_BASE);  // refinement order unimportant

    Virtual_Bind_Deep_To_Existing_Context(
        def,
        exemplar,
        nullptr,  // !!! Unused binder parameter
        REB_SET_WORD
    );

    STATE = ST_APPLIQUE_RUNNING_DEF_BLOCK;
    return CONTINUE(SPARE, def);  // first run block bound to frame

} definition_result_in_spare: {  /////////////////////////////////////////////

    return DELEGATE(OUT, frame);  // now run the frame
}}


//
//  apply: native [
//
//  {Invoke an ACTION! with all required arguments specified}
//
//      return: [<opt> <void> any-value!]
//      action [action!]
//      args "Arguments and Refinements, e.g. [arg1 arg2 /ref refine1]"
//          [block!]
//      /relax "Don't worry about too many arguments to the APPLY"
//      <local> frame index
//  ]
//
DECLARE_NATIVE(apply)
//
// 1. Binders cannot be held across evaluations at this time.  Do slow
//    lookups for refinements, but this is something that needs rethinking.
//
// 2. Make a FRAME! for the ACTION!, weaving in the ordered refinements
//    collected on the stack (if any).  Any refinements that are used in any
//    specialization level will be pushed as well, which makes them
//    out-prioritize (e.g. higher-ordered) than any used in a PATH! that were
//    pushed during the Get of the ACTION!.
//
// 3. Two argument-name labels in a row is not legal...treat it like the next
//    refinement is reaching a comma or end of block.  (Though this could be
//    treated as an <end> case?)
//
// 4. We treat <skip> parameters as if they can only be requested by name,
//    like a refinement.  This is because the evaluative nature of APPLY is
//    not compatible with the quoting requirement of skippability.
//
// 5. Low-level frame mechanics require that no-argument refinements be either
//    # or null.  As a higher-level utility, APPLY can throw in some assistance
//    so it converts (true => #) and (false => null)
//
// 6. We need to remove the binder indices, whether we are raising an error
//    or not.  But we also want any fields not assigned to be set to `~`.
//    (We wanted to avoid the situation where someone purposefully set a
//    meta-parameter to `~` being interpreted as never setting a field).
{
    INCLUDE_PARAMS_OF_APPLY;

    Value(*) action = ARG(action);
    Value(*) args = ARG(args);

    Value(*) frame = ARG(frame);  // local variable for holding GC-safe frame
    Value(*) iterator = ARG(return);  // reuse to hold Evars iterator

    REBVAR *var;  // may come from evars iterator or found by index
    REBPAR *param;  // (same)

    enum {
        ST_APPLY_INITIAL_ENTRY = STATE_0,
        ST_APPLY_LABELED_EVAL_STEP,
        ST_APPLY_UNLABELED_EVAL_STEP
    };

    if (Get_Frame_Flag(frame_, ABRUPT_FAILURE))  // a fail() in this dispatcher
        goto finalize_apply;

    switch (STATE) {
      case ST_APPLY_INITIAL_ENTRY :
        goto initial_entry;

      case ST_APPLY_LABELED_EVAL_STEP :
        if (THROWING)
            goto finalize_apply;
        goto labeled_step_result_in_spare;

      case ST_APPLY_UNLABELED_EVAL_STEP :
        if (THROWING)
            goto finalize_apply;
        if (Is_None(iterator)) {
            assert(REF(relax));
            goto handle_next_item;
        }
        goto unlabeled_step_result_in_spare;

      default : assert(false);
    }

  initial_entry: {  //////////////////////////////////////////////////////////

    /*struct Reb_Binder binder;  // see [1]
    INIT_BINDER(&binder);*/
    Context(*) exemplar = Make_Context_For_Action_Push_Partials(  // see [2]
        action,
        STACK_BASE, // lowest_ordered_dsp of refinements to weave in
        nullptr /* &binder */,
        NONE_CELL
    );
    Manage_Series(CTX_VARLIST(exemplar)); // Putting into a frame
    Init_Frame(frame, exemplar, VAL_ACTION_LABEL(action));  // GC guarded

    Drop_Data_Stack_To(STACK_BASE);  // partials ordering unimportant

    Frame(*) f = Make_Frame_At(
        args,
        FRAME_FLAG_TRAMPOLINE_KEEPALIVE
    );
    Push_Frame(SPARE, f);

    EVARS *e = TRY_ALLOC(EVARS);
    Init_Evars(e, frame);  // CTX_ARCHETYPE(exemplar) is phased, sees locals
    Init_Handle_Cdata(iterator, e, sizeof(EVARS));

    Set_Frame_Flag(frame_, NOTIFY_ON_ABRUPT_FAILURE);  // to clean up iterator
    goto handle_next_item;

} handle_next_item: {  ///////////////////////////////////////////////////////

    Frame(*) f = SUBFRAME;

    if (Is_Frame_At_End(f))
        goto finalize_apply;

    Cell(const*) at = At_Frame(f);

    if (IS_COMMA(at)) {
        Fetch_Next_Forget_Lookback(f);
        goto handle_next_item;
    }

    // We do special handling if we see a /REFINEMENT ... that is taken
    // to mean we are naming the next argument.

  #if !defined(NDEBUG)
    TRASH_POINTER_IF_DEBUG(param);
  #endif

    if (IS_PATH(at) and IS_REFINEMENT(at)) {
        STATE = ST_APPLY_LABELED_EVAL_STEP;

        Symbol(const*) symbol = VAL_REFINEMENT_SYMBOL(At_Frame(f));

        REBLEN index = Find_Symbol_In_Context(frame, symbol, false);
        if (index == 0)
            fail (Error_Bad_Parameter_Raw(rebUnrelativize(at)));

        var = CTX_VAR(VAL_CONTEXT(frame), index);
        param = ACT_PARAM(VAL_ACTION(action), index);

        if (not Is_None(var))
            fail (Error_Bad_Parameter_Raw(rebUnrelativize(at)));

        Cell(const*) lookback = Lookback_While_Fetching_Next(f);  // for error
        at = Try_At_Frame(f);

        if (at == nullptr or IS_COMMA(at))
            fail (Error_Need_Non_End_Raw(rebUnrelativize(lookback)));

        if (IS_PATH(at) and IS_REFINEMENT(at))  // see [3]
            fail (Error_Need_Non_End_Raw(rebUnrelativize(lookback)));

        Init_Integer(ARG(index), index);
    }
    else if (Is_None(iterator)) {
        STATE = ST_APPLY_UNLABELED_EVAL_STEP;
        param = nullptr;  // throw away result
    }
    else {
        STATE = ST_APPLY_UNLABELED_EVAL_STEP;

        EVARS *e = VAL_HANDLE_POINTER(EVARS, iterator);

        while (true) {
            if (not Did_Advance_Evars(e)) {
                if (not REF(relax))
                    fail (Error_Apply_Too_Many_Raw());

                FREE(EVARS, e);
                Init_None(iterator);
                param = nullptr;  // we're throwing away the evaluated product
                break;
            }

            if (
                VAL_PARAM_CLASS(e->param) == PARAM_CLASS_RETURN
                or VAL_PARAM_CLASS(e->param) == PARAM_CLASS_OUTPUT
                or GET_PARAM_FLAG(e->param, REFINEMENT)
                or GET_PARAM_FLAG(e->param, SKIPPABLE)
            ){
                Init_None(e->var);  // TBD: RETURN will be a pure local
                continue;  // skippable only requested by name, see [4]
            }
            if (Is_None(e->var)) {
                param = e->param;
                break;
            }
        }
    }

    assert(not IS_POINTER_TRASH_DEBUG(param));  // nullptr means toss result

    if (param and VAL_PARAM_CLASS(param) == PARAM_CLASS_META)
        Set_Frame_Flag(SUBFRAME, META_RESULT);  // get decayed result otherwise
    else
        Clear_Frame_Flag(SUBFRAME, META_RESULT);

    Restart_Evaluator_Frame(SUBFRAME);
    return CATCH_CONTINUE_SUBFRAME(SUBFRAME);

} labeled_step_result_in_spare: {  ///////////////////////////////////////////

    REBLEN index = VAL_UINT32(ARG(index));

    var = CTX_VAR(VAL_CONTEXT(frame), index);
    param = ACT_PARAM(VAL_ACTION(action), index);

    goto copy_spare_to_var_in_frame;

} unlabeled_step_result_in_spare: {  /////////////////////////////////////////

    EVARS *e = VAL_HANDLE_POINTER(EVARS, iterator);

    var = e->var;
    param = e->param;

    goto copy_spare_to_var_in_frame;

} copy_spare_to_var_in_frame: {  /////////////////////////////////////////////

    if (  // help convert logic for no-arg refinement, see [5]
        IS_LOGIC(SPARE)
        and GET_PARAM_FLAG(param, REFINEMENT)
        and Is_Parameter_Unconstrained(param)
    ){
        if (VAL_LOGIC(SPARE))
            Init_Blackhole(var);
        else
            Init_Nulled(var);

        FRESHEN(SPARE);
    }
    else {
        Move_Cell(var, SPARE);
    }

    goto handle_next_item;

} finalize_apply: {  /////////////////////////////////////////////////////////

    if (Is_None(iterator))
        assert(REF(relax));
    else {
        EVARS *e = VAL_HANDLE_POINTER(EVARS, iterator);
        Shutdown_Evars(e);
        FREE(EVARS, e);
        Init_None(iterator);
    }

    if (THROWING)  // assume Drop_Frame() called on SUBFRAME?
        return THROWN;

    Drop_Frame(SUBFRAME);

    Clear_Frame_Flag(frame_, NOTIFY_ON_ABRUPT_FAILURE);  // necessary?

    return DELEGATE(OUT, frame);
}}


// From %c-eval.c -- decide if this should be shared or otherwise.
//
#define Make_Action_Subframe(parent) \
    Make_Frame((parent)->feed, \
        FRAME_FLAG_FAILURE_RESULT_OK \
        | ((parent)->flags.bits \
            & (EVAL_EXECUTOR_FLAG_FULFILLING_ARG \
                | EVAL_EXECUTOR_FLAG_DIDNT_LEFT_QUOTE_TUPLE)))



//
//  run: native [
//
//  {Invoke an ACTION! inline as if it had been invoked via a WORD!}
//
//      return: [<opt> <void> any-value!]
//      action [action!]
//      args [<opt> <void> any-value! <variadic>]
//  ]
//
DECLARE_NATIVE(run)
{
    INCLUDE_PARAMS_OF_RUN;

    Value(*) action = ARG(action);
    UNUSED(ARG(args));  // uses internal mechanisms to act variadic

    Frame(*) subframe = Make_Action_Subframe(frame_);
    Push_Frame(OUT, subframe);
    Push_Action(
        subframe,
        VAL_ACTION(action),
        VAL_ACTION_BINDING(action)
    );
    Begin_Prefix_Action(subframe, VAL_ACTION_LABEL(action));

    return DELEGATE_SUBFRAME (subframe);
}
