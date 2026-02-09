//
//  file: %c-step.c
//  summary: "Code for Evaluation of one Step in the Main Interpreter"
//  project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012-2025 Ren-C Open Source Contributors
// Copyright 2012 REBOL Technologies
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
// This file contains code for the `Stepper_Executor()`.  It's responsible
// for the typical interpretation of BLOCK! or GROUP!, in terms of giving
// sequences like `x: 1 + 2` a meaning for how SET-WORD! or INTEGER! behaves.
//
//=//// NOTES /////////////////////////////////////////////////////////////=//
//
// * Stepper_Executor() is LONG.  That's largely on purpose.  Breaking it
//   into functions would add overhead (in the RUNTIME_CHECKS build, if not
//   also NO_RUNTIME_CHECKS builds) and prevent interesting optimizations.
//   Factoring better is desired--but more in order to reduce redundant code
//   paths, as opposed to making this file shorter as a goal in and of itself.
//
//   It is separated into sections, and the invariants in each section are
//   made clear with comments and asserts.
//
// * See %d-eval.c for more detailed assertions of the preconditions,
//   postconditions, and state...which are broken out to help keep this file
//   a (slightly) more manageable length.
//
// * The evaluator only moves forward, and operates on a strict window of
//   visibility of two elements at a time (current position and "lookback").
//   See `Feed` for the code that provides this abstraction over Ren-C
//   arrays as well as C va_list.
//
// * By design the evaluator isn't recursive at the C level--it's "stackless".
//   At points where a sub-expression must be evaluated in a new level, it will
//   heap-allocate that level and then do a C `return` of BOUNCE_CONTINUE.
//   Processing then goes through the "Trampoline" (see %c-trampoline.c), which
//   later re-enters the suspended level's executor with the result.  Setting
//   the level's STATE byte prior to suspension is a common way of letting a
//   level know where to pick up from when it left off.
//
// * When it encounters something that needs to be handled as a function
//   application, it defers to %c-action.c for the Action_Executor().  The
//   action gets its own level.
//

#include "sys-core.h"


// Prefer these to XXX_Executor_Flag(EVAL) in this file (much faster!)

#define Get_Eval_Executor_Flag(L,name) \
    (((L)->flags.bits & EVAL_EXECUTOR_FLAG_##name) != 0)

#define Not_Eval_Executor_Flag(L,name) \
    (((L)->flags.bits & EVAL_EXECUTOR_FLAG_##name) == 0)

#define Set_Eval_Executor_Flag(L,name) \
    ((L)->flags.bits |= EVAL_EXECUTOR_FLAG_##name)

#define Clear_Eval_Executor_Flag(L,name) \
    ((L)->flags.bits &= ~EVAL_EXECUTOR_FLAG_##name)


// The level contains a "feed" whose ->value typically represents a "current"
// step in the feed.  But the evaluator is organized in a way that the
// notion of what is "current" can get out of sync with the feed.  An example
// would be when a SET-WORD! evaluates its right hand side, causing the feed
// to advance an arbitrary amount.
//
// So the Stepper_Executor() has its own state (in `u.eval`) to track the
// "current" position, and maintains the optional cache of what the fetched
// value of that is.  These macros help make the code less ambiguous.
//
#undef At_Level
#define L_next              cast(const Value*, L->feed->p)


#undef SCRATCH  // rename for its specific use in the evaluator
#define CURRENT  u_cast(Element*, &L->scratch)  // no executor check

#define level_ L  // for OUT, SPARE, STATE macros

#define L_binding Level_Binding(L)


// !!! In earlier development, the Level* for evaluating across a block was
// reused for each action invocation.  Since no more than one action was
// running at a time, this seemed to work.  However, because "Levels" and
// "Frames" were conflated, there was concern that this would not give enough
// reified FRAME! objects to the user.  Now that Levels and Frames are
// distinct, this should be revisited.
//
// !!! Stepper_Executor() and Evaluator_Executor() have largely merged, and
// as such you need a separate Level to track across multiple steps.  It may
// be that if a Stepper has its own Level, that could be reused for actions.
//
// 1. Frequently the ACTION! we are pushing is something that lives in OUT.
//    But OUT is also where we're asking the Level to put its result.  So we
//    ease the assertion that OUT is erased in STATE_0 in Push_Level(), but
//    be sure to Erase_Cell() before getting back to the Trampoline.
//
// 2. We keep the Level alive and do Drop_Level() manually because it gives
//    us an opportunity to examine the state bits of the action frame before
//    it gets thrown away--and doesn't cost any more to do so.
//
static Result(None) Reuse_Sublevel_Target_Out_For_Action_Core(
    Level* L,
    const Value* action,
    Option(InfixMode) infix_mode
){
    possibly(action == L->out);

    Level* sub = SUBLEVEL;

    assert(sub->executor == &Just_Use_Out_Executor);
    assert(sub->feed == L->feed);

    Phase *phase = Frame_Phase(action);
    if (Not_Stub_Flag(phase, PHASE_PURE))
        Set_Eval_Executor_Flag(L, OUT_IS_DISCARDABLE);

    sub->executor = &Action_Executor;
    sub->flags.bits = (
        LEVEL_FLAG_0_IS_TRUE | LEVEL_FLAG_4_IS_TRUE
            | LEVEL_FLAG_TRAMPOLINE_KEEPALIVE
            | (L->flags.bits & LEVEL_FLAG_PURE)
            | LEVEL_FLAG_DEBUG_STATE_0_OUT_NOT_ERASED_OK  // [1]
            | LEVEL_FLAG_TRAMPOLINE_KEEPALIVE  // [2]
            | (Get_Cell_Flag(action, WEIRD_VANISHABLE) ? 0
                : (L->flags.bits & LEVEL_FLAG_VANISHABLE_VOIDS_ONLY))
    );
    sub->out = L->out;

    trap (
      Push_Action(sub, action, infix_mode)
    );

    return none;
}

#define Reuse_Sublevel_Target_Out_For_Action(action,infix_mode) \
    Reuse_Sublevel_Target_Out_For_Action_Core(L, (action), (infix_mode))


static Result(None) Reuse_Sublevel_Target_Out_For_Eval_Core(
    Level* L,
    const Element* list
){
    Level* sub = SUBLEVEL;

    assert(sub->executor == &Just_Use_Out_Executor);
    assert(sub->feed == L->feed);  // we change it, and change back later

    sub->executor = &Evaluator_Executor;

    sub->flags.bits = (
        LEVEL_FLAG_0_IS_TRUE | LEVEL_FLAG_4_IS_TRUE
            | LEVEL_FLAG_TRAMPOLINE_KEEPALIVE
            | (L->flags.bits & LEVEL_FLAG_PURE)
            | (not LEVEL_FLAG_VANISHABLE_VOIDS_ONLY)  // group semantics [1]
    );

    sub->out = L->out;

  trap (
    Feed* feed = Prep_At_Feed(
        Alloc_Feed(),
        list,
        L_binding,
        L->feed->flags.bits  // inherit L's const feed bit
    )
  );
    Release_Feed(sub->feed);
    sub->feed = feed;
    Add_Feed_Reference(sub->feed);

    Erase_Cell(Level_Spare(sub));
    Erase_Cell(Level_Scratch(sub));

    return none;
}

#define Reuse_Sublevel_Target_Out_For_Eval(list) \
    Reuse_Sublevel_Target_Out_For_Eval_Core(L, (list))


// 1. L's SPARE is where we are storing the lookahead for the next value.  It
//    may be--if it's not a function--that this is the value we want, but
//    that depends on lookahead.  We *should* be feeding that ahead as the
//    intrinsic's argument.  This implies wanting an entry mode where we
//    already have the lookahead in SPARE.
//
static Result(None) Reuse_Sublevel_Target_Spare_For_Intrinsic_Arg_Core(
    Level* L
){
    Level* sub = SUBLEVEL;

    assert(sub->executor == &Just_Use_Out_Executor);
    assert(sub->feed == L->feed);

    sub->executor = &Stepper_Executor;

    sub->flags.bits = (
        LEVEL_FLAG_0_IS_TRUE | LEVEL_FLAG_4_IS_TRUE
            | LEVEL_FLAG_TRAMPOLINE_KEEPALIVE
            | (L->flags.bits & LEVEL_FLAG_PURE)
            | EVAL_EXECUTOR_FLAG_FULFILLING_ARG
    );
    inapplicable(LEVEL_FLAG_VANISHABLE_VOIDS_ONLY);  // single step, not multi

    sub->out = Level_Spare(L);  // targets spare, rethink [1]

    Erase_Cell(Level_Spare(sub));
    Erase_Cell(Level_Scratch(sub));

    return none;
}

#define Reuse_Sublevel_Target_Spare_For_Intrinsic_Arg() \
    Reuse_Sublevel_Target_Spare_For_Intrinsic_Arg_Core(L)


//
// SET-XXX! operations want to do roughly the same thing as the first step
// of their evaluation.  They evaluate the right hand side into L->out.
//
// 1. Note that any infix left-literal operators that would look backwards to
//    see the `x:` would have intercepted it during a lookback...pre-empting
//    any of this code.
//
// 2. Using a SET-XXX! means you always have at least two elements; it's like
//    an arity-1 function.  `1 + x: whatever ...`.  This overrides the no
//    lookahead behavior flag right up front.
//
INLINE Result(None) Reuse_Sublevel_Target_Out_For_Step_Core(Level* L)
{
    Level *sub = SUBLEVEL;

    assert(sub->executor == &Just_Use_Out_Executor);
    assert(sub->feed == L->feed);

    if (Is_Feed_At_End(L->feed))  // `eval [x:]`, `eval [o.x:]`, etc. illegal
        return fail (Error_Need_Non_End(CURRENT));

    Clear_Feed_Flag(L->feed, NO_LOOKAHEAD);  // always >= 2 elements [2]

    sub->executor = &Stepper_Executor;

    sub->flags.bits = (
        LEVEL_FLAG_0_IS_TRUE | LEVEL_FLAG_4_IS_TRUE
            | LEVEL_FLAG_TRAMPOLINE_KEEPALIVE
            | (L->flags.bits & LEVEL_FLAG_PURE)  // v-- if L fulfilling, we are
            | (L->flags.bits & EVAL_EXECUTOR_FLAG_FULFILLING_ARG)
    );
    inapplicable(LEVEL_FLAG_VANISHABLE_VOIDS_ONLY);  // single step, not multi

    assert(Is_Cell_Erased(L->out));
    sub->out = L->out;

    Erase_Cell(Level_Spare(sub));
    Erase_Cell(Level_Scratch(sub));

    return none;
}

#define Reuse_Sublevel_Target_Out_For_Step() \
    Reuse_Sublevel_Target_Out_For_Step_Core(L)


//
//  Inert_Stepper_Executor: C
//
// This simplifies implementation of operators that can run in an "inert" mode:
//
//     >> any [1 + 2]
//     == 3
//
//     >> any @[1 + 2]
//     == 1
//
// Inert operations wind up costing a bit more because they push a Level when
// it seems "they don't need to".  But it means the code can be written in a
// regularized form that applies whether evaluations are done or not, and it
// handles all the things like locking the array from modification during the
// iteration, etc.
//
Bounce Inert_Stepper_Executor(Level* L)
{
    enum {
        ST_INERT_STEPPER_INITIAL_ENTRY = STATE_0,
        ST_INERT_STEPPER_FINISHED
    };

    assert(STATE == ST_INERT_STEPPER_INITIAL_ENTRY);
    STATE = ST_INERT_STEPPER_FINISHED;  // can't leave as STATE_0

    assert(Not_Level_At_End(L));

    Copy_Cell_May_Bind(OUT, At_Feed(L->feed), L_binding);
    Fetch_Next_In_Feed(L->feed);
    return OUT;
}


// This test is used to check for an infix operator in the next feed position.
//
// 1. The system for sync'ing feeds makes sure that we never see null pointers
//    in the feed->p.  API nulls are turned into nulled cells, and rebEND
//    signals (which are smaller than a Cell and not Cell-aligned) are
//    canonized into Cell-aligned memory with BASE_BYTE_END.  Hence this test
//    is safe, and will reject both nulls and ends.
//
// 2. It has been considered whether CHAIN! or PATH! should be allowed to
//    dispatch infix.  It's certainly more work and would cost more.  Whether
//    it's worth it is an open question.
//
// 3. Infix operators don't work across newlines, so even things like this
//    are no longer legal:
//
//       if condition [
//          ...
//       ]
//       else [
//          ...
//       ]
//
#define Next_Not_Word_Or_Is_Newline_Or_End(L) ( \
    (u_cast(Cell*, L->feed->p)->header.bits & ( /* maybe PG_Feed_At_End [1] */ \
        FLAG_KIND_BYTE(255) \
            | FLAG_LIFT_BYTE(255) \
            | CELL_FLAG_NEWLINE_BEFORE \
    )) != ( \
        FLAG_KIND_BYTE(TYPE_WORD) /* CHAIN! or PATH!...worth it? [2] */ \
            | FLAG_LIFT_BYTE(NOQUOTE_3)   \
            | (not CELL_FLAG_NEWLINE_BEFORE)  /* no infix newlines [3] */ \
    ))


//
//  Stepper_Executor: C
//
// Expression execution can be thought of as having four distinct states:
//
//    * new_expression
//    * evaluate
//    * lookahead
//    * finished -or- threw
//
// It is possible to preload states and start an evaluator at any of these.
//
Bounce Stepper_Executor(Level* L)
{
    if (THROWING)
        return THROWN;  // no state to clean up

    assert(TOP_INDEX >= STACK_BASE);  // e.g. REDUCE accrues
    assert(OUT != SPARE);  // overwritten by temporary calculations

  switch_on_state_byte: {

  // Given how the evaluator is written, it's inevitable that there has to be
  // a test for points to `goto` before running normal eval.  This cost is
  // paid on every entry to Stepper_Executor().
  //
  // The STATE byte states are a superset of TYPE_XXX bytes, because when the
  // type is calculated it is stored in STATE.

    switch (STATE) {
      case ST_STEPPER_INITIAL_ENTRY: goto initial_entry;
      case ST_STEPPER_LOOKING_AHEAD: goto lookahead;
      case ST_STEPPER_REEVALUATING: goto reevaluate;
      case ST_STEPPER_FULFILLING_INTRINSIC_ARG: goto intrinsic_arg_in_spare;
      case ST_STEPPER_SET_GROUP: goto set_group_result_in_out;
      case ST_STEPPER_GENERIC_SET: goto generic_set_rightside_in_out;
      case ST_STEPPER_SET_BLOCK: goto set_block_rightside_in_out;

      case TYPE_GROUP: goto group_or_meta_group_result_in_out;
      case TYPE_FRAME: goto action_result_in_out;

      case ST_STEPPER_BIND_OPERATOR: goto bind_rightside_in_out;
      case ST_STEPPER_IDENTITY_OPERATOR: goto identity_rightside_in_out;

    #if RUNTIME_CHECKS
      case ST_STEPPER_FINISHED_DEBUG:
        assert(!"Stepper STATE not re-initialized after completion");
        break;
    #endif

      default: assert(false);
    }

} initial_entry: {

  // We'll almost always need to create a new Level when doing an evaluation
  // step--even for a simple case like an INTEGER!--because we need a place
  // to do infix lookaheads without disturbing the output cell.  The only
  // case where this would not be true would be a single step evaluation with
  // a NO_LOOKAHEAD flag.
  //
  // To make the invariants easier, we always push a Level that can be used
  // by whatever processing that we do--.

    require (  // for the sake of simpler invariants, always push a Level
      Level* sub = Make_Level(
        &Just_Use_Out_Executor,
        L->feed,  // feed *usually* L->feed, but GROUP!/etc. may change it
        LEVEL_FLAG_TRAMPOLINE_KEEPALIVE  // flags overwritten
      )
    );
    Push_Level(SPARE, sub);  // by default target spare (this can change)
    definitely(sub == SUBLEVEL);
    goto start_new_expression;

} start_new_expression: {  ///////////////////////////////////////////////////

  // This point is jumped to on initial entry, but also in optimized modes
  // when a BLANK! ("comma" or "blank line") is encountered.  There's also a
  // hook that notices a C-DEBUG-BREAK invocation, and rather than going
  // through running an ACTION! it just jumps back up to here to give a direct
  // opportunity to debug the next step (without unwinding an ACTION! Level).

    Sync_Feed_At_Cell_Or_End_May_Panic(L->feed);

  #if RUNTIME_CHECKS
    if (Is_Level_At_End(L)) {
        Where_Core_Debug(L);
        debug_break();
    }
  #endif

    assert(Not_Level_At_End(L));

  #if RUNTIME_CHECKS
    Stepper_Start_New_Expression_Checks(L);
  #endif

    Update_Expression_Start(L);  // !!! See Level_Array_Index() for caveats

    if (Is_Antiform(L_next))
        panic ("Antiform passed in through API, must use @ or ^ operators");

    possibly(Not_Cell_Erased(CURRENT));  // erasure is unnecessary invariant
    Copy_Cell(CURRENT, As_Element(L_next));
    Fetch_Next_In_Feed(L->feed);

    goto lookahead_for_left_literal_infix;

} reevaluate: { //////////////////////////////////////////////////////////////

  // REEVALUATE jumps here.  Note that jumping to this label doesn't advance
  // the expression index, so as far as error messages and such are concerned,
  // `reeval x` will still start with `reeval`.

    require (  // for the sake of simpler invariants, always push a Level
      Level* sub = Make_Level(
        &Just_Use_Out_Executor,
        L->feed,  // feed *usually* L->feed, but GROUP!/etc. may change it
        LEVEL_FLAG_TRAMPOLINE_KEEPALIVE  // flags overwritten
      )
    );
    Push_Level(SPARE, sub);  // by default target spare (this can change)
    definitely(sub == SUBLEVEL);

    Erase_Cell(OUT);

    goto lookahead_for_left_literal_infix;

} lookahead_for_left_literal_infix: { ////////////////////////////////////////

  // The first thing we do in an evaluation step has to be to look ahead for
  // any function that takes its left hand side literally.  Arrow functions
  // are a good example:
  //
  //      >> x: does [print "Running X the function"]
  //
  //      >> x -> [print ["x is" x]]
  //      == ~\...\~  ; antiform (action!)
  //
  // The X did not get a chance to run before the -> looked back.
  //
  // However, there needs to be special handling for assignments, which
  // effectively start a new expression:
  //
  //      t: type of x
  //
  // We don't want our "first offer" in such a case to be to offer the `t:`
  // to TYPE.  This wastes a lookup of the binding of TYPE (which the OF
  // function doesn't care about).  But also, that lookup means that such
  // expressions couldn't be used in pure functions--because the unnecessary
  // observation of TYPE could be some stray variable that's not in the pure
  // function's legal observable scope.
  //
  // However, we DO want left-literal constructs like DEFAULT:
  //
  //     >> x: null
  //
  //     >> x: default [1000 + 20]
  //     == 1020
  //
  //     >> x: default [300 + 4]
  //     == 1020
  //
  // Hence if we see the specific pattern SET-WORD WORD1 WORD2 we are in a
  // special situation.  We may want to get past this section normally with
  // SET-WORD as CURRENT, WORD1 as L_next, and WORD2 waiting in the wings.
  // Or we may want SET-WORD in out, with the WORD1 getting it as its left
  // hand argument for infix.  Or we may want to be in a level continuation
  // with WORD1 in out, and WORD2 waiting.
  //
  // !!! This mechanic is TBD.

    assert(Is_Cell_Erased(SPARE));
    assert(Is_Cell_Erased(OUT));

    if (Next_Not_Word_Or_Is_Newline_Or_End(L)) {
        possibly(Is_Antiform(L_next));  // API calls, rebValue("^", antiform)
        goto give_up_backward_quote_priority;
    }

    Option(InfixMode) infix_mode;
    Phase* infixed;

    Element* sub_current = Copy_Cell(
        Level_Scratch(SUBLEVEL),
        As_Element(L_next)
    );
    Bind_Cell_If_Unbound(sub_current, L_binding);
    Add_Cell_Sigil(sub_current, SIGIL_META);  // for unstable lookup
    heeded (sub_current);
    heeded (Corrupt_Cell_If_Needful(Level_Spare(SUBLEVEL)));

    SUBLEVEL->out = SPARE;  // fetch next into current level's SPARE
    LEVEL_STATE_BYTE(SUBLEVEL) = 1;

    Get_Var_In_Scratch_To_Out(  // e.g. *sub's* OUT (L's SPARE)
        SUBLEVEL, NO_STEPS
    ) except (Error* e) {
        // don't care (will hit on next step if we care)
        UNUSED(e);
        goto give_up_backward_quote_priority;
    }

    if (
        not Is_Action(SPARE)
        or not (
            infix_mode = Frame_Infix_Mode(SPARE)
        )
    ){
        goto give_up_backward_quote_priority;
    }

    infixed = Frame_Phase(SPARE);

  check_first_infix_parameter_class: {

    // 1. Lookback args are fetched from OUT, then copied into an arg slot.
    //    Put the backwards quoted value into OUT.  (Do this before next
    //    step because we need value for type check)
    //
    // 2. We make a special exemption for left-stealing arguments, when they
    //    have nothing to their right.  They lose their priority and we run
    //    the left hand side with them as a priority instead.  This lets us
    //    do (the ->) or (help of)

    Option(ParamClass) pclass = Get_First_Param_Literal_Class(infixed);
    if (not pclass)
        goto give_up_backward_quote_priority;

    assert(
        pclass == PARAMCLASS_LITERAL  // infix func [@x ...] [...]
        or pclass == PARAMCLASS_SOFT
    );
    Copy_Cell_May_Bind(OUT, CURRENT, L_binding);  // left side in OUT [1]

    Copy_Cell(
        CURRENT,  // CURRENT now invoking word (->-, OF, =>)
        As_Element(L_next)
      );
    Fetch_Next_In_Feed(L->feed);  // ...now skip that invoking word

    if (
        Is_Feed_At_End(L->feed)  // v-- OUT is what used to be on left
        and (
            Type_Of_Unchecked(OUT) == TYPE_WORD
            or Type_Of_Unchecked(OUT) == TYPE_PATH
        )
    ){  // exemption: put OUT back in CURRENT and CURRENT back in feed [2]
        Move_Cell(&L->feed->fetched, CURRENT);
        L->feed->p = &L->feed->fetched;

        Move_Cell(CURRENT, cast(Element*, OUT));

        Set_Eval_Executor_Flag(L, DIDNT_LEFT_QUOTE_PATH);

        if (Is_Word(CURRENT)) {
            STATE = cast(StepperState, TYPE_WORD);
            goto handle_word_where_action_lookups_are_active;
        }

        assert(Is_Path(CURRENT));
        STATE = cast(StepperState, TYPE_PATH);
        goto handle_path_where_action_lookups_are_active;
    }

} right_hand_literal_infix_wins: {

    require (
      Reuse_Sublevel_Target_Out_For_Action(SPARE, infix_mode)
    );
    if (infix_mode == PREFIX_0)  // sets STATE_0 for level
        Erase_Cell(OUT);
    goto process_action;


}} give_up_backward_quote_priority: { ///////////////////////////////////////

    assert(Is_Cell_Erased(OUT));

    if (LIFT_BYTE(CURRENT) == NOQUOTE_3) {
        Option(Sigil) sigil = Sigil_Of(CURRENT);
        switch (opt sigil) {
          case SIGIL_0:
            goto handle_plain;

          case SIGIL_META:  // ^ allows unstable antiforms to fetch
            goto handle_any_metaform;

          case SIGIL_PIN:  // @ gives back the the value "as is"
            goto handle_any_pinned;

          case SIGIL_TIE:  // $ ties the value
            goto handle_any_tied;
        }
    }

    if (LIFT_BYTE(CURRENT) > QUASIFORM_4)
        goto handle_quoted;

    assert(LIFT_BYTE(CURRENT) == QUASIFORM_4);
    goto handle_quasiform;

} handle_quoted: { //// QUOTED! [ 'XXX  '''@XXX  '~XXX~ ] ////////////////////

    // Quoted values drop one quote level.  Binding is left as-is.

    Copy_Cell(OUT, CURRENT);

    LIFT_BYTE(OUT) -= Quote_Shift(1);
    STATE = cast(StepperState, TYPE_QUOTED);  // can't leave STATE_0

    goto lookahead;


} handle_quasiform: { //// QUASIFORM! ~XXX~ //////////////////////////////////

  // Quasiforms produce antiforms when they evaluate.  Binding is erased.
  //
  // 1. Not all quasiforms have legal antiforms.  For instance: while all
  //    WORD!s have quasiforms, only ~null~ and ~okay~ are allowed to become
  //    antiforms (LOGIC!)
  //
  // 2. If we are in a step of a sequential series of evaluations, then
  //    it is risky to allow VOID! to vanish, e.g.:
  //
  //        eval compose [some stuff (lift ^var)]  ; var incidentally VOID!
  //
  //    It's a fine line. If you had composed in code like `comment "hi"`
  //    that would be one thing, but synthesizing a lifted value from an
  //    arbitrary expression feels less specific.  Use ^ operator if you
  //    really want vaporization: `[some stuff ^ (lift ^var)]`

    Copy_Cell(OUT, CURRENT);

    require (  // may be illegal [1]
      Coerce_To_Antiform(OUT)
    );

    if (Get_Level_Flag(L, VANISHABLE_VOIDS_ONLY) and Is_Void(OUT))
        Note_Level_Out_As_Void_To_Make_Heavy(L);  // avoid accidents [2]

    STATE = cast(StepperState, TYPE_QUASIFORM);  // can't leave STATE_0
    goto lookahead;


} handle_any_pinned: { //// PINNED! (@XXX) ///////////////////////////////////

  // PINNED! types originally focused on inert "do nothing", but are seeming
  // more valuable to use for iterators.

  switch (STATE = cast(Byte, Heart_Of(CURRENT))) {

  case TYPE_BLANK: { //// LITERALLY OPERATOR (@) /////////////////////////////

  // @ acts like LITERALLY, and doesn't add binding:
  //
  //     >> abc: 10
  //
  //     >> word: @ abc
  //     == abc
  //
  //     >> get word
  //     ** PANIC: word is not bound
  //
  // The reason @ doesn't bind (when the other @xxx types do) is that the key
  // need in the API is for an "as is" operator, and it works for that.  Note
  // another difference is that the other types result in a value with the
  // @ Sigil on them, but this one gives you type as it was.
  //
  // 2. There's a twist, that @ can actually handle stable antiforms if they
  //    come in via an API feed.  This is a convenience so you can write:
  //
  //        rebElide("append block opt @", value_might_be_null);
  //
  //     ...instead of:
  //
  //        rebElide("append block opt", rebQ(value_might_be_null));
  //
  //    If you consider the API to be equivalent to TRANSCODE-ing the given
  //    material into a BLOCK! and then EVAL-ing it, then this is creating an
  //    impossible situation of having an antiform in the block.  But the
  //    narrow exception in the evaluator is considered worth it:
  //
  //      https://forum.rebol.info/t/why-isnt-a-precise-synonym-for-the/2215
  //

    if (Is_Feed_At_End(L->feed))  // no literal to take as argument
        panic (Error_Need_Non_End(CURRENT));

    assert(Not_Feed_Flag(L->feed, NEEDS_SYNC));

    if (Is_Antiform(L_next)) {
        Copy_Cell(OUT, L_next);
        trap (
          Decay_If_Unstable(L->out)  // use ^ instead if you want unstable
        );
        Fetch_Next_In_Feed(L->feed);
    }
    else {  // need to honor CELL_FLAG_CONST, etc.
        Just_Next_In_Feed(L->out, L->feed);  // !!! review infix interop
    }
    goto lookahead;

} default: { //// MISCELLANEOUS PINNED! TYPE /////////////////////////////////

  // Just leave the sigil:
  //
  //    >> @word
  //    == @word
  //
  // !!! This behavior is scheduled to change, as @ is more useful as an
  // active type with iterators.

    Inertly_Derelativize_Inheriting_Const(OUT, CURRENT, L->feed);
    goto lookahead;

  }}  // end switch on pinned type

} handle_any_tied: { //// TIED! ($XXX) ///////////////////////////////////////

    // The $XXX types evaluate to remove the decoration, but be bound:
    //
    //     >> var: 1020
    //
    //     >> $var
    //     == var
    //
    //     >> get $var
    //     == 1020
    //
    // This is distinct from quoting the item, which would give you the item
    // undecorated but not changing the binding (usually resulting in unbound).
    //
    //     >> var: 1020
    //
    //     >> get 'var
    //     ** Error: var is unbound

  switch (STATE = cast(Byte, Heart_Of(CURRENT))) {

  case TYPE_BLANK: { //// BIND OPERATOR ($) //////////////////////////////////

  // The $ sigil will evaluate the right hand side, and then bind the
  // product into the current evaluator environment.

    require (
      Reuse_Sublevel_Target_Out_For_Step()
    );

    STATE = ST_STEPPER_BIND_OPERATOR;
    return CONTINUE_SUBLEVEL;

} bind_rightside_in_out: { ///////////////////////////////////////////////////

    SUBLEVEL->executor = &Just_Use_Out_Executor;  // temporary (?) invariant
    assert(SUBLEVEL->feed == L->feed);  // we didn't change it

    if (Is_Antiform(OUT))
        panic ("$ operator cannot bind antiforms");

    Bind_Cell_If_Unbound(As_Element(OUT), Level_Binding(L));
    goto lookahead;


} default: {  //// MISCELLANEOUS TIED! TYPE //////////////////////////////////

    Inertly_Derelativize_Inheriting_Const(OUT, CURRENT, L->feed);
    Clear_Cell_Sigil(u_cast(Element*, OUT));  // remove the $
    goto lookahead;

  }}  // end switch on tied type


} handle_any_metaform: { //// META (^) ///////////////////////////////////////

  // METAFORM! types retrieve things with less intervention (decaying, or
  // perhaps suppressing VOID! => HEAVY VOID conversions).

  switch (STATE = cast(Byte, Heart_Of(CURRENT))) {

  case TYPE_BLANK: { //// IDENTITY OPERATOR (^) ///////////////////////////////

    if (Is_Feed_At_End(L->feed))  // no literal to take as argument
        panic (Error_Need_Non_End(CURRENT));

    if (Is_Antiform(L_next)) {  // special featur: tolerate antiforms as-is
        Copy_Cell(OUT, L_next);
        Fetch_Next_In_Feed(L->feed);
        goto lookahead;
    }

    require (
      Reuse_Sublevel_Target_Out_For_Step()
    );

    STATE = ST_STEPPER_IDENTITY_OPERATOR;
    return CONTINUE_SUBLEVEL;

} identity_rightside_in_out: {

    SUBLEVEL->executor = &Just_Use_Out_Executor;  // temporary (?) invariant
    assert(SUBLEVEL->feed == L->feed);  // we didn't change it

    // !!! Did all the work just by making a not-afraid of voids step?

    goto lookahead;

} case TYPE_WORD: { //// META WORD! ^XXX /////////////////////////////////////

  // A META-WORD! gives you the undecayed representation of the variable
  //
  // 1. We don't want situations like `^x: (<expr> ^y)` to assign <expr> to x
  //    just because y incidentally held a VOID!.  You need to be explicit
  //    with `^x: (<expr> ^ ^y)` to get that behavior, which would bypass the
  //    LEVEL_FLAG_VANISHABLE_VOIDS_ONLY which sequential evaluations in
  //    Evaluator_Executor() use by default.
  //
  // We do this fetch in the sublevel, because we want to potentially keep
  // the SPARE cache value around (which is the fetch of the *next* value...
  // if it was a word).

    Element* sub_current = Copy_Cell(Level_Scratch(SUBLEVEL), CURRENT);
    Bind_Cell_If_Unbound(sub_current, L_binding);
    heeded (Corrupt_Cell_If_Needful(Level_Spare(SUBLEVEL)));
    heeded (sub_current);

    SUBLEVEL->out = OUT;
    LEVEL_STATE_BYTE(SUBLEVEL) = 1;

    require (
      Get_Var_In_Scratch_To_Out(SUBLEVEL, NO_STEPS)
    );

    possibly(Not_Cell_Stable(OUT));

    if (Get_Level_Flag(L, VANISHABLE_VOIDS_ONLY) and Is_Void(OUT))
        Note_Level_Out_As_Void_To_Make_Heavy(L);  // avoid accidents [1]

    goto lookahead;


} case TYPE_TUPLE: { //// META TUPLE! ^XXX.YYY ///////////////////////////////

    // Note that the GET native on a TUPLE! won't allow GROUP! execution:
    //
    //    foo: [X]
    //    path: 'foo.(print "side effect!" 1)
    //    get path  ; not allowed, due to surprising side effects
    //
    // However a source-level GET-TUPLE! allows them, since they are at the
    // callsite and you are assumed to know what you are doing:
    //
    //    :foo.(print "side effect" 1)  ; this is allowed
    //
    // Consistent with GET-WORD!, a GET-TUPLE! won't allow nothing access on
    // the plain (unfriendly) forms.
    //
    // 1. It's possible for (^obj.lifted-error) to give back a FAILURE! due
    //    to the field being a lifted error, or (^obj.missing-field) to give
    //    a FAILURE! due to the field being absent.

    assert(Is_Metaform(CURRENT));

    Element* sub_current = Copy_Cell(Level_Scratch(SUBLEVEL), CURRENT);
    heeded (Bind_Cell_If_Unbound(sub_current, L_binding));
    heeded (sub_current);
    heeded (Corrupt_Cell_If_Needful(Level_Spare(SUBLEVEL)));

    SUBLEVEL->out = OUT;
    LEVEL_STATE_BYTE(SUBLEVEL) = 1;

    require (
      Get_Var_In_Scratch_To_Out(SUBLEVEL, GROUPS_OK)
    );
    possibly(Is_Failure(OUT));  // last step maybe missing, or meta-failure [1]

    goto lookahead;  // even FAILURE! wants lookahead (e.g. for EXCEPT)


} case TYPE_CHAIN: { //// META CHAIN! (^XXX: ^:XXX ...) //////////////////////

    goto handle_chain_or_meta_chain;


} case TYPE_GROUP: { //// META GROUP! ^(...) /////////////////////////////////

    goto handle_group_or_meta_group;


} case TYPE_BLOCK: { //// META BLOCK! ^[...] /////////////////////////////////

    // Produces a PACK! of what it is given:
    //
    //    >> ^[1 + 2 null]
    //    == ~('3 ~null~)~  ; anti
    //
    // This is the most useful meaning, and it round trips the values:
    //
    //    >> ^[a b]: ^[1 + 2 null]
    //    == ~('3 ~null~)~
    //
    //    >> a
    //    == 3
    //
    //    >> b
    //    == ~null~  ; anti

    Element* out = Inertly_Derelativize_Inheriting_Const(OUT, CURRENT, L->feed);
    KIND_BYTE(out) = TYPE_BLOCK;
    Quote_Cell(out);  // !!! was quoting, to avoid binding?

    Element* spare = Init_Word(SPARE, CANON(PACK));
    dont(Quote_Cell(As_Element(SPARE)));  // want to run word

    Api(Value*) temp = rebUndecayed_helper(
        cast(RebolContext*, Level_Binding(L)),
        spare, out, rebEND
    );
    Copy_Cell(OUT, temp);
    rebRelease(temp);

    goto lookahead;


} case TYPE_FENCE: { //// META FENCE! ^{...} /////////////////////////////////

    panic ("Don't know what ^FENCE! is going to do yet");


} default: { /////////////////////////////////////////////////////////////////

    panic (
        "Only ^WORD!, ^GROUP, ^BLOCK! eval at this time for METAFORM!"
    );

  }} // end switch()


} handle_plain: { //// *** THIS IS THE "MAIN" SWITCH STATEMENT *** ///////////

    // This switch is done with a case for all TYPE_XXX values, in order to
    // facilitate use of a "jump table optimization":
    //
    // http://stackoverflow.com/questions/17061967/c-switch-and-jump-tables
    //
    // Subverting the jump table optimization with specialized branches for
    // fast tests historically were shown to reduce performance.  Review if
    // better tricks have come along.
    //
    // 1. The Stepper_Executor()'s state bytes are a superset of the
    //    Heart_Of() of processed values.  See the ST_STEPPER_XXX enumeration.

  switch ((STATE = cast(Byte, Heart_Of(CURRENT)))) {  // superset [1]

  case TYPE_BLANK: { //// BLANK! (often manifests as "," character) //////////

  // A blank is a lightweight looking expression barrier.  It errors on
  // evaluations that aren't interstitial, or gets skipped over otherwise.
  //
  //   https://forum.rebol.info/t/1387/6
  //
  // 1. In argument fulfillment, we want to treat BLANK! the same as reaching
  //    the end of a block.  That means leaving a "hole" in the argument slot
  //    which is BEDROCK_0... it *acts* like a NULL, but is "beneath" null
  //    as it is not a value that can be a product of evaluation.  Hence by
  //    definition we cannot write it into the OUT parameter--the action
  //    executor has to preemptively notice commas in the feed and not call
  //    an evaluation step for them.
  //
  // 2. We depend on EVAL:STEP only returning NULL when it's given the tail
  //    of a block, to indicate no synthesized value (not even VOID!).  If
  //    we "just skipped" commas in this case, we'd have to create some
  //    other notion of how to detect Is_Level_At_End() that counted commas
  //    *before* asking for an evaluation.
  //
  //    So there's no getting around the fact that [,] produces VOID!.
  //
  // 3. The cleanest model of BLANK! behavior would be for it to always step
  //    over it and produce a VOID! on each step over a blank.  But this
  //    risks being costly enough that people might avoid using blanks even
  //    if they liked the visual separation.  For now, exercise both options:
  //    a high-performance "just skip it", and a slower "debug" mode.

    assert(Not_Eval_Executor_Flag(L, FULFILLING_ARG));  // handles before [1]

    if (Is_Level_At_End(L)) {  // EVAL [,] must make VOID! [2]
        Init_Void(OUT);
        dont(Note_Level_Out_As_Void_To_Make_Heavy(L));
        goto finished;
    }

    if (In_Debug_Mode(64)) {  // simulate VOID! generation, sometimes [3]
        Init_Void(OUT);
        dont(Note_Level_Out_As_Void_To_Make_Heavy(L));
        goto finished;
    }

    Erase_Cell(OUT);  // just skip to next expression
    Erase_Cell(SPARE);
    unnecessary(Erase_Cell(CURRENT));
    goto start_new_expression;


} case TYPE_FRAME: { //// FRAME! /////////////////////////////////////////////

    // If a FRAME! makes it to the SWITCH statement, that means it is either
    // literally a frame in the array (eval compose [(unrun add/) 1 2]) or
    // is being retriggered via REEVAL.
    //
    // Most FRAME! evaluations come from the antiform ("actions") triggered
    // from a WORD! or PATH! case.)
    //
    // 1. If an infix function is run at this moment, it will not have a left
    //    hand side argument.

    if (Frame_Lens(CURRENT))  // running frame if lensed
        panic ("Use REDO to restart a running FRAME! (can't EVAL)");

    Option(InfixMode) infix_mode = Frame_Infix_Mode(CURRENT);

    require (
      Reuse_Sublevel_Target_Out_For_Action(CURRENT, infix_mode)
    );
    assert(Is_Cell_Erased(OUT));  // so nothing on left [1]

    goto process_action;

} process_action: {

    // Gather args and execute function (the arg gathering makes nested
    // eval calls that lookahead, but no lookahead after the action runs)

    STATE = cast(StepperState, TYPE_FRAME);
    return CONTINUE_SUBLEVEL;

} action_result_in_out: {

    // could in theory examine action level here

    SUBLEVEL->executor = &Just_Use_Out_Executor;  // temporary (?) invariant
    assert(SUBLEVEL->feed == L->feed);  // we didn't change it

    goto lookahead;

} case TYPE_WORD: { //// WORD! ///////////////////////////////////////////////

  // A plain word tries to fetch its value through its binding.  It panics if
  // the word is unbound (or if bound to a variable which holds TRASH!).  If
  // it looks up to an antiform FRAME!, then that "Action" will be invoked.
  //
  // NOTE: The usual dispatch of infix functions is *not* via a TYPE_WORD in
  // this switch, it's by some code at the `lookahead:` label.  You only see
  // infix here when there was nothing to the left, so cases like `(+ 1 2)`

} handle_word_where_action_lookups_are_active: {

    // 1. If we're just going to panic on a TRASH! access, there's no reason
    //    to check for it first.  See if it's an ACTION!, in which case run
    //    it and we don't pay for the trash check.

    assert(not Sigil_Of(CURRENT));

    Element* sub_current = Copy_Cell(Level_Scratch(SUBLEVEL), CURRENT);
    Add_Cell_Sigil(sub_current, SIGIL_META);  // for ACTION! lookup

    heeded (Bind_Cell_If_Unbound(sub_current, L_binding));
    heeded (Corrupt_Cell_If_Needful(SPARE));

    SUBLEVEL->out = OUT;
    LEVEL_STATE_BYTE(SUBLEVEL) = 1;

    require (
      Get_Var_In_Scratch_To_Out(SUBLEVEL, NO_STEPS)
    );

    if (Is_Failure(OUT))  // e.g. couldn't pick word as field from binding
        panic (Cell_Error(OUT));  // don't conflate with action result

    if (Is_Hot_Potato(OUT))
        goto lookahead;  // legal e.g. for VETO

    if (Is_Action(OUT))  // check first [1]
        goto run_action_in_out;

    if (Is_Trash(OUT))  // checked second [1]
        panic (Error_Bad_Word_Get(CURRENT, OUT));

    if (Not_Cell_Stable(OUT))
        panic (Error_Unstable_Non_Meta_Raw(CURRENT));

    Stable* out = As_Stable(OUT);

    if (Get_Cell_Flag(CURRENT, CURRENT_NOTE_RUN_WORD)) {
        if (Is_Frame(out))
            goto run_action_in_out;
        panic ("Leading slash means execute FRAME! or ACTION! only");
    }

    goto lookahead;

} run_action_in_out: {

  // For C-DEBUG-BREAK, it's not that helpful to *actually* dispatch to an
  // action called C-DEBUG-BREAK (because we want to debug the callsite, not
  // the guts of that function).  So if a word looks up to C-DEBUG-BREAK we
  // notice that here, so you get the break right where you want it.
  //
  // Afterwards, we just jump direct to the next expression.  This allows you
  // to break in the middle of things, like:
  //
  //        1 + c-debug-break 2
  //
  // (This only catches eval steps: if you use APPLY to call C-DEBUG-BREAK
  // or if you ADAPT it etc. you'll get the debug_break() inside the call.)

#if INCLUDE_C_DEBUG_BREAK_NATIVE && RUNTIME_CHECKS

  if (Frame_Phase(OUT) == Frame_Phase(LIB(C_DEBUG_BREAK))) {
    // -----------------------------------------------------------------------
      debug_break();  // <-- C_DEBUG_BREAK lands here
    // -----------------------------------------------------------------------

      if (Is_Level_At_End(L)) {
          Init_Void(OUT);  // cue Evaluator_Executor() to keep last result
          goto finished;
      }

      Erase_Cell(OUT);
      Erase_Cell(SPARE);  // may or may not hold lookahead fetch
      unnecessary(Erase_Cell(CURRENT));  // will be fetched by feed advance
      goto start_new_expression;  // requires OUT and SPARE be erased
  }

#endif

} run_non_debug_break_action: {

  // 2. When dispatching infix and you have something on the left, you want to
  //    push the level *after* the flag for infixness has been set...to avoid
  //    overwriting the output cell that's the left hand side input.  But in
  //    this case we don't have a left input, even though we're doing infix.
  //    So pushing *before* we set the flags means the FLAG_STATE_BYTE() will
  //    be 0, and we get clearing.

    Option(InfixMode) infix_mode = Frame_Infix_Mode(OUT);

    if (infix_mode) {
        if (infix_mode != INFIX_TIGHT) {  // defer or postpone
            if (Get_Eval_Executor_Flag(L, FULFILLING_ARG)) {
                Clear_Feed_Flag(L->feed, NO_LOOKAHEAD);
                Set_Feed_Flag(L->feed, DEFERRING_INFIX);
                goto finished;
            }
        }
    }

    if (Get_Eval_Executor_Flag(L, DIDNT_LEFT_QUOTE_PATH)) {
        if (infix_mode)
            assert(false);  // !!! this won't work, can it happen?

        Clear_Eval_Executor_Flag(L, DIDNT_LEFT_QUOTE_PATH);
    }

  #if (! DEBUG_DISABLE_INTRINSICS)
    Details* details = opt Try_Frame_Details(OUT);
    if (
        not infix_mode  // too rare a case for intrinsic optimization
        and details
        and Get_Details_Flag(details, CAN_DISPATCH_AS_INTRINSIC)
        and Not_Level_At_End(L)  // can't do <hole>, fallthru to error
        and not SPORADICALLY(10)  // checked builds sometimes bypass
    ){
        Copy_Plain_Cell(CURRENT, OUT);

        Param* param = Phase_Param(details, 1);

        switch (Parameter_Class(param)) {
          case PARAMCLASS_NORMAL:
            break;

          case PARAMCLASS_META:
            break;

          case PARAMCLASS_LITERAL:
            The_Next_In_Feed(SPARE, L->feed);
            goto intrinsic_arg_in_spare;

          default:
            panic ("Unsupported Intrinsic parameter convention");
        }

        Clear_Feed_Flag(L->feed, NO_LOOKAHEAD);  // when non-infix call

        require (
          Reuse_Sublevel_Target_Spare_For_Intrinsic_Arg()
        );

        Erase_Cell(SPARE);
        STATE = ST_STEPPER_FULFILLING_INTRINSIC_ARG;
        return CONTINUE_SUBLEVEL;
    }
  #endif

    require (
      Reuse_Sublevel_Target_Out_For_Action(OUT, infix_mode)
    );
    Erase_Cell(OUT);  // want OUT clear, even if infix_mode sets state nonzero

    goto process_action;

} intrinsic_arg_in_spare: { /////////////////////////////////////////////////

    SUBLEVEL->executor = &Just_Use_Out_Executor;  // temporary (?) invariant
    assert(SUBLEVEL->feed == L->feed);  // we didn't change it

  #if (! DEBUG_DISABLE_INTRINSICS)

    Details* details = Ensure_Frame_Details(CURRENT);

    Param* param = Phase_Params_Head(details);
    if (
        Is_Cell_A_Veto_Hot_Potato(SPARE)
        and Not_Parameter_Flag(param, WANT_VETO)
    ){
        Init_Null_Signifying_Vetoed(OUT);
        Set_Eval_Executor_Flag(L, OUT_IS_DISCARDABLE);
        goto lookahead;
    }
    if (Get_Parameter_Flag(param, UNDO_OPT) and Any_Void(SPARE)) {
        Init_Null(SPARE);
    }
    else if (Parameter_Class(param) != PARAMCLASS_META) {
      require (
        Decay_If_Unstable(SPARE)  // decay may eval, do before intrinsic
      );
    }

    Dispatcher* dispatcher = Details_Dispatcher(details);

    assert(Not_Level_Flag(SUBLEVEL, DISPATCHING_INTRINSIC));
    Set_Level_Flag(SUBLEVEL, DISPATCHING_INTRINSIC);  // level_ is not its Level
    dont(Set_Level_Flag(SUBLEVEL, RUNNING_TYPECHECK));  // want panic if bad args

    Copy_Cell(Level_Spare(SUBLEVEL), SPARE);
    Copy_Cell(Level_Scratch(SUBLEVEL), CURRENT);
    SUBLEVEL->out = OUT;

    Bounce b = Apply_Cfunc(dispatcher, SUBLEVEL);

  #if RUNTIME_CHECKS
    if (
        b != nullptr
        and b != BOUNCE_OKAY
        and Is_Intrinsic_Typechecker(details)
    ){
        panic ("Intrinsic typechecker overwrote output cell");
    }
  #endif

    b = opt Irreducible_Bounce(L, b);
    if (b)  // can't BOUNCE_CONTINUE etc. from an intrinsic dispatch
        panic ("Intrinsic dispatcher returned Irreducible Bounce");

    if (
        Get_Level_Flag(L, VANISHABLE_VOIDS_ONLY)
        and Not_Cell_Flag(CURRENT, WEIRD_VANISHABLE)
        and Is_Void(OUT)
    ){
        Note_Level_Out_As_Void_To_Make_Heavy(L);
    }

    Clear_Level_Flag(SUBLEVEL, DISPATCHING_INTRINSIC);
    if (Not_Details_Flag(details, PURE))
        Set_Eval_Executor_Flag(L, OUT_IS_DISCARDABLE);
    goto lookahead;

  #endif  // DEBUG_DISABLE_INTRINSICS

} handle_chain_or_meta_chain:  //// CHAIN! [ a:  ^a:  b:c:d  ^:e ] /////////
  case TYPE_CHAIN: {

    // Due to the consolidation of all the SET-XXX! and GET-XXX! types as
    // CHAIN! with leading or trailing blanks, CHAIN! has to break that down
    // and dispatch to the appropriate behavior.
    //
    // 1. There's a weird feature of binding being sensitive to SET-WORD!s
    //    only that is currently broken.  When we convert a SET-WORD to a
    //    WORD! to do the assign, that feature is lost.

    Option(SingleHeart) singleheart = Try_Get_Sequence_Singleheart(CURRENT);
    if (singleheart) {
        assume (
          Unsingleheart_Sequence_Preserve_Sigil(CURRENT)
        );
    }
    switch (opt singleheart) {
      case NOT_SINGLEHEART_0:
        break;  // wasn't xxx: or :xxx where xxx is BLOCK!/CHAIN!/WORD!/etc

      case TRAILING_BLANK_AND(WORD): {  // FOO: or ^FOO:
        Bind_Cell_If_Unbound(CURRENT, L_binding);
        goto handle_generic_set; }

      case TRAILING_BLANK_AND(TUPLE): {  // a.b.c: is a set tuple
        goto handle_generic_set; }

      case TRAILING_BLANK_AND(BLOCK): {  // [a b]: multi-return assign
        goto handle_set_block; }

      case TRAILING_BLANK_AND(GROUP): {  // (xxx): -- generic retrigger set
        Bind_Cell_If_Unbound(CURRENT, L_binding);
        require (
          Reuse_Sublevel_Target_Out_For_Eval(CURRENT)
        );
        // (not LEVEL_FLAG_VANISHABLE_VOIDS_ONLY)

        Erase_Cell(OUT);
        STATE = ST_STEPPER_SET_GROUP;
        return CONTINUE_SUBLEVEL; }

      case LEADING_BLANK_AND(WORD):  // :FOO -turn voids to null
      case LEADING_BLANK_AND(TUPLE): {  // :a.b.c - same
        Element* sub_current = Copy_Cell(Level_Scratch(SUBLEVEL), CURRENT);
        heeded (Add_Cell_Sigil(sub_current, SIGIL_META));
        Bind_Cell_If_Unbound(sub_current, L_binding);
        heeded (Corrupt_Cell_If_Needful(Level_Spare(SUBLEVEL)));

        SUBLEVEL->out = OUT;
        LEVEL_STATE_BYTE(SUBLEVEL) = 1;

        require (
          Get_Var_In_Scratch_To_Out(SUBLEVEL, GROUPS_OK)
        );
        if (Not_Cell_Stable(OUT)) {
            if (Is_Void(OUT) or Is_Trash(OUT))
                Init_Null(OUT);
            else if (Is_Action(OUT))
                Deactivate_Action(OUT);
            else
                panic (":WORD!/:TUPLE! gave unstable non-void/trash/action");
        }
        goto lookahead; }

      case LEADING_BLANK_AND(BLOCK): {  // !!! :[a b] reduces, not great...
        Bind_Cell_If_Unbound(CURRENT, L_binding);
        if (rebRunThrows(
            OUT,  // <-- output
            CANON(REDUCE), CURRENT
        )){
            goto return_thrown;
        }
        goto lookahead; }

      case LEADING_BLANK_AND(GROUP): {
        panic ("GET-GROUP! has no evaluator meaning at this time"); }

      default:  // it's just something like :1 or <tag>:
        panic ("No current evaluation for things like :1 or <tag>:");
    }

    require (
      Get_Chain_Push_Refinements(
        OUT,  // where to write action
        CURRENT,
        L_binding
    ));

    assert(Is_Action(OUT));

    if (Is_Frame_Infix(OUT)) {  // too late, left already evaluated
        Drop_Data_Stack_To(STACK_BASE);
        panic ("Use `->-` to shove left infix operands into CHAIN!s");
    }

} handle_action_in_out_with_refinements_pushed: {

    require (
      Reuse_Sublevel_Target_Out_For_Action(OUT, PREFIX_0)
    );
    SUBLEVEL->baseline.stack_base = STACK_BASE;  // !!! refinements, review
    Erase_Cell(OUT);  // not infix, sub state is 0
    goto process_action;


} case TYPE_GROUP: //// GROUP! (...) /////////////////////////////////////////
  handle_group_or_meta_group: {

  // Groups simply evaluate their contents, and can evaluate to VOID! if the
  // contents completely disappear.
  //
  // 1. For an explanation of starting this particular Evaluator_Executor()
  //    as being unafraid of voids, see notes at the top of %c-eval.c
  //    Simply put, we want `expr` and `(expr)` to behave similarly, and the
  //    constraints forcing `eval [expr] to be different from `expr` w.r.t.
  //    VOID! vanishing don't apply to inline groups.

    require (
      Reuse_Sublevel_Target_Out_For_Eval(CURRENT)
    );

    Erase_Cell(OUT);
    return CONTINUE_SUBLEVEL;

} group_or_meta_group_result_in_out: {

    SUBLEVEL->executor = &Just_Use_Out_Executor;  // temporary (?) invariant
    Release_Feed(SUBLEVEL->feed);
    SUBLEVEL->feed = L->feed;  // restore this (also invariant)
    Add_Feed_Reference(SUBLEVEL->feed);

    L->flags.bits |= (
        SUBLEVEL->flags.bits & EVAL_EXECUTOR_FLAG_OUT_IS_DISCARDABLE
    );

    if (Is_Group(CURRENT))
        goto lookahead;  // not decayed, result is good

    assert(Is_Meta_Form_Of(GROUP, CURRENT));

    panic ("^(...) behavior is likely to act like ^ (...), in time");

} case TYPE_TUPLE: { //// TUPLE! [ a.  b.c.d  .e ] ///////////////////////////

    // TUPLE! runs through an extensible mechanism based on PICK and POKE.
    // Hence `a.b.c` is kind of like a shorthand for `pick (pick a 'b) 'c`.
    //
    // In actuality, the mechanism is more sophisticated than that...because
    // some picking does "sub-value" addressing.  For more details, see the
    // explanation in %sys-pick.h
    //
    // For now, we defer to what GET does.
    //
    // Tuples looking up to nothing (~ antiform) are handled consistently with
    // WORD! and GET-WORD!, and will error...directing you use GET:ANY if
    // fetching nothing is what you actually intended.
    //
    // 1. Cases like (^obj.lifted-error) and (obj.missing-field) will return
    //    a FAILURE! antiform, and (^obj.lifted-pack) can return a PACK!, etc.

    Element* spare = Copy_Sequence_At(SPARE, CURRENT, 0);
    bool blank_at_head = Is_Space(spare);
    if (
        not blank_at_head  // `.a` means pick member from "self"
        and Any_Inert(spare)  // `1.2.3` is inert
    ){
        Copy_Cell_May_Bind(OUT, CURRENT, L_binding);
        goto lookahead;
    }

    Element* sub_current = Copy_Cell(Level_Scratch(SUBLEVEL), CURRENT);
    heeded (Bind_Cell_If_Unbound(sub_current, L_binding));
    heeded (sub_current);
    heeded (Corrupt_Cell_If_Needful(Level_Spare(SUBLEVEL)));

    SUBLEVEL->out = OUT;
    LEVEL_STATE_BYTE(SUBLEVEL) = 1;

    Get_Var_In_Scratch_To_Out(SUBLEVEL, GROUPS_OK) except (Error* e) {
        Init_Context_Cell(OUT, TYPE_ERROR, e);
        Failify_Cell(OUT);
    } else {
        possibly(Not_Cell_Stable(OUT));  // unmeta'd item [1]
    }

    Set_Eval_Executor_Flag(L, OUT_IS_DISCARDABLE);
    goto lookahead;


} case TYPE_PATH: { //// PATH! [ a/  b/c/d  e/ ] /////////////////////////////

  // Ren-C moved to member access with "dots instead of slashes" (TUPLE!)
  // and refinements are done with "colons instead of slashes" (CHAIN!).  So
  // PATH!s role has come to be specificially dealing with functions:
  //
  //   * abc/     - means ABC is a function, return it as-is
  //   * /abc     - means ensure ABC is a function and run it
  //   * abc/def  - means ABC is a context, DEF is a function, run it
  //   * abc/def/ - means ABC and DEF are functions, compose them
  //
  // 1. It's likely that paths like 1/2 or otherwise inert-headed will be
  //    inert and evaluate to themselves.
  //
  // 2. Slash at head will signal running actions soon enough.  But for the
  //    moment it is still refinement.  Let's try not binding it by default
  //    just to see what headaches that causes...if any.

} handle_path_where_action_lookups_are_active: {

    bool slash_at_head;
    bool slash_at_tail;
    Option(SingleHeart) single = Try_Get_Sequence_Singleheart(CURRENT);

    if (not single) {
        Element* spare = Copy_Sequence_At(SPARE, CURRENT, 0);
        if (Any_Inert(spare)) {
            if (Is_Space(spare))
                slash_at_head = true;
            else {
                Copy_Cell_May_Bind(OUT, CURRENT, L_binding);  // inert [2]
                goto lookahead;
            }
        }
        else
            slash_at_head = false;

        Length len = Sequence_Len(CURRENT);
        spare = Copy_Sequence_At(SPARE, CURRENT, len - 1);
        slash_at_tail = Is_Space(spare);
    }
    else switch (unwrap single) {
      case LEADING_BLANK_AND(WORD): {
        assume (
          Unsingleheart_Sequence(CURRENT)
        );
        Set_Cell_Flag(CURRENT, CURRENT_NOTE_RUN_WORD);
        goto handle_word_where_action_lookups_are_active; }

      case LEADING_BLANK_AND(CHAIN): {  // /abc: or /?:?:?
        assume (
          Unsingleheart_Sequence(CURRENT)
        );

        switch (opt Try_Get_Sequence_Singleheart(CURRENT)) {
          case TRAILING_BLANK_AND(WORD): {  // /abc: is set actions only
            assume (
              Unsingleheart_Sequence(CURRENT)
            );
            Set_Cell_Flag(CURRENT, SCRATCH_VAR_NOTE_ONLY_ACTION);
            goto handle_generic_set; }

          case TRAILING_BLANK_AND(TUPLE): {  // /a.b.c: is set actions only
            assume (
              Unsingleheart_Sequence(CURRENT)
            );
            Set_Cell_Flag(CURRENT, SCRATCH_VAR_NOTE_ONLY_ACTION);
            goto handle_generic_set; }

          default:
            panic ("/a:b:c will guarantee a function call, in time");
        }
        break; }

      default:
        slash_at_tail = Singleheart_Has_Trailing_Blank(unwrap single);
        slash_at_head = Singleheart_Has_Leading_Blank(unwrap single);
        assert(slash_at_head == not slash_at_tail);
        break;
    }

  delegate_to_get_path: {

  // 1. It would not make sense to return a definitional error when a path
  //    lookup does not exist.  Imagine making null back for `try lib/append`
  //    if you wrote `try lib/append [a b c] [d e]` when lib/append did not
  //    exist--that's completely broken.
  //
  // 2. Since paths with trailing slashes just return the action as-is, it's
  //    an arity-0 operation.  Returning a definitional error isn't complete
  //    nonsense, but still might not be great.  Review the choice.
  //
  // 3. Trailing slash notation is a particularly appealing way of denoting
  //    that something is an action, and that you'd like to fetch it in a
  //    way that does not take arguments:
  //
  //         for-next: specialize for-skip/ [skip: 1]
  //         ;                         ---^
  //         ; slash helps show block is not argument
  //
  // 4. Left hand sides don't not look ahead at paths to find infix functions.
  //    This is because PATH! dispatch is costly and can error in more ways
  //    than sniffing a simple WORD! for infix can.  So the prescribed way of
  //    running infix with paths is `left ->- right/side`, which uses an infix
  //    WORD! to mediate the interaction.

    Element* sub_current = Copy_Cell(Level_Scratch(SUBLEVEL), CURRENT);
    heeded (Bind_Cell_If_Unbound(sub_current, L_binding));
    heeded (Corrupt_Cell_If_Needful(Level_Spare(SUBLEVEL)));

    possibly(TOP_INDEX != STACK_BASE);  // make map!, reduce [], etc.
    StackIndex base = TOP_INDEX;

    SUBLEVEL->out = OUT;
    LEVEL_STATE_BYTE(SUBLEVEL) = 1;

    Get_Path_Push_Refinements(SUBLEVEL) except (Error* e) {
        possibly(slash_at_tail);  // ...or, exception for arity-0? [2]
        panic (e);  // don't FAIL, PANIC [1]
    }
    assert(Is_Action(OUT));

    if (slash_at_tail) {  // do not run action, just return it [3]
        if (TOP_INDEX != base) {
            if (Specialize_Action_Throws(
                SPARE, OUT, nullptr, base
            )){
                goto return_thrown;
            }
            Move_Cell(OUT, SPARE);
        }
        goto lookahead;
    }

    if (Is_Frame_Infix(OUT)) {  // too late, left already evaluated [4]
        Drop_Data_Stack_To(STACK_BASE);
        panic ("Use `->-` to shove left infix operands into PATH!s");
    }

    UNUSED(slash_at_head);  // !!! should e.g. enforce /1.2.3 as error?
    goto handle_action_in_out_with_refinements_pushed;

}} handle_generic_set: { /////////////////////////////////////////////////////

  // Right side is evaluated into `out`, and then copied to the variable.
  //
  // !!! The evaluation ordering is dictated by the fact that there isn't a
  // separate "evaluate tuple to target location" and "set target' step.  This
  // is because some assignment targets (e.g. gob.size.x:) do not correspond
  // to a cell that can be returned; the tuple operation "encodes as it goes"
  // and requires the value to set as a parameter.  It is counterintuitive
  // given the "left-to-right" nature of the language:
  //
  //     >> foo: make object! [[bar][bar: 10]]
  //
  //     >> foo.(print "left" 'bar): (print "right" 20)
  //     right
  //     left
  //     == 20

    assert(
        Is_Word(CURRENT) or Is_Meta_Form_Of(WORD, CURRENT)
        or Is_Tuple(CURRENT) or Is_Meta_Form_Of(TUPLE, CURRENT)
        or Is_Space(CURRENT)
    );
    STATE = ST_STEPPER_GENERIC_SET;

    require (
      Reuse_Sublevel_Target_Out_For_Step()
    );

    return CONTINUE_SUBLEVEL;

} generic_set_rightside_in_out: {

    SUBLEVEL->executor = &Just_Use_Out_Executor;  // temporary (?) invariant
    assert(SUBLEVEL->feed == L->feed);  // we didn't change it

    if (Is_Space(CURRENT))  // e.g. `(void): ...`  !!! use space var!
        goto lookahead;  // pass through everything

    if (Is_Failure(OUT) and not Is_Metaform(CURRENT))
        goto lookahead;  // you can say (try var: fail "hi") without panicking

    Element* sub_current = As_Element(
        Copy_Cell_Core(
            Level_Scratch(SUBLEVEL),
            CURRENT,
            CELL_MASK_COPY | CELL_FLAG_SCRATCH_VAR_NOTE_ONLY_ACTION
        )
    );
    heeded (Bind_Cell_If_Unbound(sub_current, L_binding));
    heeded (sub_current);
    heeded (Corrupt_Cell_If_Needful(Level_Spare(SUBLEVEL)));
    SUBLEVEL->out = OUT;

    Set_Var_In_Scratch_To_Out(SUBLEVEL, GROUPS_OK) except (Error* e) {
        Init_Context_Cell(OUT, TYPE_ERROR, e);
        Failify_Cell(OUT);
    }

    Set_Eval_Executor_Flag(L, OUT_IS_DISCARDABLE);
    goto lookahead;

} set_group_result_in_out: {

    SUBLEVEL->executor = &Just_Use_Out_Executor;  // temporary (?) invariant
    Release_Feed(SUBLEVEL->feed);
    SUBLEVEL->feed = L->feed;  // restore this (also invariant)
    Add_Feed_Reference(SUBLEVEL->feed);

    if (Is_Space(CURRENT))
       goto handle_generic_set;

    if (Any_Void(OUT)) {
        Init_Space(CURRENT);  // can't put voids in feed position
        goto handle_generic_set;
    }

    require (
      Stable* stable_out = Decay_If_Unstable(OUT)
    );

    switch (opt Type_Of(stable_out)) {
      case TYPE_BLOCK:
        Copy_Cell(CURRENT, As_Element(stable_out));
        Erase_Cell(OUT);  // !!! necessary?
        goto handle_set_block;

      case TYPE_WORD:
        Copy_Cell(CURRENT, As_Element(stable_out));
        Erase_Cell(OUT);  // !!! necessary?
        goto handle_generic_set;

      case TYPE_TUPLE:
        Copy_Cell(CURRENT, As_Element(stable_out));
        Erase_Cell(OUT);  // !!! necessary?
        goto handle_generic_set;

      default:
        break;
    }

    panic ("Unknown type for use in SET-GROUP!");

} handle_set_block: {  ///////////////////////////////////////////////////////

    STATE = ST_STEPPER_SET_BLOCK;

    Copy_Cell(Level_Scratch(SUBLEVEL), CURRENT);

    require (
      bool threw = Push_Set_Block_Instructions_To_Stack_Throws(
        SUBLEVEL, L_binding
      )
    );
    if (threw)
        goto return_thrown;

    SUBLEVEL->baseline.stack_base = TOP_INDEX;

    require (
      Reuse_Sublevel_Target_Out_For_Step()
    );

    return CONTINUE_SUBLEVEL;

} set_block_rightside_in_out: {  /////////////////////////////////////////////

    SUBLEVEL->executor = &Just_Use_Out_Executor;  // temporary (?) invariant
    assert(SUBLEVEL->feed == L->feed);  // we didn't change it

    SUBLEVEL->baseline.stack_base = STACK_BASE;
    SUBLEVEL->out = OUT;

    require (
      Set_Block_From_Instructions_On_Stack_To_Out(SUBLEVEL)
    );
    Set_Eval_Executor_Flag(L, OUT_IS_DISCARDABLE);
    goto lookahead;


} case TYPE_FENCE: { ///// FENCE! {...} //////////////////////////////////////

    // FENCE! is the guinea pig for a technique of calling a function defined
    // in the local environment to do the handling.

    Element* out = Inertly_Derelativize_Inheriting_Const(OUT, CURRENT, L->feed);
    Quote_Cell(out);

    Element* spare = Init_Word(SPARE, CANON(FENCE_X_EVAL));
    dont(Quote_Cell(As_Element(SPARE)));  // want to run word

    Api(Value*) temp = rebValue_helper(  // pass binding explicitly to helper
        cast(RebolContext*, Level_Binding(L)),
        spare, out,
        rebEND  // must pass END explicitly to helper
    );
    Copy_Cell(OUT, temp);
    rebRelease(temp);
    goto lookahead;


} case TYPE_0_constexpr: //// "INERT" TYPES (EXTENSIBILITY TBD) //////////////
  case TYPE_BLOCK:
  case TYPE_BLOB:
  case TYPE_TEXT:
  case TYPE_FILE:
  case TYPE_EMAIL:
  case TYPE_URL:
  case TYPE_TAG:
  case TYPE_RUNE:
  case TYPE_BITSET:
  case TYPE_MAP:
  case TYPE_VARARGS:
  case TYPE_OBJECT:
  case TYPE_MODULE:
  case TYPE_ERROR:
  case TYPE_PORT:
  case TYPE_LET:
  case TYPE_INTEGER:
  case TYPE_DECIMAL:
  case TYPE_PERCENT:
  case TYPE_PAIR:
  case TYPE_TIME:
  case TYPE_DATE:
  case TYPE_PARAMETER:
  case TYPE_HANDLE: {

    // Today these datatypes are all inert, but with RebindableSyntax the
    // concept is that you can define a function that says how things like
    // INTEGER! will behave.

    Inertly_Derelativize_Inheriting_Const(OUT, CURRENT, L->feed);
    goto lookahead;


} default: { //// !! CORRUPTION (pseudotypes or otherwise) !! ////////////////

    crash (CURRENT);

  }}  //// END handle_plain: SWITCH STATEMENT ////////////////////////////////


} lookahead: { ///////////////////////////////////////////////////////////////

    // We're sitting at what "looks like the end" of an evaluation step.
    // But we still have to consider infix.  e.g.
    //
    //    [pos val]: evaluate:step [1 + 2 * 3]
    //
    // We want that to give a position of [] and `val = 9`.  The evaluator
    // cannot just dispatch on TYPE_INTEGER in the switch() above, give you 1,
    // and consider its job done.  It has to notice that the word `+` looks up
    // to an ACTION! whose cell has an InfixMode set in the header.
    //
    // Next, there's a subtlety with FEED_FLAG_NO_LOOKAHEAD which explains why
    // processing of the 2 argument doesn't greedily continue to advance, but
    // waits for `1 + 2` to finish.  This is because the right hand argument
    // of math operations tend to be declared #tight.
    //
    // If that's not enough to consider :-) it can even be the case that
    // subsequent infix gets "deferred".  Then, possibly later the evaluated
    // value gets re-fed back in, and we jump right to this post-switch point
    // to give it a "second chance" to take the infix.  (See 'deferred'.)
    //
    // So this post-switch step is where all of it happens, and it's tricky!
    //
    // 1. If something was run with the expectation it should take the next
    //    arg from the output cell, and an evaluation cycle ran that wasn't
    //    an ACTION! (or that was an arity-0 action), that's not what was
    //    meant.  But it can happen, e.g. `x: 10 | x ->-`, where ->- doesn't
    //    get an opportunity to quote left because it has no argument...and
    //    instead retriggers and lets x run.
    //
    // 2. For long-pondered technical reasons, only WORD! is able to dispatch
    //    infix.  If necessary to dispatch an infix function via path, then
    //    a word is used to do it, like `>>` in `x: >> lib/default [...]`.

    assert(SUBLEVEL->executor == &Just_Use_Out_Executor);

    if (Get_Eval_Executor_Flag(L, DIDNT_LEFT_QUOTE_PATH))
        panic (Error_Literal_Left_Path_Raw());  // [1]

    if (Next_Not_Word_Or_Is_Newline_Or_End(L)) {
        possibly(Is_Antiform(L_next));  // API calls, rebValue("^", antiform)
        Clear_Feed_Flag(L->feed, NO_LOOKAHEAD);
        goto finished;
    }

    Element* sub_current = Copy_Cell(
        Level_Scratch(SUBLEVEL), As_Element(L_next)
    );
    Bind_Cell_If_Unbound(sub_current, L_binding);
    Add_Cell_Sigil(sub_current, SIGIL_META);  // need for unstable lookup
    heeded (sub_current);
    heeded (Corrupt_Cell_If_Needful(Level_Spare(SUBLEVEL)));

    Erase_Cell(SPARE);  // ideally we'd reuse if we could

    SUBLEVEL->out = SPARE;
    LEVEL_STATE_BYTE(SUBLEVEL) = 1;

    Get_Var_In_Scratch_To_Out(SUBLEVEL, NO_STEPS) except (Error* e) {
        UNUSED(e);
        goto finished;  // lookup failure not a problem (for *this* step)
    }

} test_word_or_action_for_infix: { ///////////////////////////////////////////

    Option(InfixMode) infix_mode;

    if (
        (
            not (
                Is_Word(As_Element(L_next))
                and Is_Action(SPARE)
            )
            and not Is_Frame(As_Element(L_next))
        )
        or not (infix_mode = Frame_Infix_Mode(SPARE))
    ){
      lookback_quote_too_late: // run as if starting new expression

        Clear_Feed_Flag(L->feed, NO_LOOKAHEAD);
        Clear_Eval_Executor_Flag(L, INERT_OPTIMIZATION);

        goto finished;
    }

  check_for_literal_first: { /////////////////////////////////////////////////

    // 1. Left-quoting by infix needs to be done in the lookahead before an
    //    evaluation, not this one that's after.  This happens in cases like:
    //
    //        left-the: infix func [@value] [value]
    //        the <something> left-the
    //
    //    But due to the existence of <hole>-able parameters, the left quoting
    //    function might be okay with seeing nothing on the left.  Start a
    //    new expression and let it error if that's not ok.

    Phase* infixed = Frame_Phase(SPARE);
    ParamList* paramlist = Phase_Paramlist(infixed);

    if (Get_Flavor_Flag(VARLIST, paramlist, PARAMLIST_LITERAL_FIRST)) {  // [1]
        assert(Not_Eval_Executor_Flag(L, DIDNT_LEFT_QUOTE_PATH));
        if (Get_Eval_Executor_Flag(L, DIDNT_LEFT_QUOTE_PATH))
            panic (Error_Literal_Left_Path_Raw());

        const Param* first = First_Unspecialized_Param(nullptr, infixed);
        if (Parameter_Class(first) == PARAMCLASS_SOFT) {
            if (Get_Feed_Flag(L->feed, NO_LOOKAHEAD)) {
                Clear_Feed_Flag(L->feed, NO_LOOKAHEAD);
                Clear_Eval_Executor_Flag(L, INERT_OPTIMIZATION);
                goto finished;
            }
        }
        else if (Not_Eval_Executor_Flag(L, INERT_OPTIMIZATION))
            goto lookback_quote_too_late;
    }

    Clear_Eval_Executor_Flag(L, INERT_OPTIMIZATION);  // served purpose if set

    if (
        Get_Eval_Executor_Flag(L, FULFILLING_ARG)
        and infix_mode != INFIX_DEFER
                            // ^-- (1 + if null [2] else [3]) => 4
    ){
        if (Get_Feed_Flag(L->feed, NO_LOOKAHEAD)) {
            // Don't do infix lookahead if asked *not* to look.

            Clear_Feed_Flag(L->feed, NO_LOOKAHEAD);

            assert(Not_Feed_Flag(L->feed, DEFERRING_INFIX));
            Set_Feed_Flag(L->feed, DEFERRING_INFIX);

            goto finished;
        }

        Clear_Feed_Flag(L->feed, NO_LOOKAHEAD);
    }

    // A deferral occurs, e.g. with:
    //
    //     return if condition [...] else [...]
    //
    // The first time the ELSE is seen, IF is fulfilling its branch argument
    // and doesn't know if its done or not.  So this code senses that and
    // runs, returning the output without running ELSE, but setting a flag
    // to know not to do the deferral more than once.
    //
    if (
        Get_Eval_Executor_Flag(L, FULFILLING_ARG)
        and (
            infix_mode == INFIX_POSTPONE
            or (
                infix_mode == INFIX_DEFER
                and Not_Feed_Flag(L->feed, DEFERRING_INFIX)
            )
        )
    ){
        if (
            Is_Action_Level(L->prior)
            and Get_Executor_Flag(ACTION, L->prior, ERROR_ON_DEFERRED_INFIX)
        ){
            // Operations that inline functions by proxy (such as MATCH and
            // ENSURE) cannot directly interoperate with THEN or ELSE...they
            // are building a frame with PG_Dummy_Action as the function, so
            // running a deferred operation in the same step is not an option.
            // The expression to the left must be in a GROUP!.
            //
            panic (Error_Ambiguous_Infix_Raw());
        }

        Clear_Feed_Flag(L->feed, NO_LOOKAHEAD);

        if (
            Is_Action_Level(L->prior)
            //
            // ^-- !!! Before stackless it was always the case when we got
            // here that a function level was fulfilling, because setting word
            // would reuse levels while fulfilling arguments...but stackless
            // changed this and has setting words start new Levels.  Review.
            //
            and not Is_Level_Fulfilling_Or_Typechecking(L->prior)
        ){
            // This should mean it's a variadic level, e.g. when we have
            // the 2 in the output slot and are at the THEN in:
            //
            //     variadic2 1 2 then (t => [print ["t is" t] <then>])
            //
            // We used to treat this like a barrier, but there is now no such
            // thing as a "BARRIER_HIT" flag.  What should we do now?  Try
            // just jumping to `finished`.
            //
            goto finished;
        }

        Set_Feed_Flag(L->feed, DEFERRING_INFIX);

        // Leave infix operator pending in the feed.  It's up to the parent
        // level to decide whether to ST_STEPPER_LOOKING_AHEAD to jump
        // back in and finish fulfilling this arg or not.  If it does resume
        // and we get to this check again, L->prior->deferred can't be null,
        // otherwise it would be an infinite loop.
        //
        goto finished;
    }

    Clear_Feed_Flag(L->feed, DEFERRING_INFIX);

    // An evaluative lookback argument we don't want to defer, e.g. a normal
    // argument or a deferable one which is not being requested in the context
    // of parameter fulfillment.  We want to reuse the OUT value and get it
    // into the new function's frame.

    require (
      Reuse_Sublevel_Target_Out_For_Action(SPARE, infix_mode)
    );

    Fetch_Next_In_Feed(L->feed);

    if (infix_mode == PREFIX_0)  // infix_mode sets STATE_0 if prefix
        Erase_Cell(OUT);
    goto process_action;


}} finished: { ///////////////////////////////////////////////////////////////

  // 1. Want to keep this flag between an operation and an ensuing infix in
  //    the same level, so can't clear in Drop_Action(), e.g. due to:
  //
  //        left-the: infix the/
  //        o: make object! [f: does [1]]
  //        o.f left-the  ; want error suggesting SHOVE, need flag for it
  //
  // 2. We wait until the end of the stepper to turn voids into heavy void.
  //    If we did it sooner, we would prevent infix routines (e.g. ELSE)
  //    from seeing the voids.

    assert(TOP_LEVEL->prior == L);
    Drop_Level(TOP_LEVEL);

    Clear_Eval_Executor_Flag(L, DIDNT_LEFT_QUOTE_PATH);  // [1]

    if (Is_Level_Out_Noted_Void_To_Make_Heavy(L))  // [2]
        Init_Heavy_Void(OUT);

  #if RUNTIME_CHECKS
    Stepper_Exiting_Checks(L);

    possibly(STATE == ST_STEPPER_INITIAL_ENTRY);  // TYPE_0 is state 0
    STATE = ST_STEPPER_FINISHED_DEBUG;  // must reset to STATE_0 if reused
  #endif

    possibly(Not_Eval_Executor_Flag(L, OUT_IS_DISCARDABLE));
    return OUT;


} return_thrown: { ///////////////////////////////////////////////////////////

    assert(TOP_LEVEL->prior == L);
    Drop_Level(TOP_LEVEL);

  #if RUNTIME_CHECKS
    Stepper_Exiting_Checks(L);
  #endif

    return BOUNCE_THROWN;
}}
