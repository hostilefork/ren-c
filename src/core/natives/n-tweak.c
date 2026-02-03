//
//  file: %n-tweak.c
//  summary: "Core functionality unifying getting and setting"
//  section: natives
//  project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2024 Ren-C Open Source Contributors
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
// GET and SET tend to share a lot of work, so they unified on a common set
// of infrastructure called TWEAK.
//
// TWEAK speaks only on the level of single variables, so it doesn't know how
// to set things like BLOCK!: those higher-level abilities are in GET and SET.
//

#include "sys-core.h"


// Although TUPLE! uses quotes to indicate a desire to unbind the target, the
// STEPS pushed to stack uses quotes and quasiforms to be literal lifted
// states.  So once a step is produced, the unbind instruction has to be
// encoded another way.
//
// That encoding hasn't been worked out yet to persist, so it's not persistent
// at the moment--it's just a cell flag that is processed for one TWEAK but
// isn't popped into the steps.  Review.
//
#define CELL_FLAG_STEP_NOTE_WANTS_UNBIND  CELL_FLAG_NOTE


// We want to allow (append.series) to give you back a PARAMETER!, this may
// be applicable to other antiforms also (SPLICE!, maybe?)  But probably too
// risky to let you do it with FAILURE!, and misleading to do it with PACK!.
//
static Option(Error*) Trap_Adjust_Lifted_Antiform_For_Tweak(Value* spare)
{
    assert(Is_Lifted_Antiform(spare));
    if (Heart_Of(spare) == TYPE_FRAME) {  // e.g. (append.series)
        LIFT_BYTE_RAW(spare) = ONEQUOTE_NONQUASI_5;
        return SUCCESS;
    }

    return Error_User("TWEAK* cannot be used on non-ACTION! antiforms");
}


//
//  Trap_Call_Pick_Refresh_Dual_In_Spare: C
//
// This breaks out the stylized code for calling TWEAK*, in a Level that
// can be reused across multiple TWEAK* calls.
//
// The stylization is to reduce the number of C-stack-based cells that need
// to be protected from GC.  Instead, cells are written directly into the
// locations they need to be, with careful orchestration.  (This also means
// less make-work of copying bits around from one location to another.)
//
// 1. SPARE indicates both the LOCATION used for the TWEAK*, and the output
//    of the TWEAK* call.  It's a "dual" because for normal values it is
//    a lifted representation--but if it's a non-lifted ACTION! then it is
//    a function to call to do the next TWEAK* with.  This prevents explosions
//    in cases like (some-struct.million-ints.10), where you don't want the
//    (some-struct.million-ints) pick to give back a BLOCK! of a million
//    INTEGER!s just so you can pick one of them out of it.
//
Option(Error*) Trap_Call_Pick_Refresh_Dual_In_Spare(  // [1]
    Level* parent,
    Level* const sub,  // sublevel will Push_Level() if not already pushed
    StackIndex picker_index,
    bool dont_indirect
){
    Option(Heart) adjusted = none;

    Element* location_arg;
    Stable* picker_arg;
    Element* dual_arg;

  adjust_antiform_pick_if_needed: {

    Dual* dual_spare = As_Dual(Level_Spare(parent));
    if (Is_Lifted_Antiform(dual_spare)) {
        adjusted = Heart_Of_Unsigiled_Isotopic(dual_spare);
        Option(Error*) e = Trap_Adjust_Lifted_Antiform_For_Tweak(dual_spare);
        if (e)
            return e;  // don't panic, caller will handle
    }

    require (
      Push_Action(sub, LIB(TWEAK_P), PREFIX_0)
    );

  proxy_arguments_to_frame_dont_panic_in_this_scope: {

  // We carefully lay things out so the old SPARE gets moved into the frame,
  // to free it up to be used for the output.  But this is delicate, as we
  // cannot panic() while an allocated-but-not-pushed Level is extant.
  // So everything in this section must succeed.

    USE_LEVEL_SHORTHANDS (sub);
    INCLUDE_PARAMS_OF_TWEAK_P;

    assert(Is_Possibly_Unstable_Value_Quoted(dual_spare));
    location_arg = Copy_Cell(
        Erase_ARG(LOCATION),
        dual_spare
    );
    Unquote_Cell(location_arg);

    picker_arg = Copy_Cell(
        Erase_ARG(PICKER),
        Data_Stack_At(Stable, picker_index)
    );

    dual_arg = Init_Null_Signifying_Tweak_Is_Pick(Erase_ARG(DUAL));
    USED(dual_arg);

}} erase_parent_spare_now_that_we_are_done_extracting_it: {

    assert(sub->out == Level_Spare(parent));
    Erase_Cell(sub->out);

} adjust_frame_arguments_now_that_its_safe_to_panic: {

    if (Any_Lifted(picker_arg)) {  // literal x.'y or x.('y) => 'y
        Known_Stable_Unlift_Cell(picker_arg);

        if (Is_Logic(picker_arg)) {
            Drop_Action(sub);
            return Error_User(
                "PICK with logic picker never allowed"
            );
        }
    }
    else {
        Element* pick_instruction = As_Element(picker_arg);
        if (Sigil_Of(pick_instruction)) {
            Drop_Action(sub);
            return Error_User(
                "PICK instruction cannot have sigil for variable access"
            );
        }
    }

} call_tweak: {

  // We actually call TWEAK*, the lower-level function that uses the dual
  // protocol--instead of PICK.

    if (SPORADICALLY(32)) {
        LEVEL_STATE_BYTE(sub) = ST_ACTION_TYPECHECKING;
    } else {
        Mark_Typechecked(u_cast(Param*, location_arg));
        Mark_Typechecked(u_cast(Param*, picker_arg));
        Mark_Typechecked(u_cast(Param*, dual_arg));
        Set_Executor_Flag(ACTION, sub, IN_DISPATCH);
    }

    bool threw = Trampoline_With_Top_As_Root_Throws();
    if (threw)  // don't want to return casual error you can TRY from
        return Error_No_Catch_For_Throw(sub);

    assert(sub == TOP_LEVEL);
    unnecessary(Drop_Action(sub));  // !! action is dropped, should it be?

    if (Is_Null_Signifying_Slot_Unavailable(Level_Spare(parent)))
        goto return_without_unbinding;

    Dual* dual_spare = As_Dual(Level_Spare(parent));

    if (Is_Dualized_Bedrock(dual_spare)) {
        if (dont_indirect)
            goto return_without_unbinding;

        goto handle_indirect_pick;
    }

    goto possibly_unbind_spare_and_return;

  handle_indirect_pick: { ////////////////////////////////////////////////////

  // 1. The drain could PANIC regardless of access via VAR or ^VAR, (as it is
  //    "dishonest" to give anything back, when whatever was last assigned was
  //    discarded).  But it's probably more useful if ^VAR is willing to give
  //    a FAILURE! so you can say (try ^var) and get NULL.

    if (Is_Bedrock_Dual_An_Accessor(dual_spare)) {  // FRAME!
        Api(Value*) result = rebLift(dual_spare);
        Copy_Cell(dual_spare, As_Dual(result));  // result of running FRAME!
        rebRelease(result);
        goto possibly_unbind_spare_and_return;
    }

    if (Is_Bedrock_Dual_An_Alias(dual_spare)) {  // ^WORD!, ^TUPLE!
        Quote_Cell(dual_spare);
        Api(Value*) result = rebLift(CANON(GET), dual_spare);
        Copy_Cell(dual_spare, As_Dual(result));  // lifted result of GET
        rebRelease(result);
        goto possibly_unbind_spare_and_return;
    }

    if (Is_Bedrock_Dual_A_Drain(dual_spare)) {  // SPACE
        Quasify_Isotopic_Fundamental(  // signify lifted FAILURE! [1]
            Init_Context_Cell(
                dual_spare, TYPE_ERROR, Error_Cant_Get_Drain_Raw()
            )
        );
        goto return_without_unbinding;
    }

    if (Is_Bedrock_Dual_A_Hole(dual_spare)) {  // unspecialized cell
        if (adjusted == TYPE_FRAME) { // picking parameter from an ACTION!
            LIFT_BYTE(dual_spare) = ONEQUOTE_NONQUASI_5;  // plain lifted
        } else {  // make it look like a NULL
            Init_Lifted_Null_Signifying_Unspecialized(dual_spare);
        }
        goto return_without_unbinding;
    }

    return Error_User("TWEAK* returned unknown dualized bedrock element");

} possibly_unbind_spare_and_return: { ///////////////////////////////////////

    if (Get_Cell_Flag(
        Data_Stack_At(Element, picker_index), STEP_NOTE_WANTS_UNBIND
    )){
        Unbind_Cell_If_Bindable_Core(dual_spare);  // unbind after reading
    }

}} return_without_unbinding: { ////////////////////////////////////////////////

    return SUCCESS;
}}


Option(Error*) Trap_Tweak_Spare_Is_Dual_To_Top_Put_Writeback_Dual_In_Spare(
    Level* parent,
    Level* const sub,
    StackIndex picker_index
){
    if (Is_Lifted_Antiform(Level_Spare(parent)))
        return Error_User("TWEAK* cannot be used on antiforms");

    Element* scratch_var = As_Element(Level_Scratch(parent));

    require (
      Push_Action(sub, LIB(TWEAK_P), PREFIX_0)
    );

    Element* location_arg;
    Stable* picker_arg;
    Value* value_arg;

  proxy_arguments_to_frame_dont_panic_in_this_scope: {

    Dual* dual_location_spare = As_Dual(Level_Spare(parent));  // incoming

  // We can't panic while there's an extant level that's not pushed.
  //
  // (See notes in Trap_Call_Pick_Refresh_Dual_In_Spare() for more details.)
  //
  // 1. GET:STEPS returns @var for steps of var.  But is (get @var) same as
  //    (get $var) ?

    USE_LEVEL_SHORTHANDS (sub);
    INCLUDE_PARAMS_OF_TWEAK_P;

    assert(Is_Possibly_Unstable_Value_Quoted(dual_location_spare));
    location_arg = Copy_Cell(
        Erase_ARG(LOCATION),
        dual_location_spare
    );
    Unquote_Cell(location_arg);

    picker_arg = Copy_Cell(
        Erase_ARG(PICKER),
        Data_Stack_At(Element, picker_index)
    );

    value_arg = u_cast(Value*, Erase_ARG(DUAL));

} erase_parent_spare_now_that_we_are_done_extracting_it: {

    assert(sub->out == Level_Spare(parent));
    Erase_Cell(sub->out);

} adjust_frame_arguments_now_that_its_safe_to_panic: {

    attempt {  // v-- how to handle cases like ^x.(...) and know it's ^META?
        if (Any_Lifted(picker_arg)) {  // literal x.'y or x.('y) => 'y
            Known_Stable_Unlift_Cell(picker_arg);

            if (Is_Logic(picker_arg)) {
                Drop_Action(sub);
                return Error_User(
                    "PICK with logic picker never allowed"
                );
            }

            if (Is_Any_Lifted_Void(TOP_ELEMENT)) // don't know if was ^META :-(
                break;  // remove signal

            if (Is_Quoted(TOP_ELEMENT))
                break;

            if (Is_Lifted_Non_Meta_Assignable_Unstable_Antiform(TOP_ELEMENT))
                break;

            Value* sub_spare = Copy_Cell(Level_Spare(sub), TOP_ELEMENT);
            require (
              Unlift_Cell_No_Decay(sub_spare)
            );
            Decay_If_Unstable(sub_spare) except (Error* e) {
                Drop_Action(sub);
                return e;
            }
            Copy_Cell(TOP_ELEMENT, Lift_Cell(sub_spare));
            break;
        }

        Element* picker_instruction = As_Element(picker_arg);
        Option(Sigil) picker_sigil = Sigil_Of(picker_instruction);
        UNUSED(picker_sigil);  // ideas on the table for this...

        if (SIGIL_META == Cell_Underlying_Sigil(scratch_var))
            continue;  // don't decay TOP_ELEMENT

        // if not meta, needs to decay if unstable

        if (not Any_Lifted(TOP_ELEMENT))
            continue;  // dual signal, do not lift dual

        if (Is_Lifted_Action(TOP_ELEMENT)) {  // !!! must generalize all sets
            if (Is_Word(picker_arg)) {
                Update_Frame_Cell_Label(  // !!! is this a good idea?
                    TOP_ELEMENT, Word_Symbol(picker_arg)
                );
            }
            continue;
        }

        if (Get_Cell_Flag(scratch_var, SCRATCH_VAR_NOTE_ONLY_ACTION)) {
            Drop_Action(sub);
            return Error_User(
                "/word: and /obj.field: assignments need ACTION!"
            );
        }

        if (Is_Lifted_Non_Meta_Assignable_Unstable_Antiform(TOP_ELEMENT))
            continue;  // (x: ()) or (x: ~) works, (x: ~()~ doesn't)

        bool was_singular_pack = (
            Is_Lifted_Pack(TOP_ELEMENT) and Series_Len_At(TOP_ELEMENT) == 1
        );

        if (was_singular_pack) {  // alias hack: allow (alias: ~(^word)~)
            const Dual* at = u_cast(Dual*, List_Item_At(TOP_ELEMENT));
            if (  // allow only some bedrock forms
                Is_Dual_Alias(at)
                or Is_Dual_Accessor(at)
                or Is_Dual_Drain(at)
            ){
                Copy_Cell(TOP_ELEMENT, at);
                continue;
            }
        }

        Value* sub_spare = Copy_Cell(Level_Spare(sub), TOP_ELEMENT);
        require (
          Unlift_Cell_No_Decay(sub_spare)
        );
        Decay_If_Unstable(sub_spare) except (Error* e) {
            Drop_Action(sub);
            return e;
        };
        Copy_Lifted_Cell(TOP_ELEMENT, sub_spare);
    }
    then {  // not quoted...
        Clear_Cell_Sigil(As_Element(picker_arg));  // drop any sigils
    }

    Copy_Cell(value_arg, TOP_ELEMENT);

    Clear_Cell_Flag(
        scratch_var, SCRATCH_VAR_NOTE_ONLY_ACTION  // consider *once*
    );

} call_updater: {

  // 1. We return success--in the sense of a non-`fail()` Error* result--even
  //    if the slot was not available.  If we're on the last pick of a tuple,
  //    the pick may not panic.  (Note this is distinct from if the picker was
  //    objectively bad, like using an OBJECT! picker in a BLOCK!)

    if (Get_Cell_Flag(
        Data_Stack_At(Element, picker_index), STEP_NOTE_WANTS_UNBIND
    )){
        Unbind_Cell_If_Bindable_Core(  // unbind before writing
            Level_Spare(parent)
        );
    }

    if (SPORADICALLY(32)) {
        LEVEL_STATE_BYTE(sub) = ST_ACTION_TYPECHECKING;
    } else {
        Mark_Typechecked(u_cast(Param*, location_arg));
        Mark_Typechecked(u_cast(Param*, picker_arg));
        Mark_Typechecked(u_cast(Param*, value_arg));
        Set_Executor_Flag(ACTION, sub, IN_DISPATCH);
    }

    bool threw = Trampoline_With_Top_As_Root_Throws();

    if (threw)  // don't want to return casual error you can TRY from
        return Error_No_Catch_For_Throw(TOP_LEVEL);

    Stable* stable_spare = As_Stable(Level_Spare(parent));
    if (Is_Logic(stable_spare)) {  // "success" even if unavailable [1]
        possibly(Is_Okay_Signifying_No_Writeback(stable_spare));
        possibly(Is_Null_Signifying_Slot_Unavailable(stable_spare));
        goto return_success;
    }

    Dual* dual_spare = As_Dual(Level_Spare(parent));

    if (not Is_Dualized_Bedrock(dual_spare))
        goto return_success;

  handle_indirect_poke: {

  // This means TWEAK* returned OUT_UNLIFTED_DUAL_INDIRECT_POKE, and whatever
  // that output was written to the SPARE of level_.  It means that it didn't
  // write the value, but told us where it wants the value to be written
  // (such as to an aliased variable, or through a SETTER function).
  //
  // We handle it here, and indicate there is no writeback needed.
  //
  // (If someone wanted a writeback *and* to run something like a SETTER from
  // a Bedrock slot, they'd have to handle the SETTER themselves and then
  // return what they wanted to write back.  We provide this general mechanism
  // as a convenience, expecting most POKE targets to want writeback -OR-
  // an indirect poke--but not both at once.)

    if (Is_Bedrock_Dual_An_Accessor(dual_spare)) {  // FRAME!
        Element* quoted = Copy_Cell(Level_Spare(sub), TOP_ELEMENT);
        rebElide(dual_spare, quoted);  // quote suppresses eval
    }
    else if (Is_Bedrock_Dual_An_Alias(dual_spare)) {  // ^WORD!, ^TUPLE!
        Element* quoted = Copy_Lifted_Cell(Level_Spare(sub), TOP_ELEMENT);
        Quote_Cell(dual_spare);
        rebElide(CANON(TWEAK), dual_spare, quoted);  // quote suppresses eval
    }
    else if (Is_Bedrock_Dual_A_Drain(dual_spare)) {  // SPACE
        // do nothing, you wrote to a drain...
    }
    else return Error_User(
        "TWEAK* returned bad dualized bedrock element for writeback"
    );

    Init_Okay_Signifying_No_Writeback(stable_spare);
    goto return_success;

}} return_success: { //////////////////////////////////////////////////////////

    Corrupt_Cell_If_Needful(TOP);  // shouldn't use past this point
    return SUCCESS;
}}


//
//  Trap_Push_Steps_To_Stack: C
//
Option(Error*) Trap_Push_Steps_To_Stack(
    Level* level_,  // OUT may be FAILURE! antiform, see [A]
    bool groups_ok
){
    StackIndex base = TOP_INDEX;

    Element* scratch_var = As_Element(SCRATCH);

    Option(Error*) error = SUCCESS;

    if (Is_Word(scratch_var) or Is_Meta_Form_Of(WORD, scratch_var))
        goto handle_scratch_var_as_wordlike;

    if (Is_Tuple(scratch_var) or Is_Meta_Form_Of(TUPLE, scratch_var))
        goto handle_scratch_var_as_sequence;

    if (Is_Tied_Form_Of(BLOCK, scratch_var))
        goto handle_scratch_var_as_tied_steps_block;

    error = Error_Bad_Value(scratch_var);
    goto return_error;

  handle_scratch_var_as_wordlike: {

    if (not Try_Get_Binding_Of(SPARE, scratch_var)) {
        error = Error_No_Binding_Raw(scratch_var);
        goto return_error;
    }

    Copy_Cell(PUSH(), As_Element(SPARE));
    Lift_Cell(TOP_STABLE);  // dual protocol, lift (?)

    Copy_Cell(PUSH(), scratch_var);  // save var for steps + error messages
    switch (opt Cell_Underlying_Sigil(TOP_ELEMENT)) {
      case SIGIL_0:
        break;

      case SIGIL_META:
        TOP->header.bits &= (~ CELL_MASK_SIGIL);
        break;

      case SIGIL_PIN:
      case SIGIL_TIE:
        error = Error_User(
            "PICK instruction only understands ^META sigil, for now..."
        );
        goto return_error;
    }

    unnecessary(Lift_Cell(TOP_STABLE));  // if ^x, not literally ^x ... meta-variable

    goto return_success;

} handle_scratch_var_as_sequence: {

    // If we have a sequence, then GROUP!s must be evaluated.  (If we're given
    // a steps array as input, then a GROUP! is literally meant as a
    // GROUP! by value).  These evaluations should only be allowed if the
    // caller has asked us to return steps.

    if (not Sequence_Has_Pointer(scratch_var)) {  // compressed byte form
        error = Error_Bad_Value(scratch_var);
        goto return_error;
    }

    const Base* payload1 = CELL_PAYLOAD_1(scratch_var);
    if (Is_Base_A_Cell(payload1)) {  // pair optimization
        // pairings considered "Listlike", handled by List_At()
    }
    else switch (Stub_Flavor(cast(Flex*, payload1))) {
      case FLAVOR_SYMBOL: {
        if (Get_Cell_Flag(scratch_var, LEADING_BLANK)) {  // `/a` or `.a`
            if (Heart_Of(scratch_var) != TYPE_TUPLE) {
                error = Error_User("GET leading space only allowed on TUPLE!");
                goto return_error;
            }
            Init_Word(SPARE, CANON(DOT_1));
            Tweak_Cell_Binding(
                u_cast(Element*, SPARE),
                Cell_Binding(scratch_var)
            );
            if (not Try_Get_Binding_Of(PUSH(), u_cast(Element*, SPARE))) {
                DROP();
                error = Error_No_Binding_Raw(As_Element(SPARE));
                goto return_error;
            }
            Lift_Cell(TOP_STABLE);
            Lift_Cell(Init_Word(PUSH(), CANON(DOT_1)));
            Lift_Cell(Init_Word(PUSH(), u_cast(const Symbol*, payload1)));
            goto return_success;
        }

        // `a/` or `a.`
        //
        // !!! If this is a PATH!, it should error if it's not an action...
        // and if it's a TUPLE! it should error if it is an action.  Review.
        //
        goto handle_scratch_var_as_wordlike; }

      case FLAVOR_SOURCE:
        break;  // fall through

      default:
        crash (scratch_var);
    }

    const Element* tail;
    const Element* head = List_At(&tail, scratch_var);
    const Element* at;
    Context* at_binding = Cell_Binding(scratch_var);

    if (Any_Word(head)) {  // add binding at head
        if (not Try_Get_Binding_Of(
            PUSH(), Copy_Cell_May_Bind(SPARE, head, at_binding)
        )){
            error = Error_No_Binding_Raw(As_Element(SPARE));
            goto return_error;
        }

        Lift_Cell(TOP_STABLE);  // dual protocol, lift (?)
    }

    for (at = head; at != tail; ++at) {
        bool unbind;
        switch (LIFT_BYTE(at)) {
          case NOQUOTE_3:
            unbind = false;
            break;

          case ONEQUOTE_NONQUASI_5:
            unbind = true;
            break;

          default:
            panic ("TUPLE! dialect allows single quote 'unbind on items");
        }

        if (Heart_Of(at) == TYPE_GROUP) {
            if (not groups_ok) {
                error = Error_Bad_Get_Group_Raw(scratch_var);
                goto return_error;
            }

            if (Eval_Any_List_At_Throws(SPARE, at, at_binding)) {
                error = Error_No_Catch_For_Throw(TOP_LEVEL);
                goto return_error;
            }

            Stable* spare_picker = (
                Decay_If_Unstable(SPARE)
            ) except (Error* e) {
                error = e;
                goto return_error;
            }

            possibly(Is_Antiform(spare_picker));  // e.g. PICK MAP DATATYPE!
            Copy_Lifted_Cell(PUSH(), spare_picker);  // lift is literal pick
        }
        else {
            Copy_Cell_May_Bind(PUSH(), at, at_binding);
            if (unbind)
                LIFT_BYTE(TOP) = NOQUOTE_3;
        }

        // !!! Here we could validate or rule out items in the TUPLE! dialect,
        // however the work would be repeated in the steps pushing; it is
        // likely better to just let the step processing do it.

        if (unbind)
            Set_Cell_Flag(TOP, STEP_NOTE_WANTS_UNBIND);
    }

    goto return_success;

} handle_scratch_var_as_tied_steps_block: {

    const Element* tail;
    const Element* head = List_At(&tail, scratch_var);
    const Element* at;
    Context* at_binding = Cell_Binding(scratch_var);
    for (at = head; at != tail; ++at)
        Copy_Cell_May_Bind(PUSH(), at, at_binding);

    goto return_success;

} return_success: { //////////////////////////////////////////////////////////

    Corrupt_Cell_If_Needful(SPARE);
    return SUCCESS;

} return_error: { ////////////////////////////////////////////////////////////

    Drop_Data_Stack_To(base);
    return error;
}}


//
//  Trap_Tweak_From_Stack_Steps_With_Dual_Out: C
//
// This is centralized code for setting or "tweaking" variables.
//
// **Almost all parts of the system should go through this code for assignment,
// even when they know they have just a WORD! in their hand and don't need path
// dispatch.**  Only a few places bypass this code for reasons of optimization,
// but they must do so carefully, because that would skip things like
// accessors (which implement type checking, etc.)
//
// 1. The calling function should do `heeded (Corrupt_Cell_If_Needful(SPARE))`.
//    This helps be sure they're not expecting SPARE to be untouched.  (It's
//    better than trying to work "Corrupts_Spare()" into the already quite-long
//    name of the function.)
//
Option(Error*) Trap_Tweak_From_Stack_Steps_With_Dual_Out(
    Level* level_,  // OUT may be FAILURE! antiform, see [A]
    StackIndex base,
    TweakMode mode
){
    assert(OUT != SCRATCH and OUT != SPARE);

    Stable* out = As_Stable(OUT);

    assert(LEVEL == TOP_LEVEL);
    possibly(Get_Cell_Flag(SCRATCH, SCRATCH_VAR_NOTE_ONLY_ACTION));

  #if NEEDFUL_DOES_CORRUPTIONS  // confirm caller pre-corrupted spare [1]
    assert(Not_Cell_Readable(SPARE));
  #endif

    Sink(Stable) spare_location_dual = SPARE;

    StackIndex stackindex_top;

    Option(Error*) error = SUCCESS;  // for common exit path on error

    Element* scratch_var = As_Element(SCRATCH);

  #if RUNTIME_CHECKS
    Protect_Cell(scratch_var);  // (common exit path undoes this protect)
    Protect_Cell(OUT);
  #endif

    // We always poke from the top of the stack, not from OUT.  This is
    // because we may have to decay it, and we don't want to modify OUT.
    // It also simplifies the bookkeeping because we don't have to remember
    // if we're looking to poke from the stack or not.

    stackindex_top = TOP_INDEX;  // capture "top of stack" before push

    Copy_Cell(PUSH(), As_Stable(OUT));

    require (
      Level* sub = Make_End_Level(
        &Action_Executor,
        LEVEL_FLAG_DEBUG_STATE_0_OUT_NOT_ERASED_OK
      )
    );
    dont(Erase_Cell(SPARE));  // spare read before erase
    Push_Level(SPARE, sub);

  poke_again: { //////////////////////////////////////////////////////////////

    StackIndex stackindex = base + 1;

  do_stack_thing: {

    OnStack(Element*) at = Data_Stack_At(Element, stackindex);
    Copy_Cell(spare_location_dual, at);  // dual protocol, leave lifted
    if (not Any_Lifted(spare_location_dual)) {
        panic ("First Element in STEPS must be lifted");
    }

    ++stackindex;

} calculate_pick_stack_limit: {

    StackIndex limit = stackindex_top;
    if (Is_Null_Signifying_Tweak_Is_Pick(out))
        limit = stackindex_top + 1;

    if (stackindex == limit)
        goto check_for_updater;

  keep_picking_until_last_step: {

    for (; stackindex != limit; ++stackindex, Restart_Action_Level(sub)) {
        bool dont_indirect = (
            (mode == ST_TWEAK_TWEAKING) and (stackindex == limit - 1)
        );
        error = Trap_Call_Pick_Refresh_Dual_In_Spare(
            level_, sub, stackindex, dont_indirect
        );
        if (error)
            panic (unwrap error);

        if (Is_Null_Signifying_Slot_Unavailable(As_Stable(SPARE))) {
          treat_like_pick_absent_signal:
            Copy_Cell(SPARE, Data_Stack_At(Element, stackindex));
            error = Error_Bad_Pick_Raw(As_Element(SPARE));
            if (
                stackindex == limit - 1
                and not Is_Metaform(Data_Stack_At(Element, stackindex))
            ){
                goto return_error;  // last step can be tolerant, see [A]
            }
            panic (unwrap error);
        }

        if (dont_indirect) {
            possibly(LIFT_BYTE(SPARE) == NOQUOTE_3);  // bedrock introspect
            continue;
        }

        assert(Any_Lifted(SPARE));  // successful pick

        if (Is_Metaform(scratch_var))
            continue;  // all meta picks are as-is

        if (Is_Lifted_Unstable_Antiform(SPARE)) {
            if (
                Is_Lifted_Action(As_Stable(SPARE))  // e.g. asking APPEND.DUP
                and stackindex != limit - 1
            ){
                continue;  // allow it if NOT last step (picks PARAMETER!)
            }
            if (
                Is_Lifted_Hot_Potato(As_Stable(SPARE))
                and stackindex == limit - 1
            ){
                continue;  // last non-meta pick can be unstable if hot-potato
            }
            if (Is_Lifted_Void(As_Stable(SPARE))) {
                goto treat_like_pick_absent_signal;  // like before void pick
            }
            error = Error_Unstable_Non_Meta_Raw(
                Data_Stack_At(Element, stackindex)
            );
            goto return_error;
        }

        continue;  // if not last pick in tuple, pick again from this product
    }

}} check_for_updater: {

    if (
        not Is_Metaform(scratch_var)
        and Is_Lifted_Antiform(spare_location_dual)
        and Not_Stable_Antiform_Heart(
            Heart_Of_Unsigiled_Isotopic(spare_location_dual)
        )
        and not Is_Lifted_Hot_Potato(spare_location_dual)  // allow e.g. VETO
        and not Is_Lifted_Action(spare_location_dual) // temporarily allow
        // allow VOID!, also?
    ){
        panic ("PICK result cannot be unstable unless metaform");
    }

    // 1. SPARE was picked via dual protocol.  At the moment of the PICK,
    //    the picker may have been ^META, in which case we wouldn't want to
    //    decay... but otherwise we would.  But that decay was already done
    //    (it just re-lifted it) so the undecayed won't make an unstable
    //    value here if the picker wasn't ^META.

    if (Is_Null_Signifying_Tweak_Is_Pick(out)) {
        assert(Is_Null(TOP_STABLE));
      #if RUNTIME_CHECKS
        Unprotect_Cell(OUT);
      #endif
        Copy_Cell(OUT, spare_location_dual);
        goto return_success;
    }

    // This may be the first time we do an update, or it may be a writeback
    // as we go back through the list of steps to update any bits that are
    // required to update in the referencing cells.

    error = Trap_Tweak_Spare_Is_Dual_To_Top_Put_Writeback_Dual_In_Spare(
        level_,
        sub,
        stackindex  // picker_index
    );

    if (error)
        goto return_error;

    Stable* spare_writeback_dual = As_Stable(SPARE);

    // Subsequent updates become pokes, regardless of initial updater function

    if (Is_Okay_Signifying_No_Writeback(spare_writeback_dual)) {
      #if RUNTIME_CHECKS
        Unprotect_Cell(OUT);
      #endif
        goto return_success;
    }

    if (Is_Null_Signifying_Slot_Unavailable(spare_writeback_dual)) {
        error = Error_Bad_Pick_Raw(Data_Stack_At(Element, stackindex));
        if (
            mode != ST_TWEAK_TWEAKING
            and Is_Metaform(scratch_var)  // would conflate, panic vs. error
        ){
            panic (unwrap error);
        }
        goto return_error;
    }

    if (stackindex_top == base + 1) {
        panic (
            "Last TWEAK* step in POKE gave non-okay writeback instruction"
        );
    }

    Assert_Cell_Stable(spare_writeback_dual);
    Copy_Cell(Data_Stack_At(Value, TOP_INDEX), spare_writeback_dual);

    --stackindex_top;

    Restart_Action_Level(sub);
    goto poke_again;

}} return_error: { ///////////////////////////////////////////////////////////

    assert(error);
  #if RUNTIME_CHECKS
    Unprotect_Cell(OUT);
    if (Is_Null_Signifying_Tweak_Is_Pick(OUT))
        Corrupt_Cell_If_Needful(OUT);  // so you don't think it picked null
  #endif

    Drop_Level(sub);
    goto finalize_and_return;

} return_success: { //////////////////////////////////////////////////////////

    possibly(Is_Failure(OUT));  // success may be FAILURE! antiform, see [A]

    assert(not error);

    Drop_Level(sub);
    DROP();  // drop pushed cell for decaying OUT/etc.

    goto finalize_and_return;

} finalize_and_return: { /////////////////////////////////////////////////////

    assert(LEVEL == TOP_LEVEL);

    Corrupt_Cell_If_Needful(SPARE);

  #if RUNTIME_CHECKS
    Unprotect_Cell(scratch_var);
    assert(not (OUT->header.bits & CELL_FLAG_PROTECTED));
  #endif

    return error;
}}


//
//  Trap_Tweak_Var_In_Scratch_With_Dual_Out: C
//
Option(Error*) Trap_Tweak_Var_In_Scratch_With_Dual_Out(
    Level* level_,  // OUT may be FAILURE! antiform, see [A]
    Option(Element*) steps_out,  // no GROUP!s if nulled
    TweakMode mode
){
    possibly(SPARE == steps_out or SCRATCH == steps_out);

    assert(STATE != STATE_0);  // trampoline rule: OUT only erased if STATE_0

    possibly(TOP_INDEX != STACK_BASE);
    StackIndex base = TOP_INDEX;

    bool groups_ok = (steps_out != nullptr);
    Option(Error*) e = Trap_Push_Steps_To_Stack(level_, groups_ok);
    if (e)
        return e;

    e = Trap_Tweak_From_Stack_Steps_With_Dual_Out(level_, base, mode);
    if (e) {
        Drop_Data_Stack_To(base);
        return e;
    }

    if (not steps_out or steps_out == GROUPS_OK) {
        Drop_Data_Stack_To(base);
        return SUCCESS;
    }

    if (TOP_INDEX == base + 1 and Is_Word(TOP_ELEMENT)) {
        Copy_Cell(unwrap steps_out, TOP_ELEMENT);
        DROP();
    }
    else
        Init_Block(unwrap steps_out, Pop_Source_From_Stack(base));

    Add_Cell_Sigil(unwrap steps_out, SIGIL_TIE);  // $[bl o ck] or $word steps

    return SUCCESS;
}


//
//  tweak: native [
//
//  "Low-level variable setter, that can assign within the dual band"
//
//      return: [
//          quasiform! quoted!
//          frame! word! ^word! ^tuple! space? parameter!
//          failure!  "Passthru even if it skips the assign"
//      ]
//      target "Word or tuple, or calculated sequence steps (from GET)"
//          [
//              <opt>
//              word! tuple!
//              ^word! ^tuple!
//              $block!
//          ]
//      dual "Ordinary GET or SET with lifted value (unlifts), else dual"
//          [
//              <opt>  "act as a raw GET of the dual state"
//              frame!  "store a GETTER/SETTER function in dual band"
//              word!  "special instructions (e.g. PROTECT, UNPROTECT)"
//              ^word! ^tuple!  "store an alias to another variable"
//              space?  "store a 'drain', which erases all assignments"
//              quasiform! quoted!  "store unlifted values as a normal SET"
//          ]
//      :groups "Allow GROUP! Evaluations"
//      :steps "Return evaluation steps for reproducible access"
//      {dual-refinement-placeholder}  ; GET and SET have :DUAL refinement
//  ]
//
DECLARE_NATIVE(TWEAK)
{
    INCLUDE_PARAMS_OF_TWEAK;
    UNUSED(ARG(STEPS));  // TBD

    Copy_Cell(OUT, LOCAL(DUAL));

    if (not ARG(TARGET))
        return OUT;   // same for SET as [10 = (void): 10]

    Element* target = Element_ARG(TARGET);

    Option(Element*) steps;
    if (ARG(GROUPS))
        steps = GROUPS_OK;
    else
        steps = NO_STEPS;  // no GROUP! evals

    if (STATE == STATE_0)
        STATE = ST_TWEAK_TWEAKING;  // we'll set out to something not erased

    heeded (Copy_Cell(SCRATCH, target));
    heeded (Corrupt_Cell_If_Needful(SPARE));

    Option(Error*) e = Trap_Tweak_Var_In_Scratch_With_Dual_Out(
        LEVEL,
        steps,
        u_cast(TweakMode, STATE)  // should last step of pick not indirect?
    );
    if (e) {
        Init_Context_Cell(OUT, TYPE_ERROR, unwrap e);
        Failify_Cell(OUT);
        return OUT;
    }

  return_value_even_if_we_dont_assign: {

  // We want parity between (set $x expression) and (x: expression).  It's
  // very useful that you can write (e: rescue [x: expression]) and in the case
  // of an error, have the assignment skipped and the error trapped.
  //
  // Note that (set $ '^x fail "hi") will assign the failure! to X, but will
  // still pass through the failure as the overall expression result.

    return OUT;
}}
