//
//  File: %u-parse.c
//  Summary: "parse dialect interpreter"
//  Section: utility
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2021 Ren-C Open Source Contributors
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
//=//// !!! NOTICE !!! /////////////////////////////////////////////////////=//
//
// The PARSE code in R3-Alpha was a fairly organic codebase, and was largely
// concerned with being performant (to make it a viable competitor to things
// like RegEx).  Since it did flag-fiddling in lieu of enforcing a generalized
// architecture, there were significant irregularities...and compositions of
// rules that seemed like they should be legal wouldn't work.  Many situations
// that should have been errors would be ignored or have strange behaviors.
//
// The code was patched to make its workings clearer over time in Ren-C, and
// to try and eliminate mechanical bugs (such as bad interactions with the GC).
// But the basic method was not attacked from the ground up.  Recursions of
// the parser were unified with the level model of recursing the evaluator...
// but that was the only true big change.
//
// However, a full redesign has been started with %src/mezz/uparse.reb.  This
// is in the spirit of "parser combinators" as defined in many other languages,
// but brings in the PARSE dialect's succinct symbolic nature.  That design is
// extremely slow, however--and will need to be merged in with some of the
// ideas in this file.
//
//=/////////////////////////////////////////////////////////////////////////=//
//
// As a major operational difference from R3-Alpha, each recursion in Ren-C's
// PARSE runs using a "Rebol Stack Level"--similar to how the DO evaluator
// works.  So `[print "abc"]` and `[thru "abc"]` are both seen as "code" and
// iterated using the same mechanic.  (The rules are also locked from
// modification during the course of the PARSE, as code is in Ren-C.)
//
// This leverages common services like reporting the start of the last
// "expression" that caused an error.  So merely calling `fail()` will use
// the call stack to properly indicate the start of the parse rule that caused
// a problem.  But most importantly, debuggers can break in and see the
// state at every step in the parse rule recursions.
//
// The function users see on the stack for each recursion is a native called
// SUBPARSE.  Although it is shaped similarly to typical DO code, there are
// differences.  The subparse advances the "current evaluation position" in
// the frame as it operates, so it is a variadic function...with the rules as
// the variadic parameter.  Calling it directly looks a bit unusual:
//
//     >> flags: 0
//     >> subparse "aabb" flags some "a" some "b"
//     == 4
//
// But as far as a debugging tool is concerned, the "where" of each frame
// in the call stack is what you would expect.
//

#include "sys-core.h"


// !!! R3-Alpha would frequently conflate indexes and flags, which could be
// confusing in the evaluator and led to many THROWN values being overlooked.
// To deal with this, a REBIXO datatype (Index-OR-a-flag) was introduced.  It
// helped transition the system to its current mechanism where there is no
// THROWN type indicator--rather a _Throws() boolean-return convention that
// chains through the stack.  PARSE is left as the only user of the datatype,
// and should also be converted to the cleaner convention.
//
#define REBIXO REBLEN
#define THROWN_FLAG ((REBLEN)(-1))
#define END_FLAG ((REBLEN)(-2))


//
// These macros are used to address into the frame directly to get the
// current parse rule, current input series, current parse position in that
// input series, etc.  Because the cell bits of the frame arguments are
// modified as the parse runs, that means users can see the effects at
// a breakpoint.
//
// (Note: when arguments to natives are viewed under the debugger, the
// debug frames are read only.  So it's not possible for the user to change
// the Any_Series! of the current parse position sitting in slot 0 into
// a DECIMAL! and crash the parse, for instance.  They are able to change
// usermode authored function arguments only.)
//

// The compiler typically warns us about not using all the arguments to
// a native at some point.  Service routines may use only some of the values
// in the parse frame, so defeat that check.
//
#define USE_PARAMS_OF_SUBPARSE \
    INCLUDE_PARAMS_OF_SUBPARSE; \
    USED(ARG(input)); \
    USED(ARG(flags)); \
    USED(ARG(collection)); \
    USED(ARG(num_quotes)); \
    USED(ARG(position)); \
    USED(ARG(save))

#define P_AT_END            Is_Level_At_End(level_)
#define P_RULE              At_Level(level_)  // rvalue
#define P_RULE_SPECIFIER    Level_Specifier(level_)

#define P_TYPE              VAL_TYPE(ARG(input))
#define P_INPUT             Cell_Series(ARG(input))
#define P_INPUT_BINARY      Cell_Binary(ARG(input))
#define P_INPUT_STRING      Cell_String(ARG(input))
#define P_INPUT_ARRAY       Cell_Array(ARG(input))
#define P_INPUT_SPECIFIER   Cell_Specifier(ARG(input))
#define P_INPUT_IDX         VAL_INDEX_UNBOUNDED(ARG(input))
#define P_INPUT_LEN         Cell_Series_Len_Head(ARG(input))

#define P_FLAGS             mutable_VAL_INT64(ARG(flags))

#define P_COLLECTION \
    (Is_Nulled(ARG(collection)) \
        ? nullptr \
        : Cell_Array_Known_Mutable(ARG(collection)) \
    )

#define P_NUM_QUOTES        VAL_INT32(ARG(num_quotes))

#define P_POS               VAL_INDEX_UNBOUNDED(ARG(position))

// !!! The way that PARSE works, it will sometimes run the thing it finds
// in the array...but if it's a WORD! or PATH! it will look it up and run
// the result.  When it's in the array, the specifier for that array needs
// to be applied to it.  But when the value has been fetched, that specifier
// shouldn't be used again...because virtual binding isn't supposed to
// carry through references.  The hack to get virtual binding running is to
// always put the fetched rule in the same place...and then the specifier
// is only used when the rule *isn't* in that cell.
//
#define P_SAVE              ARG(save)
#define rule_specifier() \
    (rule == ARG(save) ? SPECIFIED : P_RULE_SPECIFIER)


// !!! R3-Alpha's PARSE code long predated frames, and was retrofitted to use
// them as an experiment in Ren-C.  If it followed the rules of frames, then
// what is seen in a lookback is only good for *one* unit of time and may be
// invalid after that.  It takes several observations and goes back expecting
// a word to be in the same condition, so it can't use opt_lookback yet.
//
// (The evaluator pushes SET-WORD!s and SET-PATH!s to the stack in order to
// be able to reuse the frame and avoid a recursion.  This would have to do
// that as well.)
//
#define FETCH_NEXT_RULE_KEEP_LAST(opt_lookback,L) \
    *opt_lookback = P_RULE; \
    Fetch_Next_Forget_Lookback(L)

#define FETCH_NEXT_RULE(L) \
    Fetch_Next_Forget_Lookback(L)


#define FETCH_TO_BAR_OR_END(L) \
    while (not P_AT_END and not ( \
        VAL_TYPE_UNCHECKED(P_RULE) == REB_WORD \
        and Cell_Word_Symbol(P_RULE) == Canon(BAR_1) \
    )){ \
        FETCH_NEXT_RULE(L); \
    }


// See the notes on `flags` in the main parse loop for how these work.
//
enum parse_flags {
    //
    // In R3-Alpha, the "parse->flags" (persistent across an iteration) were
    // distinct from the "flags" (per recursion, zeroed on each loop).  The
    // former had undocumented overlap with the values of AM_FIND_XXX flags.
    //
    // They are unified in Ren-C, with the overlap asserted.
    //
    PF_0 = 1 << 0,
    PF_FIND_CASE = 1 << 1,
    PF_FIND_MATCH = 1 << 2,

    PF_SET = 1 << 3,
    PF_COPY = 1 << 4,
    PF_NOT = 1 << 5,
    PF_NOT2 = 1 << 6,  // #1246
    PF_7 = 1 << 7,
    PF_AHEAD = 1 << 8,
    PF_REMOVE = 1 << 9,
    PF_INSERT = 1 << 10,
    PF_CHANGE = 1 << 11,
    PF_LOOPING = 1 << 12,
    PF_FURTHER = 1 << 13,  // must advance parse input to count as a match
    PF_TRY = 1 << 14,  // want NULL (not no-op) if no matches

    PF_ONE_RULE = 1 << 15,  // signal to only run one step of the parse

    PF_REDBOL = 1 << 16,  // use Rebol2/Red-style rules

    PF_MAX = PF_REDBOL
};

STATIC_ASSERT(PF_MAX <= INT32_MAX);  // needs to fit in VAL_INTEGER()

// Note: clang complains if `cast(int, ...)` used here, though gcc doesn't
STATIC_ASSERT((int)AM_FIND_CASE == (int)PF_FIND_CASE);
STATIC_ASSERT((int)AM_FIND_MATCH == (int)PF_FIND_MATCH);

#define PF_FIND_MASK \
    (PF_FIND_CASE | PF_FIND_MATCH)

#define PF_STATE_MASK (~PF_FIND_MASK & ~PF_ONE_RULE & ~PF_REDBOL)


// In %words.r, the parse words are lined up in order so they can be quickly
// filtered, skipping the need for a switch statement if something is not
// a parse command.
//
// !!! This and other efficiency tricks from R3-Alpha should be reviewed to
// see if they're really the best option.
//
inline static Option(SymId) VAL_CMD(const Cell* v) {
    Option(SymId) sym = Cell_Word_Id(v);
    if (sym >= SYM_SET and sym <= SYM_END)
        return sym;
    return SYM_0;
}


// Subparse_Throws() is a helper that sets up a call frame and invokes the
// SUBPARSE native--which represents one level of PARSE recursion.
//
// !!! It is the intent of Ren-C that calling functions be light and fast
// enough through Do_Va() and other mechanisms that a custom frame constructor
// like this one would not be needed.  Data should be gathered on how true
// it's possible to make that.
//
// !!! Calling subparse creates another recursion.  This recursion means
// that there are new arguments and a new frame spare cell.  Callers do not
// evaluate directly into their output slot at this time (except the top
// level parse), because most of them are framed to return other values.
//
static bool Subparse_Throws(
    bool *interrupted_out,
    Sink(Value(*)) out,
    const Cell* input,
    Specifier* input_specifier,
    Level* const L,
    Option(Array*) collection,
    Flags flags
){
    assert(Any_Series_Kind(Cell_Heart(input)));

    Push_Level(out, L);  // checks for C stack overflow
    assert(Is_Fresh(out));

    Push_Action(L, VAL_ACTION(Lib(SUBPARSE)), UNBOUND);

    Begin_Prefix_Action(L, Canon(SUBPARSE));

    // This needs to be set before INCLUDE_PARAMS_OF_SUBPARSE; it is what
    // ensures that usermode accesses to the frame won't be able to fiddle
    // the frame values to bit patterns the native might crash on.
    //
    Set_Series_Info(L->varlist, HOLD);

    USE_LEVEL_SHORTHANDS (L);
    INCLUDE_PARAMS_OF_SUBPARSE;

    Init_Nulled(Erase_Cell(ARG(return)));

    Derelativize(Erase_Cell(ARG(input)), input, input_specifier);

    assert((flags & PF_STATE_MASK) == 0);  // no "parse state" flags allowed
    Init_Integer(Erase_Cell(ARG(flags)), flags);

    // If there's an array for collecting into, there has to be some way of
    // passing it between frames.
    //
    REBLEN collect_tail;
    if (collection) {
        Init_Block(Erase_Cell(ARG(collection)), unwrap(collection));
        collect_tail = Array_Len(unwrap(collection));  // rollback here on fail
    }
    else {
        Init_Nulled(Erase_Cell(ARG(collection)));
        collect_tail = 0;
    }

    // Locals in frame would be unset on entry if called by action dispatch.
    //
    Init_Trash(Erase_Cell(ARG(num_quotes)));
    Init_Trash(Erase_Cell(ARG(position)));
    Init_Trash(Erase_Cell(ARG(save)));

    // !!! By calling the subparse native here directly from its C function
    // vs. going through the evaluator, we don't get the opportunity to do
    // things like HIJACK it.  Consider APPLY-ing it.
    //
    Set_Executor_Flag(ACTION, L, IN_DISPATCH);

    Bounce b = N_subparse(L);

    Drop_Action(L);

    if ((b == BOUNCE_THROWN or Is_Nulled(out)) and collection)
        Set_Series_Len(unwrap(collection), collect_tail);  // abort rollback

    if (b == BOUNCE_THROWN) {
        Drop_Level(L);

        // ACCEPT and REJECT are special cases that can happen at nested parse
        // levels and bubble up through the throw mechanism to break a looping
        // construct.
        //
        // !!! R3-Alpha didn't react to these instructions in general, only in
        // the particular case where subparsing was called in an iterated
        // construct.  Even then, it could only break through one level of
        // depth.  Most places would treat them the same as a normal match
        // or not found.  This returns the interrupted flag which is still
        // ignored by most callers, but makes that fact more apparent.
        //
        const REBVAL *label = VAL_THROWN_LABEL(LEVEL);
        if (Is_Frame(label)) {
            if (VAL_ACTION(label) == VAL_ACTION(Lib(PARSE_REJECT))) {
                CATCH_THROWN(out, LEVEL);
                *interrupted_out = true;
                return false;
            }

            if (VAL_ACTION(label) == VAL_ACTION(Lib(PARSE_BREAK))) {
                CATCH_THROWN(out, LEVEL);
                assert(Is_Integer(out));
                *interrupted_out = true;
                return false;
            }
        }

        return true;
    }

    Drop_Level(L);

    assert(b == out);

    *interrupted_out = false;
    return false;
}


// Very generic errors.  Used to be parameterized with the parse rule in
// question, but now the `where` at the time of failure will indicate the
// location in the parse dialect that's the problem.

inline static Context* Error_Parse_Rule(void) {
    return Error_Parse_Rule_Raw();
}

inline static Context* Error_Parse_End(void) {
    return Error_Parse_End_Raw();
}

inline static Context* Error_Parse_Command(Level* level_) {
    return Error_Parse_Command_Raw(P_RULE);
}

inline static Context* Error_Parse_Variable(Level* level_) {
    return Error_Parse_Variable_Raw(P_RULE);
}


static void Print_Parse_Index(Level* level_) {
    USE_PARAMS_OF_SUBPARSE;

    DECLARE_LOCAL (input);
    Init_Series_Cell_At_Core(
        input,
        P_TYPE,
        P_INPUT,
        P_POS,
        Is_Series_Array(P_INPUT)
            ? P_INPUT_SPECIFIER
            : SPECIFIED
    );

    // Either the rules or the data could be positioned at the end.  The
    // data might even be past the end.
    //
    // !!! Or does PARSE adjust to ensure it never is past the end, e.g.
    // when seeking a position given in a variable or modifying?
    //
    if (P_AT_END) {
        if (P_POS >= cast(REBIDX, P_INPUT_LEN))
            rebElide("print {[]: ** END **}");
        else
            rebElide("print [{[]:} mold", input, "]");
    }
    else {
        DECLARE_LOCAL (rule);
        Derelativize(rule, P_RULE, P_RULE_SPECIFIER);

        if (P_POS >= cast(REBIDX, P_INPUT_LEN))
            rebElide("print [mold", rule, "{** END **}]");
        else {
            rebElide("print ["
                "mold", rule, "{:} mold", input,
            "]");
        }
    }
}


//
//  Get_Parse_Value: C
//
// Gets the value of a word (when not a command) or path.  Returns all other
// values as-is.
//
static const Cell* Get_Parse_Value(
    Sink(Value(*)) cell,  // storage for fetched values; must be GC protected
    const Cell* rule,
    Specifier* specifier
){
    if (Is_Word(rule)) {
        if (VAL_CMD(rule))  // includes IS_BAR()...also a "command"
            return rule;

        Get_Word_May_Fail(cell, rule, specifier);
    }
    else if (Is_Tuple(rule)) {
        Get_Var_May_Fail(cell, rule, specifier, false);
    }
    else
        return rule;

    if (Is_Nulled(cell))
        fail (Error_Bad_Null(rule));

    if (Is_Integer(cell))
        fail ("Use REPEAT on integers https://forum.rebol.info/t/1578/6");

    return cell;
}

//
//  Process_Group_For_Parse: C
//
// Historically a single group in PARSE ran code, discarding the value (with
// a few exceptions when appearing in an argument position to a rule).  Ren-C
// adds another behavior for GET-GROUP!, e.g. :(...).  This makes them act
// like a COMPOSE that runs each time they are visited.
//
bool Process_Group_For_Parse_Throws(
    Sink(Value(*)) out,
    Level* level_,
    const Cell* group  // may be same as `cell`
){
    USE_PARAMS_OF_SUBPARSE;

    assert(cast(Cell*, out) != group);

    assert(Is_Group(group) or Is_Get_Group(group));
    Specifier* derived = (group == P_SAVE)
        ? SPECIFIED
        : Derive_Specifier(P_RULE_SPECIFIER, group);

  blockscope {
    Atom(*) atom_out = out;
    if (Do_Any_Array_At_Throws(atom_out, group, derived))
        return true;
    Decay_If_Unstable(atom_out);
  }

    // !!! The input is not locked from modification by agents other than the
    // PARSE's own REMOVE/etc.  This is a sketchy idea, but as long as it's
    // allowed, each time arbitrary user code runs, rules have to be adjusted
    //
    if (P_POS > cast(REBIDX, P_INPUT_LEN))
        P_POS = P_INPUT_LEN;

    return false;
}


//
//  Parse_One_Rule: C
//
// Used for parsing ANY-SERIES! to match the next rule in the ruleset.  If it
// matches, return the index just past it.
//
// This function is also called by To_Thru, consequently it may need to
// process elements other than the current one in the frame.  Hence it
// is parameterized by an arbitrary `pos` instead of assuming the P_POS
// that is held by the frame.
//
// The return result is either an int position, END_FLAG, or THROWN_FLAG
// Only in the case of THROWN_FLAG will L->out (aka OUT) be affected.
// Otherwise, it should exit the routine as an END marker (as it started);
//
static REBIXO Parse_One_Rule(
    Level* level_,
    REBLEN pos,
    const Cell* rule
){
    USE_PARAMS_OF_SUBPARSE;

    assert(Is_Fresh(OUT));

    if (Is_Group(rule) or Is_Get_Group(rule)) {
        bool inject = Is_Get_Group(rule);  // rule may be SPARE
        if (Process_Group_For_Parse_Throws(SPARE, level_, rule))
            return THROWN_FLAG;

        if (not inject or Is_Void(SPARE)) {  // !!! Should this be legal?
            assert(pos <= P_INPUT_LEN);  // !!! Process_Group ensures
            return pos;
        }
        // was a GET-GROUP! :(...), use result as rule
        rule = SPARE;
    }

    if (pos == P_INPUT_LEN) {  // at end of input
        if (Is_Logic(rule) or Is_Block(rule) or Is_Void(rule)) {
            //
            // Only these types can *potentially* handle an END input.
        }
        else if (
            (Is_Text(rule) or Is_Binary(rule))
            and (Cell_Series_Len_At(rule) == 0)
            and (Any_String_Kind(P_TYPE) or P_TYPE == REB_BINARY)
        ){
            // !!! The way this old R3-Alpha code was structured is now very
            // archaic (compared to UPARSE).  But while that design stabilizes,
            // this patch handles the explicit case of wanting to match
            // something like:
            //
            //     >> did parse3 "ab" [thru ["ab"] ""]
            //     == ~true~  ; anti
            //
            // Just to show what should happen in the new model (R3-Alpha did
            // not have that working for multiple reasons...lack of making
            // progress in the "" rule, for one.)
            //
            return pos;
        }
        else
            return END_FLAG;  // Other cases below assert if item is END
    }

    if (Is_Void(rule)) {
        return pos;  // void matches always
    }
    else if (Is_Logic(rule)) {
        if (Cell_Logic(rule))
            return pos;  // true matches always
        return END_FLAG;  // false matches never
    }
    else switch (VAL_TYPE(rule)) {  // handle w/same behavior for all P_INPUT
      case REB_INTEGER:
        fail ("Non-rule-count INTEGER! in PARSE must be literal, use QUOTE");

      case REB_BLOCK: {
        //
        // Process subrule in its own frame.  It will not change P_POS
        // directly (it will have its own P_POSITION_VALUE).  Hence the return
        // value regarding whether a match occurred or not has to be based on
        // the result that comes back in OUT.

        REBLEN pos_before = P_POS;
        P_POS = pos;  // modify input position

        Level* sub = Make_Level_At_Core(
            rule, rule_specifier(),
            LEVEL_MASK_NONE
        );

        DECLARE_LOCAL (subresult);
        bool interrupted;
        if (Subparse_Throws(
            &interrupted,
            FRESHEN(subresult),
            ARG(position),  // affected by P_POS assignment above
            SPECIFIED,
            sub,
            P_COLLECTION,
            (P_FLAGS & PF_FIND_MASK)
                | (P_FLAGS & PF_REDBOL)
        )){
            return THROWN_FLAG;
        }

        UNUSED(interrupted);  // !!! ignore "interrupted" (ACCEPT or REJECT?)

        P_POS = pos_before;  // restore input position

        if (Is_Nulled(subresult))
            return END_FLAG;

        REBINT index = VAL_INT32(subresult);
        assert(index >= 0);
        return index; }

      default:;
        // Other cases handled distinctly between blocks/strings/binaries...
    }

    if (Is_Series_Array(P_INPUT)) {
        const Cell* item = Array_At(P_INPUT_ARRAY, pos);

        switch (VAL_TYPE(rule)) {
          case REB_QUOTED:
            Copy_Cell(SPARE, rule);
            rule = Unquotify(SPARE, 1);
            break;  // fall through to direct match

          case REB_THE_WORD: {
            bool any = false;
            Get_Var_May_Fail(SPARE, rule, P_RULE_SPECIFIER, any);
            rule = SPARE;
            break; }  // all through to direct match

          case REB_TYPE_WORD:
          case REB_TYPE_BLOCK:
          case REB_TYPE_GROUP:
          case REB_PARAMETER: {
            if (Typecheck_Atom_Core(rule, P_RULE_SPECIFIER, cast(Atom(const*), item)))
                return pos + 1;  // type was in typeset
            return END_FLAG; }

          default:
            break;
        }

        // !!! R3-Alpha said "Match with some other value"... is this a good
        // default?!
        //
        if (Cmp_Value(item, rule, did (P_FLAGS & AM_FIND_CASE)) == 0)
            return pos + 1;

        return END_FLAG;
    }
    else {
        assert(Any_String_Kind(P_TYPE) or P_TYPE == REB_BINARY);

        if (Is_The_Word(rule)) {
            bool any = false;
            Get_Var_May_Fail(SPARE, rule, P_RULE_SPECIFIER, any);
            rule = SPARE;
        }

        // We try to allow some conveniences when parsing strings based on
        // how items render, e.g.:
        //
        //     >> parse? "ab<c>10" ['ab <c> '10]
        //     == ~true~  ; anti
        //
        // It can be less visually noisy than:
        //
        //     >> parse? "ab<c>10" ["ab" {<c>} "10"]
        //     == ~true~  ; anti
        //
        // !!! The concept is based somewhat on what was legal in FIND for
        // Rebol2, and leverages quoting.  It's being experimented with.
        //
        enum Reb_Kind rule_heart = Cell_Heart(rule);
        if (
            (Any_Word_Kind(rule_heart) and Cell_Num_Quotes(rule) == 1)
            or (Any_String_Kind(rule_heart) and Cell_Num_Quotes(rule) <= 1)
            or (rule_heart == REB_ISSUE and Cell_Num_Quotes(rule) <= 1)
            or (rule_heart == REB_BINARY and Cell_Num_Quotes(rule) == 0)
            or (rule_heart == REB_INTEGER and Cell_Num_Quotes(rule) == 1)
        ){
            REBLEN len;
            REBINT index = Find_Value_In_Binstr(
                &len,
                ARG(position),
                Cell_Series_Len_Head(ARG(position)),
                rule,
                (P_FLAGS & PF_FIND_MASK) | AM_FIND_MATCH
                    | (Is_Issue(rule) ? AM_FIND_CASE : 0),
                1  // skip
            );
            if (index == NOT_FOUND)
                return END_FLAG;
            return cast(REBLEN, index) + len;
        }
        else switch (VAL_TYPE(rule)) {
          case REB_BITSET: {
            //
            // Check current char/byte against character set, advance matches
            //
            bool uncased;
            Codepoint uni;
            if (P_TYPE == REB_BINARY) {
                uni = *Binary_At(P_INPUT_BINARY, P_POS);
                uncased = false;
            }
            else {
                uni = Get_Char_At(c_cast(String*, P_INPUT), P_POS);
                uncased = not (P_FLAGS & AM_FIND_CASE);
            }

            if (Check_Bit(VAL_BITSET(rule), uni, uncased))
                return P_POS + 1;

            return END_FLAG; }

          default:
            fail (Error_Parse_Rule());
        }
    }
}


//
//  To_Thru_Block_Rule: C
//
// The TO and THRU keywords in PARSE do not necessarily match the direct next
// item, but scan ahead in the series.  This scan may be successful or not,
// and how much the match consumes can vary depending on how much THRU
// content was expressed in the rule.
//
// !!! This routine from R3-Alpha is fairly circuitous.  As with the rest of
// the code, it gets clarified in small steps.
//
static REBIXO To_Thru_Block_Rule(
    Level* level_,
    const Cell* rule_block,
    bool is_thru
){
    USE_PARAMS_OF_SUBPARSE;

    DECLARE_STABLE (cell);  // holds evaluated rules (use frame cell instead?)

    // Note: This enumeration goes through <= P_INPUT_LEN, because the
    // block rule might be something like `to [{a} | end]`.  e.g. being
    // positioned on the end cell or null terminator of a string may match.
    //
    DECLARE_LOCAL (iter);
    Copy_Cell(iter, ARG(position));  // need to slide pos
    for (
        ;
        VAL_INDEX_RAW(iter) <= cast(REBIDX, P_INPUT_LEN);
        ++VAL_INDEX_RAW(iter)
    ){  // see note
        const Cell* blk_tail = Array_Tail(Cell_Array(rule_block));
        const Cell* blk = Array_Head(Cell_Array(rule_block));
        for (; blk != blk_tail; blk++) {
            if (IS_BAR(blk))
                fail (Error_Parse_Rule());  // !!! Shouldn't `TO [|]` succeed?

            const Cell* rule;
            if (not (Is_Group(blk) or Is_Get_Group(blk)))
                rule = blk;
            else {
                bool inject = Is_Get_Group(blk);
                if (Process_Group_For_Parse_Throws(cell, level_, blk))
                    return THROWN_FLAG;

                if (not inject or Is_Void(cell))
                    continue;

                rule = cell;
            }

            if (Is_Word(rule)) {
                Option(SymId) cmd = VAL_CMD(rule);

                if (cmd) {
                    if (cmd == SYM_END) {
                        if (not (P_FLAGS & PF_REDBOL))
                            fail ("Use <end> instead of END outside PARSE2");

                        if (VAL_INDEX(iter) >= P_INPUT_LEN)
                            return P_INPUT_LEN;
                        goto next_alternate_rule;
                    }
                    else if (
                        cmd == SYM_QUOTE // temporarily same for bootstrap
                    ){
                        if (not (P_FLAGS & PF_REDBOL))
                            fail ("Use THE instead of QUOTE outside PARSE2");

                        rule = ++blk;  // next rule is the literal value
                        if (rule == blk_tail)
                            fail (Error_Parse_Rule());
                    }
                    else if (
                        cmd == SYM_THE // temporarily same for bootstrap
                    ){
                        rule = ++blk;  // next rule is the literal value
                        if (rule == blk_tail)
                            fail (Error_Parse_Rule());
                    }
                    else
                        fail (Error_Parse_Rule());
                }
                else {
                    Get_Word_May_Fail(cell, rule, P_RULE_SPECIFIER);
                    rule = cell;
                }
            }
            else if (Is_Tag(rule) and not (P_FLAGS & PF_REDBOL)) {
                bool strict = true;
                if (0 == CT_String(rule, Root_End_Tag, strict)) {
                    if (VAL_INDEX(iter) >= P_INPUT_LEN)
                        return P_INPUT_LEN;
                    goto next_alternate_rule;
                }
                else if (0 == CT_String(rule, Root_Here_Tag, strict)) {
                    // ignore for now
                }
                else
                    fail ("TAG! combinator must be <here> or <end> ATM");
            }
            else if (Is_Tuple(rule))
                rule = Get_Parse_Value(cell, rule, P_RULE_SPECIFIER);
            else if (Is_Path(rule))
                fail ("Use TUPLE! a.b.c instead of PATH! a/b/c");

            // Try to match it:
            if (Any_Array_Kind(P_TYPE) or Any_Sequence_Kind(P_TYPE)) {
                if (Any_Array(rule))
                    fail (Error_Parse_Rule());

                REBIXO ixo = Parse_One_Rule(level_, VAL_INDEX(iter), rule);
                if (ixo == THROWN_FLAG)
                    return THROWN_FLAG;

                if (ixo == END_FLAG) {
                    // fall through, keep looking
                }
                else {  // ixo is pos we matched past, so back up if only TO
                    VAL_INDEX_RAW(iter) = ixo;
                    if (is_thru)
                        return VAL_INDEX(iter);  // don't back up
                    return VAL_INDEX(iter) - 1;  // back up
                }
            }
            else if (P_TYPE == REB_BINARY) {
                Byte ch1 = *Cell_Binary_At(iter);

                if (VAL_INDEX(iter) == P_INPUT_LEN) {
                    //
                    // If we weren't matching END, then the only other thing
                    // we'll match at the BINARY! end is an empty BINARY!.
                    // Not a NUL codepoint, because the internal BINARY!
                    // terminator is implementation detail.
                    //
                    assert(ch1 == '\0');  // internal BINARY! terminator
                    if (Is_Binary(rule) and Cell_Series_Len_At(rule) == 0)
                        return VAL_INDEX(iter);
                }
                else if (IS_CHAR(rule)) {
                    if (Cell_Codepoint(rule) > 0xff)
                        fail (Error_Parse_Rule());

                    if (ch1 == Cell_Codepoint(rule)) {
                        if (is_thru)
                            return VAL_INDEX(iter) + 1;
                        return VAL_INDEX(iter);
                    }
                }
                else if (Is_Binary(rule)) {
                    Size rule_size;
                    const Byte* rule_data = Cell_Binary_Size_At(
                        &rule_size,
                        rule
                    );

                    Size iter_size;
                    const Byte* iter_data = Cell_Binary_Size_At(
                        &iter_size,
                        iter
                    );

                    if (
                        iter_size == rule_size
                        and (0 == memcmp(iter_data, rule_data, iter_size))
                    ){
                        if (is_thru)  // ^-- VAL_XXX_AT checked VAL_INDEX()
                            return VAL_INDEX_RAW(iter) + 1;
                        return VAL_INDEX_RAW(iter);
                    }
                }
                else if (Is_Integer(rule)) {
                    if (VAL_INT64(rule) > 0xff)
                        fail (Error_Parse_Rule());

                    if (ch1 == VAL_INT32(rule)) {
                        if (is_thru)
                            return VAL_INDEX(iter) + 1;
                        return VAL_INDEX(iter);
                    }
                }
                else
                    fail (Error_Parse_Rule());
            }
            else {
                assert(Any_String_Kind(P_TYPE));

                Codepoint unadjusted = Get_Char_At(
                    P_INPUT_STRING,
                    VAL_INDEX(iter)
                );
                if (unadjusted == '\0') {  // cannot be passed to UP_CASE()
                    assert(VAL_INDEX(iter) == P_INPUT_LEN);

                    if (Is_Text(rule) and Cell_Series_Len_At(rule) == 0)
                        return VAL_INDEX(iter);  // empty string can match end

                    goto next_alternate_rule;  // other match is END (above)
                }

                Codepoint ch;
                if (P_FLAGS & AM_FIND_CASE)
                    ch = unadjusted;
                else
                    ch = UP_CASE(unadjusted);

                if (IS_CHAR(rule)) {
                    Codepoint ch2 = Cell_Codepoint(rule);
                    if (ch2 == 0)
                        goto next_alternate_rule;  // no 0 char in ANY-STRING!

                    if (not (P_FLAGS & AM_FIND_CASE))
                        ch2 = UP_CASE(ch2);
                    if (ch == ch2) {
                        if (is_thru)
                            return VAL_INDEX(iter) + 1;
                        return VAL_INDEX(iter);
                    }
                }
                else if (Is_Bitset(rule)) {
                    bool uncased = not (P_FLAGS & AM_FIND_CASE);
                    if (Check_Bit(VAL_BITSET(rule), ch, uncased)) {
                        if (is_thru)
                            return VAL_INDEX(iter) + 1;
                        return VAL_INDEX(iter);
                    }
                }
                else if (Any_String(rule)) {
                    REBLEN len = Cell_Series_Len_At(rule);
                    REBINT i = Find_Value_In_Binstr(
                        &len,
                        iter,
                        Cell_Series_Len_Head(iter),
                        rule,
                        (P_FLAGS & PF_FIND_MASK) | AM_FIND_MATCH,
                        1  // skip
                    );

                    if (i != NOT_FOUND) {
                        if (is_thru)
                            return cast(REBLEN, i) + len;
                        return cast(REBLEN, i);
                    }
                }
                else if (Is_Integer(rule)) {
                    if (unadjusted == cast(Codepoint, VAL_INT32(rule))) {
                        if (is_thru)
                            return VAL_INDEX(iter) + 1;
                        return VAL_INDEX(iter);
                    }
                }
                else
                    fail (Error_Parse_Rule());
            }

          next_alternate_rule:  // alternates are BAR! separated `[a | b | c]`

            do {
                ++blk;
                if (blk == blk_tail)
                    goto next_input_position;
            } while (not IS_BAR(blk));
        }

      next_input_position:;  // not matched yet, keep trying to go THRU or TO
    }
    return END_FLAG;
}


//
//  To_Thru_Non_Block_Rule: C
//
// There's a high-level split between block and non-block rule processing,
// as blocks are the common case.
//
static REBIXO To_Thru_Non_Block_Rule(
    Level* level_,
    const Cell* rule,
    bool is_thru
){
    USE_PARAMS_OF_SUBPARSE;

    if (Is_Logic(rule))  // no-op if true, match failure if false
        return Cell_Logic(rule) ? cast(REBLEN, P_POS) : END_FLAG;

    enum Reb_Kind kind = VAL_TYPE(rule);
    assert(kind != REB_BLOCK);

    // R3-Alpha does not support `TO []` or `THRU []` as a no-op, so we fall
    // back on not recognizing blanks literally in Redbol mode.
    //
    if (kind == REB_BLANK and (P_FLAGS & PF_REDBOL))
        return P_POS;  // make it a no-op

    if (kind == REB_VOID)
        return P_POS;  // make it a no-op

    if (kind == REB_WORD and Cell_Word_Id(rule) == SYM_END) {
        if (not (P_FLAGS & PF_REDBOL))
            fail ("Use <end> instead of END outside PARSE2");

        // `TO/THRU END` JUMPS TO END INPUT SERIES (ANY SERIES TYPE)
        //
        return P_INPUT_LEN;
    }

    if (kind == REB_TAG and not (P_FLAGS & PF_REDBOL)) {
        bool strict = true;
        if (0 == CT_String(rule, Root_End_Tag, strict)) {
            return P_INPUT_LEN;
        }
        else if (0 == CT_String(rule, Root_Here_Tag, strict)) {
            fail ("TO/THRU <here> isn't supported in PARSE3");
        }
        else
            fail ("TAG! combinator must be <here> or <end> ATM");
    }

    if (Is_Series_Array(P_INPUT)) {
        //
        // FOR ARRAY INPUT WITH NON-BLOCK RULES, USE Find_In_Array()
        //
        // !!! This adjusts it to search for non-literal words, but are there
        // other considerations for how non-block rules act with array input?
        //
        Flags find_flags = (P_FLAGS & AM_FIND_CASE);
        DECLARE_STABLE (temp);
        if (Is_Quoted(rule)) {  // make `'[foo bar]` match `[foo bar]`
            Derelativize(temp, rule, P_RULE_SPECIFIER);
            rule = Unquotify(temp, 1);
        }
        else if (Is_The_Word(rule)) {
            bool any = false;
            Get_Var_May_Fail(temp, rule, P_RULE_SPECIFIER, any);
            rule = temp;
        }
        else if (Is_Type_Word(rule) or Is_Type_Group(rule)) {
            Derelativize(temp, rule, P_RULE_SPECIFIER);
            Quasify(temp);
            Meta_Unquotify_Known_Stable(temp);
            rule = temp;
        }

        Length len;
        REBINT i = Find_In_Array(
            &len,
            P_INPUT_ARRAY,
            P_INPUT_SPECIFIER,
            P_POS,
            Array_Len(P_INPUT_ARRAY),
            rule,
            SPECIFIED,  // !!! is it specific?
            find_flags,
            1
        );
        assert(len == 1);

        if (i == NOT_FOUND)
            return END_FLAG;

        if (is_thru)
            return cast(REBLEN, i) + len;

        return cast(REBLEN, i);
    }
    else {
        if (Is_The_Word(rule)) {
            bool any = false;
            Get_Var_May_Fail(SPARE, rule, P_RULE_SPECIFIER, any);
            rule = SPARE;
        }
    }

    //=//// PARSE INPUT IS A STRING OR BINARY, USE A FIND ROUTINE /////////=//

    REBLEN len;  // e.g. if a TAG!, match length includes < and >
    REBINT i = Find_Value_In_Binstr(
        &len,
        ARG(position),
        Cell_Series_Len_Head(ARG(position)),
        rule,
        (P_FLAGS & PF_FIND_MASK),
        1  // skip
    );

    if (i == NOT_FOUND)
        return END_FLAG;

    if (is_thru)
        return i + len;

    return i;
}


// This handles marking positions, either as plain `pos:` the SET-WORD! rule,
// or the newer `mark pos` rule.  Handles WORD! and PATH!.
//
static void Handle_Mark_Rule(
    Level* level_,
    const Cell* rule,
    Specifier* specifier
){
    USE_PARAMS_OF_SUBPARSE;

    // !!! Experiment: Put the quote level of the original series back on when
    // setting positions (then remove)
    //
    //     parse just '''{abc} ["a" mark x:]` => '''{bc}

    Quotify(ARG(position), P_NUM_QUOTES);

    enum Reb_Kind k = VAL_TYPE(rule);
    if (k == REB_WORD or k == REB_SET_WORD) {
        Copy_Cell(Sink_Word_May_Fail(rule, specifier), ARG(position));
    }
    else if (
        k == REB_PATH or k == REB_SET_PATH
        or k == REB_TUPLE or k == REB_SET_TUPLE
    ){
        // !!! Assume we might not be able to corrupt SPARE (rule may be
        // in SPARE?)
        //
        DECLARE_LOCAL (temp);
        Quotify(Derelativize(OUT, rule, specifier), 1);
        if (rebRunThrows(
            cast(REBVAL*, temp),  // <-- output cell
            Canon(SET), OUT, ARG(position)
        )){
            fail (Error_No_Catch_For_Throw(LEVEL));
        }
        FRESHEN(OUT);
    }
    else
        fail (Error_Parse_Variable(level_));

    Dequotify(ARG(position));  // go back to 0 quote level
}


static void Handle_Seek_Rule_Dont_Update_Begin(
    Level* level_,
    const Cell* rule,
    Specifier* specifier
){
    USE_PARAMS_OF_SUBPARSE;

    enum Reb_Kind k = VAL_TYPE(rule);
    if (k == REB_WORD or k == REB_GET_WORD or k == REB_TUPLE) {
        Get_Var_May_Fail(SPARE, rule, specifier, false);
        rule = SPARE;
        k = VAL_TYPE(rule);
    }

    REBINT index;
    if (k == REB_INTEGER) {
        index = VAL_INT32(rule);
        if (index < 1)
            fail ("Cannot SEEK a negative integer position");
        --index;  // Rebol is 1-based, C is 0 based...
    }
    else if (Any_Series_Kind(k)) {
        if (Cell_Series(rule) != P_INPUT)
            fail ("Switching PARSE series is not allowed");
        index = VAL_INDEX(rule);
    }
    else {  // #1263
        DECLARE_LOCAL (specific);
        Derelativize(specific, rule, P_RULE_SPECIFIER);
        fail (Error_Parse_Series_Raw(specific));
    }

    if (cast(REBLEN, index) > P_INPUT_LEN)
        P_POS = P_INPUT_LEN;
    else
        P_POS = index;
}

// !!! Note callers will `continue` without any post-"match" processing, so
// the only way `begin` will get set for the next rule is if they set it,
// else commands like INSERT that follow will insert at the old location.
//
// https://github.com/rebol/rebol-issues/issues/2269
//
// Without known resolution on #2269, it isn't clear if there is legitimate
// meaning to seeking a parse in mid rule or not.  So only reset the begin
// position if the seek appears to be a "separate rule" in its own right.
//
#define HANDLE_SEEK_RULE_UPDATE_BEGIN(f,rule,specifier) \
    Handle_Seek_Rule_Dont_Update_Begin((L), (rule), (specifier)); \
    if (not (P_FLAGS & PF_STATE_MASK)) \
        begin = P_POS;


//
//  subparse: native [
//
//  {Internal support function for PARSE (acts as variadic to consume rules)}
//
//      return: [<opt> integer!]
//      input [any-series! any-array! quoted!]
//      flags [integer!]
//      /collection "Array into which any KEEP values are collected"
//          [any-series!]
//      <local> position num-quotes save
//  ]
//
DECLARE_NATIVE(subparse)
//
// Rules are matched until one of these things happens:
//
// * A rule fails, and is not then picked up by a later "optional" rule.
// This returns NULL.
//
// * You run out of rules to apply without any failures or errors, and the
// position in the input series is returned.  This may be at the end of
// the input data or not--it's up to the caller to decide if that's relevant.
// This will return OUT with out containing an integer index.
//
// !!! The return of an integer index is based on the R3-Alpha convention,
// but needs to be rethought in light of the ability to switch series.  It
// does not seem that all callers of Subparse's predecessor were prepared for
// the semantics of switching the series.
//
// * A `fail()`, in which case the function won't return--it will longjmp
// up to the most recently pushed handler.  This can happen due to an invalid
// rule pattern, or if there's an error in code that is run in parentheses.
//
// * A throw-style result caused by DO code run in parentheses (e.g. a
// THROW, RETURN, BREAK, CONTINUE).  This returns a thrown value.
//
// * A special throw to indicate a return out of the PARSE itself, triggered
// by the RETURN instruction.  This also returns a thrown value, but will
// be caught by PARSE before returning.
//
{
    INCLUDE_PARAMS_OF_SUBPARSE;

    UNUSED(ARG(flags));  // used via P_FLAGS

    Level* L = level_;  // nice alias of implicit native parameter

    // If the input is quoted, e.g. `parse just ''''[...] [rules]`, we dequote
    // it while we are processing the ARG().  This is because we are trying
    // to update and maintain the value as we work in a way that can be shown
    // in the debug stack frame.
    //
    // But we save the number of quotes in a local variable.  This way we can
    // put the quotes back on whenever doing a COPY etc.
    //
    assert(Is_Trash(ARG(num_quotes)));
    Init_Integer(ARG(num_quotes), Cell_Num_Quotes(ARG(input)));
    Dequotify(ARG(input));

    // Make sure index position is not past END
    if (
        VAL_INDEX_UNBOUNDED(ARG(input))
        > cast(REBIDX, Cell_Series_Len_Head(ARG(input)))
    ){
        VAL_INDEX_RAW(ARG(input)) = Cell_Series_Len_Head(ARG(input));
    }

    assert(Is_Trash(ARG(position)));
    Copy_Cell(ARG(position), ARG(input));

    // Every time we hit an alternate rule match (with |), we have to reset
    // any of the collected values.  Remember the tail when we started.
    //
    // !!! Could use the VAL_INDEX() of ARG(collect) for this
    //
    // !!! How this interplays with throws that might be caught before the
    // COLLECT's stack level is not clear (mostly because ACCEPT and REJECT
    // were not clear; many cases dropped them on the floor in R3-Alpha, and
    // no real resolution exists...see the UNUSED(interrupted) cases.)
    //
    REBLEN collection_tail = P_COLLECTION ? Array_Len(P_COLLECTION) : 0;
    UNUSED(ARG(collection));  // implicitly accessed as P_COLLECTION

    assert(Is_Fresh(OUT));  // invariant provided by parse3

  #if !defined(NDEBUG)
    //
    // These parse state variables live in frame varlists, which can be
    // annoying to find to inspect in the debugger.  This makes pointers into
    // the value payloads so they can be seen more easily.
    //
    const REBIDX *pos_debug = &P_POS;
    (void)pos_debug;  // UNUSED() forces corruption in C++11 debug builds
  #endif

    REBIDX begin = P_POS;  // point at beginning of match

    // The loop iterates across each REBVAL's worth of "rule" in the rule
    // block.  Some of these rules just set `flags` and `continue`, so that
    // the flags will apply to the next rule item.  If the flag is PF_SET
    // or PF_COPY, then the `set_or_copy_word` pointers will be assigned
    // at the same time as the active target of the COPY or SET.
    //
    // !!! This flagging process--established by R3-Alpha--is efficient
    // but somewhat haphazard.  It may work for `while ["a" | "b"]` to
    // "set the PF_WHILE" flag when it sees the `while` and then iterate
    // a rule it would have otherwise processed just once.  But there are
    // a lot of edge cases like `while |` where this method isn't set up
    // to notice a "grammar error".  It could use review.
    //
    assert((P_FLAGS & PF_STATE_MASK) == 0);

    const Cell* set_or_copy_word = NULL;

    REBINT mincount = 1;  // min pattern count
    REBINT maxcount = 1;  // max pattern count


    //==////////////////////////////////////////////////////////////////==//
    //
    // PRE-RULE PROCESSING SECTION
    //
    //==////////////////////////////////////////////////////////////////==//

    // For non-iterated rules, including setup for iterated rules.
    // The input index is not advanced here, but may be changed by
    // a GET-WORD variable.

  pre_rule: ;  // next line is declaration, need semicolon

    const Cell* rule = P_AT_END ? nullptr : P_RULE;

    Update_Expression_Start(L);

    //=//// FIRST THINGS FIRST: CHECK FOR END /////////////////////////////=//

    if (rule == nullptr)
        goto do_signals;

    //=//// HANDLE BAR! (BEFORE GROUP!) ///////////////////////////////////=//

    // BAR!s cannot be abstracted.  If they could be, then you'd have to
    // run all GET-GROUP! `:(...)` to find them in alternates lists.
    //
    // Note: First test, so `[| ...anything...]` is a "no-op" match

    if (IS_BAR(rule))  // reached BAR! without a match failure, good!
        return Init_Integer(OUT, P_POS);  // indicate match @ current pos

    //=//// HANDLE COMMA! (BEFORE GROUP...?) //////////////////////////////=//

    // The R3-Alpha PARSE design wasn't based on any particular notion of
    // "instruction format"; it fiddled a lot of flags like PF_WHILE to know
    // what construct you were in.  So things like `parse "a" [some]` were not
    // set up to deliver errors in a sense of "keywords that take arguments".
    //
    // Hence in this formulation, an expression barrier is a little hard to
    // make.  PARSE should be rewritten in a better way, but until it is
    // the we have to intuit the rule situation.
    //
    // !!! For now we assume that a GROUP! evaluation to produce a comma
    // will just error, vs. be okay in interstitial positions.  But unlike
    // BAR! there's no scan skipping that *requires* commas to be at source
    // level, so this could be relaxed if there was a good reason to.

    if (Is_Comma(rule)) {
        if (mincount != 1 or maxcount != 1 or (P_FLAGS & PF_STATE_MASK))
            fail (Error_Expression_Barrier_Raw());
        FETCH_NEXT_RULE(L);
        goto pre_rule;
    }

    //=//// (GROUP!) AND :(GET-GROUP!) PROCESSING /////////////////////////=//

    if (Is_Group(rule) or Is_Get_Group(rule)) {

        // Code below may jump here to re-process groups, consider:
        //
        //    rule: just (print "Hi")
        //    parse "a" [:('rule) "a"]
        //
        // First it processes the group to get RULE, then it looks that
        // up and gets another group.  In theory this could continue
        // indefinitely, but for now a GET-GROUP! can't return another.

      process_group: {

        bool inject = Is_Get_Group(rule);
        if (Process_Group_For_Parse_Throws(SPARE, L, rule))
            return THROWN;

        if (not inject or Is_Void(SPARE)) {  // (...) or void :(...)
            FETCH_NEXT_RULE(L);  // ignore result and go on to next rule
            goto pre_rule;
        }
        rule = Move_Cell(P_SAVE, SPARE);
    }}
    else {
        // If we ran the GROUP! then that invokes the evaluator, and so
        // we already gave the GC and cancellation a chance to run.  But
        // if not, we might want to do it here... (?)

      do_signals:

        Update_Tick_If_Enabled();

        if (--g_ts.eval_countdown <= 0) {
            if (Do_Signals_Throws(LEVEL))
                return THROWN;
        }

        Maybe_DebugBreak_On_Tick();
    }

    // Some iterated rules have a parameter.  `3 into [some "a"]` will
    // actually run the INTO `rule` 3 times with the `subrule` of
    // `[some "a"]`.  Because it is iterated it is only captured the first
    // time through, nullptr indicates it's not been captured yet.
    //
    const Cell* subrule = nullptr;

    if (rule == nullptr)  // means at end
        goto return_position;  // done all needed to do for end position

    //=//// ANY-WORD!/ANY-PATH! PROCESSING ////////////////////////////////=//

    if (Is_Word(rule) or Is_Get_Word(rule) or Is_Set_Word(rule)) {
        Option(SymId) cmd = VAL_CMD(rule);
        if (cmd) {
            if (not Is_Word(rule)) {
                //
                // Command but not WORD! (COPY:, :THRU)
                //
                fail (Error_Parse_Command(L));
            }

            if (cmd > SYM_BREAK)  // R3-Alpha claimed "optimization"
                goto skip_pre_rule;  // but jump tables are fast, review

            switch (cmd) {
              case SYM_WHILE:
                if (not (P_FLAGS & PF_REDBOL)) {
                    fail (
                        "Please replace PARSE3's WHILE with TRY SOME -or-"
                        " TRY FURTHER SOME--it's being reclaimed as arity-2."
                        " https://forum.rebol.info/t/1540/12 (or use PARSE2)"
                    );
                }

              run_while_rule:
                P_FLAGS |= PF_LOOPING;
                assert(mincount == 1 and maxcount == 1);  // true on entry
                mincount = 0;
                maxcount = INT32_MAX;
                FETCH_NEXT_RULE(L);
                P_FLAGS |= PF_LOOPING;
                goto pre_rule;

              case SYM_SOME:
                if (P_FLAGS & PF_REDBOL) {
                    P_FLAGS |= PF_FURTHER;
                }
                assert(
                    (mincount == 1 or mincount == 0)  // could be OPT SOME
                    and maxcount == 1
                );  // true on entry
                P_FLAGS |= PF_LOOPING;
                maxcount = INT32_MAX;
                FETCH_NEXT_RULE(L);
                goto pre_rule;

              case SYM_ANY:
                if (P_FLAGS & PF_REDBOL) {
                    P_FLAGS |= PF_FURTHER;
                    goto run_while_rule;
                }
                fail (
                    "Please replace PARSE's ANY with TRY SOME"
                    " -- it's being reclaimed for a new construct"
                    " https://forum.rebol.info/t/1540/12 (or use PARSE2)"
                );

              case SYM_REPEAT:
                //
                // !!! OPT REPEAT (N) RULE can't work because OPT is done by
                // making the minimum number of match counts zero.  But
                // unfortunately if that rule isn't in a BLOCK! then the
                // 0 repeat rule transfers onto the rule... making it act like
                // `REPEAT (N) OPT RULE` which is not the same.
                //

                if (mincount != 1 or maxcount != 1)
                    fail (
                        "Old PARSE REPEAT does not mix with ranges or OPT"
                        " so put a block around the REPEAT or use UPARSE!"
                    );

                FETCH_NEXT_RULE(L);
                if (not Is_Group(P_RULE))
                    fail ("Old PARSE REPEAT requires GROUP! for times count");

                assert(Is_Fresh(OUT));
                if (Eval_Value_Throws(OUT, P_RULE, P_RULE_SPECIFIER))
                    goto return_thrown;

                if (Is_Integer(OUT)) {
                    mincount = Int32s(OUT, 0);
                    maxcount = Int32s(OUT, 0);
                } else {
                    if (
                        not Is_Block(OUT)
                        or not (
                            Cell_Series_Len_At(OUT) == 2
                            and Is_Integer(Cell_Array_Item_At(OUT))
                            and Is_Integer(Cell_Array_Item_At(OUT) + 1)
                        )
                    ){
                        fail ("REPEAT takes INTEGER! or length 2 BLOCK! range");
                    }

                    mincount = Int32s(Cell_Array_Item_At(OUT), 0);
                    maxcount = Int32s(Cell_Array_Item_At(OUT) + 1, 0);

                    if (maxcount < mincount)
                        fail ("REPEAT range can't have lower max than minimum");
                }

                FRESHEN(OUT);

                FETCH_NEXT_RULE(L);
                goto pre_rule;

              case SYM_FURTHER:  // require advancement
                P_FLAGS |= PF_FURTHER;
                FETCH_NEXT_RULE(L);
                goto pre_rule;

              case SYM_OPT:
                if (not (P_FLAGS & PF_REDBOL))
                    fail ("Please use TRY instead of OPT in PARSE3");
                goto try_or_opt;

              case SYM_TRY:
                if (P_FLAGS & PF_REDBOL)
                    fail ("Please use OPT instead of TRY in PARSE2");
                goto try_or_opt;

              try_or_opt:
                P_FLAGS |= PF_TRY;
                mincount = 0;
                FETCH_NEXT_RULE(L);
                goto pre_rule;

              case SYM_COPY:
                P_FLAGS |= PF_COPY;
                goto set_or_copy_pre_rule;

              case SYM_SET:
                P_FLAGS |= PF_SET;
                goto set_or_copy_pre_rule;

              case SYM_LET:
              set_or_copy_pre_rule:

                FETCH_NEXT_RULE(L);

                if (not (Is_Word(P_RULE) or Is_Set_Word(P_RULE)))
                    fail (Error_Parse_Variable(L));

                if (VAL_CMD(P_RULE))  // set set [...]
                    fail (Error_Parse_Command(L));

                // We need to add a new binding before we derelativize w.r.t.
                // the in-effect specifier.
                //
                if (cmd == SYM_LET) {
                    BINDING(FEED_SINGLE(L->feed)) = Make_Let_Patch(
                        Cell_Word_Symbol(P_RULE),
                        P_RULE_SPECIFIER
                    );
                    if (Is_Word(P_RULE)) {  // no further action
                        FETCH_NEXT_RULE(L);
                        goto pre_rule;
                    }
                    goto handle_set;
                }

                FETCH_NEXT_RULE_KEEP_LAST(&set_or_copy_word, L);
                goto pre_rule;

              case SYM_COLLECT:
                fail ("COLLECT should only follow a SET-WORD! in PARSE");

              case SYM_KEEP: {
                if (not P_COLLECTION)
                    fail ("Used PARSE KEEP with no COLLECT in effect");

                FETCH_NEXT_RULE(L);  // e.g. skip the KEEP word!

                REBLEN pos_before = P_POS;

                rule = Get_Parse_Value(P_SAVE, P_RULE, P_RULE_SPECIFIER);

                // If you say KEEP (whatever) then that acts like /ONLY did
                // and KEEP SPREAD (whatever) will splice
                //
                bool spread;
                if (Is_Word(rule) and Cell_Word_Id(rule) == SYM_SPREAD) {
                    spread = true;
                    FETCH_NEXT_RULE(L);  // e.g. skip the SPREAD
                    rule = Get_Parse_Value(P_SAVE, P_RULE, P_RULE_SPECIFIER);
                }
                else
                    spread = false;

                if (Is_Group(rule)) {
                    //
                    // !!! GROUP! means ordinary evaluation of material
                    // that is not matched as a PARSE rule; this is an idea
                    // which is generalized in UPARSE
                    //
                    if (Do_Any_Array_At_Throws(
                        OUT,
                        rule,
                        rule_specifier()
                    )){
                        goto return_thrown;
                    }

                    if (Is_Void(OUT))
                        fail (Error_Bad_Void());

                    if (spread) {
                        if (not Is_Block(OUT))
                            fail ("SPREAD only works with BLOCK! in PARSE3");

                        rebElide(
                            Canon(APPEND), ARG(collection),
                                Canon(SPREAD), rebQ(OUT)
                        );
                    }
                    else
                        rebElide(Canon(APPEND), ARG(collection), rebQ(OUT));

                    FRESHEN(OUT);  // since we didn't throw, put it back

                    // Don't touch P_POS, we didn't consume anything from
                    // the input series but just fabricated DO material.

                    FETCH_NEXT_RULE(L);
                }
                else {  // Ordinary rule (may be block, may not be)

                    Level* sub = Make_Level(L->feed, LEVEL_MASK_NONE);

                    bool interrupted;
                    assert(Is_Fresh(OUT));  // invariant until finished
                    bool threw = Subparse_Throws(
                        &interrupted,
                        OUT,
                        ARG(position),
                        SPECIFIED,
                        sub,
                        P_COLLECTION,
                        (P_FLAGS & PF_FIND_MASK) | PF_ONE_RULE
                            | (P_FLAGS & PF_REDBOL)
                    );

                    UNUSED(interrupted);  // !!! ignore ACCEPT/REJECT (?)

                    if (threw)
                        goto return_thrown;

                    if (Is_Nulled(OUT)) {  // match of rule failed
                        FRESHEN(OUT);  // restore invariant
                        goto next_alternate;  // backtrack collect, seek |
                    }
                    REBLEN pos_after = VAL_INT32(OUT);
                    FRESHEN(OUT);  // restore invariant

                    assert(pos_after >= pos_before);  // 0 or more matches

                    Array* target;
                    if (pos_after == pos_before and spread) {
                        target = nullptr;
                    }
                    else if (Any_String_Kind(P_TYPE)) {
                        target = nullptr;
                        Init_Any_String(
                            Alloc_Tail_Array(P_COLLECTION),
                            P_TYPE,
                            Copy_String_At_Limit(
                                ARG(position),
                                pos_after - pos_before
                            )
                        );
                    }
                    else if (not Is_Series_Array(P_INPUT)) {  // BINARY! (?)
                        target = nullptr;  // not an array, one item
                        Init_Series_Cell(
                            Alloc_Tail_Array(P_COLLECTION),
                            P_TYPE,
                            Copy_Binary_At_Len(
                                P_INPUT,
                                pos_before,
                                pos_after - pos_before
                            )
                        );
                    }
                    else if (not spread) {  // taken to mean "add as one block"
                        target = Make_Array_Core(
                            pos_after - pos_before,
                            NODE_FLAG_MANAGED
                        );
                        Init_Block(Alloc_Tail_Array(P_COLLECTION), target);
                    }
                    else
                        target = P_COLLECTION;

                    if (target) {
                        REBLEN n;
                        for (n = pos_before; n < pos_after; ++n) {
                            Derelativize(
                                Alloc_Tail_Array(target),
                                Array_At(P_INPUT_ARRAY, n),
                                P_INPUT_SPECIFIER
                            );
                        }
                    }

                    P_POS = pos_after;  // continue from end of kept data
                }
                goto pre_rule; }

              case SYM_NOT_1:  // see TO-C-NAME
                P_FLAGS |= PF_NOT;
                P_FLAGS ^= PF_NOT2;
                FETCH_NEXT_RULE(L);
                goto pre_rule;

              case SYM_AND_1:  // see TO-C-NAME
              case SYM_AHEAD:
                P_FLAGS |= PF_AHEAD;
                FETCH_NEXT_RULE(L);
                goto pre_rule;

              case SYM_REMOVE:
                P_FLAGS |= PF_REMOVE;
                FETCH_NEXT_RULE(L);
                goto pre_rule;

              case SYM_INSERT:
                P_FLAGS |= PF_INSERT;
                FETCH_NEXT_RULE(L);
                goto post_match_processing;

              case SYM_CHANGE:
                P_FLAGS |= PF_CHANGE;
                FETCH_NEXT_RULE(L);
                goto pre_rule;

                // IF is deprecated in favor of `:(expression)`.  But it is
                // currently used for bootstrap.  Remove once the bootstrap
                // executable is updated to have GET-GROUP!s.  Substitution:
                //
                //    (go-on?: either condition [[accept]][[reject]])
                //    go-on?
                //
                // !!! Note: PARSE3/REDBOL may be a modality it needs to
                // support, and Red added IF.  It might be necessary to keep
                // it (though Rebol2 did not have IF in PARSE...)
                //
              case SYM_IF: {
                FETCH_NEXT_RULE(L);
                if (P_AT_END)
                    fail (Error_Parse_End());

                if (not Is_Group(P_RULE))
                    fail (Error_Parse_Rule());

                DECLARE_LOCAL (condition);
                if (Do_Any_Array_At_Throws(  // note: might GC
                    condition,
                    P_RULE,
                    P_RULE_SPECIFIER
                )) {
                    goto return_thrown;
                }

                FETCH_NEXT_RULE(L);

                if (Is_Truthy(Stable_Unchecked(condition)))
                    goto pre_rule;

                Init_Nulled(ARG(position));  // not found
                goto post_match_processing; }

              case SYM_ACCEPT: {
                //
                // ACCEPT means different things in Rebol2/Red (synonym for
                // BREAK) where in UPARSE it means RETURN.
                //
                if (P_FLAGS & PF_REDBOL)
                    goto handle_break;

              handle_accept:

                FETCH_NEXT_RULE(L);

                DECLARE_LOCAL (thrown_arg);
                if (Is_Tag(P_RULE)) {
                    if (rebDid(P_RULE, "= <here>"))
                        Copy_Cell(thrown_arg, ARG(position));
                    else
                        fail ("PARSE3 ACCEPT TAG! only works with <here>");
                }
                else if (Is_Group(P_RULE)) {
                    if (Eval_Value_Throws(thrown_arg, P_RULE, P_RULE_SPECIFIER))
                        goto return_thrown;
                }
                else
                    fail ("PARSE3 ACCEPT only works with GROUP! and <here>");

                Init_Thrown_With_Label(LEVEL, thrown_arg, Lib(PARSE_ACCEPT));
                goto return_thrown; }

              handle_break:
              case SYM_BREAK: {
                //
                // This has to be throw-style, because it's not enough
                // to just say the current rule succeeded...it climbs
                // up and affects an enclosing parse loop.
                //
                DECLARE_LOCAL (thrown_arg);
                Init_Integer(thrown_arg, P_POS);

                Init_Thrown_With_Label(LEVEL, thrown_arg, Lib(PARSE_BREAK));
                goto return_thrown; }

              case SYM_REJECT: {
                //
                // Similarly, this is a break/continue style "throw"
                //
                Init_Thrown_With_Label(LEVEL, Lib(NULL), Lib(PARSE_REJECT));
                goto return_thrown; }

              case SYM_FAIL:  // deprecated... use LOGIC! false instead
                Init_Nulled(ARG(position));  // not found
                FETCH_NEXT_RULE(L);
                goto post_match_processing;

              case SYM_LIMIT:
                fail (Error_Not_Done_Raw());

              case SYM__Q_Q:
                Print_Parse_Index(L);
                FETCH_NEXT_RULE(L);
                goto pre_rule;

              case SYM_RETURN:
                if (P_FLAGS & PF_REDBOL)
                    goto handle_accept;
                fail ("RETURN keyword switched to ACCEPT in PARSE3/UPARSE");

              case SYM_SEEK: {
                FETCH_NEXT_RULE(L);  // skip the SEEK word
                // !!! what about `seek ^(first x)` ?
                HANDLE_SEEK_RULE_UPDATE_BEGIN(L, P_RULE, P_RULE_SPECIFIER);
                FETCH_NEXT_RULE(L);  // e.g. skip the `x` in `seek x`
                goto pre_rule; }

              default:  // the list above should be exhaustive
                assert(false);
            }

          skip_pre_rule:;

            // Any other WORD! with VAL_CMD() is a parse keyword, but is
            // a "match command", so proceed...
        }
        else {
            // It's not a PARSE command, get or set it

            // Historically SET-WORD! was used to capture the parse position.
            // However it is being repurposed as the tool for any form of
            // assignment...a new generalized SET.
            //
            // UPARSE2 should be used with code that wants the old semantics.
            // The performance on that should increase with time.
            //
            if (Is_Set_Word(rule)) {
                //
                // !!! Review meaning of marking the parse in a slot that
                // is a target of a rule, e.g. `thru pos: xxx`
                //
                // https://github.com/rebol/rebol-issues/issues/2269

                if (P_FLAGS & PF_REDBOL) {
                    FETCH_NEXT_RULE_KEEP_LAST(&set_or_copy_word, L);
                    Handle_Mark_Rule(L, set_or_copy_word, P_RULE_SPECIFIER);
                    goto pre_rule;
                }

                goto handle_set;
            }
            else if (Is_Get_Word(rule)) {
                //
                // :word - change the index for the series to a new position
                //
                if (P_FLAGS & PF_REDBOL) {
                    Handle_Seek_Rule_Dont_Update_Begin(L, rule, P_RULE_SPECIFIER);
                    FETCH_NEXT_RULE(L);
                    goto pre_rule;
                }
                fail ("GET-WORD! in PARSE is reserved (unless using PARSE2)");
            }
            else {
                assert(Is_Word(rule));  // word - some other variable

                if (rule != P_SAVE) {
                    Get_Parse_Value(P_SAVE, rule, P_RULE_SPECIFIER);
                    rule = P_SAVE;
                }
            }
        }
    }
    else if (Is_Tuple(rule)) {
        Get_Var_May_Fail(SPARE, rule, P_RULE_SPECIFIER, false);
        rule = Copy_Cell(P_SAVE, SPARE);
    }
    else if (Is_Set_Tuple(rule)) {
      handle_set:
        FETCH_NEXT_RULE_KEEP_LAST(&set_or_copy_word, L);

        // As an interim measure, permit `pos: <here>` to act as
        // setting the position, just as `pos:` did historically.
        // This will change to be generic SET after this has had some
        // time to settle.  Allows also `here: <here>` with `pos: here`
        //
        if (Is_Word(P_RULE) and Cell_Word_Id(P_RULE) == SYM_ACROSS) {
            FETCH_NEXT_RULE(L);
            P_FLAGS |= PF_COPY;
            goto pre_rule;
        }
        else if (
            Is_Word(P_RULE)
            and Cell_Word_Id(P_RULE) == SYM_COLLECT
        ){
            FETCH_NEXT_RULE(L);

            Array* collection = Make_Array_Core(
                10,  // !!! how big?
                NODE_FLAG_MANAGED
            );
            Push_GC_Guard(collection);

            Level* sub = Make_Level(L->feed, LEVEL_MASK_NONE);

            bool interrupted;
            assert(Is_Fresh(OUT));  // invariant until finished
            bool threw = Subparse_Throws(
                &interrupted,
                OUT,
                ARG(position),  // affected by P_POS assignment above
                SPECIFIED,
                sub,
                collection,
                (P_FLAGS & PF_FIND_MASK) | PF_ONE_RULE
                    | (P_FLAGS & PF_REDBOL)
            );

            Drop_GC_Guard(collection);
            UNUSED(interrupted);  // !!! ignore ACCEPT/REJECT (?)

            if (threw)
                goto return_thrown;

            if (Is_Nulled(OUT)) {  // match of rule failed
                FRESHEN(OUT);  // restore invariant
                goto next_alternate;  // backtrack collect, seek |
            }
            P_POS = VAL_INT32(OUT);
            FRESHEN(OUT);  // restore invariant

            Init_Block(
                Sink_Word_May_Fail(set_or_copy_word, P_RULE_SPECIFIER),
                collection
            );
            goto pre_rule;
        }
        else if (Is_Word(P_RULE)) {
            DECLARE_STABLE (temp);
            const Cell* gotten = Get_Parse_Value(
                temp,
                P_RULE,
                P_RULE_SPECIFIER
            );
            bool strict = true;
            if (
                Is_Tag(gotten)
                and 0 == CT_String(gotten, Root_Here_Tag, strict)
            ){
                FETCH_NEXT_RULE(L);
            }
            // fall through
        }
        else if (Is_Tag(P_RULE)) {
            bool strict = true;
            if (0 == CT_String(P_RULE, Root_Here_Tag, strict))
                FETCH_NEXT_RULE(L);

            // fall through
        }
        else
            fail ("PARSE SET-WORD! use with <HERE>, COLLECT, ACROSS");

        Handle_Mark_Rule(L, set_or_copy_word, P_RULE_SPECIFIER);
        goto pre_rule;
    }
    else if (Any_Path(rule)) {
        fail ("Use TUPLE! a.b.c instead of PATH! a/b/c");
    }

    if (IS_BAR(rule))
        fail ("BAR! must be source level (else PARSE can't skip it)");

    if (Is_Nulled(rule))
        fail ("NULL rules are not allowed in PARSE");

    if (Is_Logic(rule)) {  // true is a no-op, false causes match failure
        if (Cell_Logic(rule)) {
            FETCH_NEXT_RULE(L);
            goto pre_rule;
        }
        FETCH_NEXT_RULE(L);
        Init_Nulled(ARG(position));  // not found
        goto post_match_processing;
    }
    else switch (VAL_TYPE(rule)) {
      case REB_GROUP:
        goto process_group;  // GROUP! can make WORD! that fetches GROUP!

      case REB_INTEGER:  // Specify repeat count
        mincount = maxcount = Int32s(rule, 0);

        FETCH_NEXT_RULE(L);
        if (P_AT_END)
            fail (Error_Parse_End());

        rule = Get_Parse_Value(P_SAVE, P_RULE, P_RULE_SPECIFIER);

        if (Is_Integer(rule)) {
            if (P_FLAGS & PF_REDBOL)
                maxcount = Int32s(rule, 0);
            else
                fail (
                    "[1 2 rule] now illegal https://forum.rebol.info/t/1578/6"
                    " (or use PARSE2)"
                );
        }
        break;

      case REB_TAG: {  // tag combinator in UPARSE, matches in UPARSE2
        if (P_FLAGS & PF_REDBOL)
            break;  // gets treated as literal item

        bool strict = true;
        if (0 == CT_String(rule, Root_Here_Tag, strict)) {
            FETCH_NEXT_RULE(L);  // not being assigned w/set-word!, no-op
            goto pre_rule;
        }
        if (0 == CT_String(rule, Root_End_Tag, strict)) {
            FETCH_NEXT_RULE(L);
            begin = P_POS;
            goto handle_end;
        }
        fail ("Only TAG! combinators PARSE3 supports are <here> and <end>"); }

      default:;
        // Fall through to next section
    }


    //==////////////////////////////////////////////////////////////////==//
    //
    // ITERATED RULE PROCESSING SECTION
    //
    //==////////////////////////////////////////////////////////////////==//

    // Repeats the same rule N times or until the rule fails.
    // The index is advanced and stored in a temp variable i until
    // the entire rule has been satisfied.

    FETCH_NEXT_RULE(L);

    begin = P_POS;  // input at beginning of match section

    REBINT count;  // gotos would cross initialization
    count = 0;
    while (count < maxcount) {
        assert(
            not IS_BAR(rule)
            and not Is_Logic(rule)
            and not Is_Integer(rule)
            and not Is_Group(rule)
        );  // these should all have been handled before iterated section

        REBIXO i;  // temp index point

        if (Is_Word(rule)) {  // could be literal BLANK!, now SYM_SKIP
            Option(SymId) cmd = VAL_CMD(rule);

            switch (cmd) {
              case SYM_SKIP:
                i = (P_POS < cast(REBIDX, P_INPUT_LEN))
                    ? cast(REBLEN, P_POS + 1)
                    : END_FLAG;
                break;

              case SYM_END:
                if (not (P_FLAGS & PF_REDBOL))
                    fail ("Use <end> instead of END outside PARSE2");
                goto handle_end;

              case SYM_TO:
              case SYM_THRU: {
                if (P_AT_END)
                    fail (Error_Parse_End());

                if (!subrule) {  // capture only on iteration #1
                    subrule = Get_Parse_Value(
                        P_SAVE, P_RULE, P_RULE_SPECIFIER
                    );
                    FETCH_NEXT_RULE(L);
                }

                bool is_thru = (cmd == SYM_THRU);

                if (Is_Block(subrule))
                    i = To_Thru_Block_Rule(L, subrule, is_thru);
                else
                    i = To_Thru_Non_Block_Rule(L, subrule, is_thru);
                break; }

              case SYM_QUOTE:  // temporarily behaving like LIT for bootstrap
              case SYM_THE: {
                if (not Is_Series_Array(P_INPUT))
                    fail (Error_Parse_Rule());  // see #2253

                if (P_AT_END)
                    fail (Error_Parse_End());

                if (not subrule)  // capture only on iteration #1
                    FETCH_NEXT_RULE_KEEP_LAST(&subrule, L);

                const Cell* input_tail = Array_Tail(P_INPUT_ARRAY);
                const Cell* cmp = Array_At(P_INPUT_ARRAY, P_POS);

                if (cmp == input_tail)
                    i = END_FLAG;
                else if (
                    0 == Cmp_Value(cmp, subrule, did (P_FLAGS & AM_FIND_CASE))
                ){
                    i = P_POS + 1;
                }
                else
                    i = END_FLAG;
                break; }

              case SYM_INTO: {
                if (P_AT_END)
                    fail (Error_Parse_End());

                if (!subrule) {
                    subrule = Get_Parse_Value(
                        P_SAVE, P_RULE, P_RULE_SPECIFIER
                    );
                    FETCH_NEXT_RULE(L);
                }

                if (not Is_Block(subrule))
                    fail (Error_Parse_Rule());

                // parse ["aa"] [into ["a" "a"]] ; is legal
                // parse "aa" [into ["a" "a"]] ; is not...already "into"
                //
                if (not Is_Series_Array(P_INPUT))
                    fail (Error_Parse_Rule());

                Element(const*) input_tail = Array_Tail(P_INPUT_ARRAY);
                Element(const*) into = Array_At(P_INPUT_ARRAY, P_POS);
                if (into == input_tail) {
                    i = END_FLAG;  // `parse [] [into [...]]`, rejects
                    break;
                }
                else if (Any_Path_Kind(Cell_Heart(into))) {
                    //
                    // Can't PARSE an ANY-PATH! because it has no position
                    // But would be inconvenient if INTO did not support.
                    // Transform implicitly into a BLOCK! form.
                    //
                    // !!! Review faster way of sharing the AS transform.
                    //
                    Derelativize(SPARE, into, P_INPUT_SPECIFIER);
                    into = cast(Element(*), rebValue("as block! @", SPARE));
                }
                else if (
                    not Any_Series_Kind(Cell_Heart(into))
                ){
                    i = END_FLAG;  // `parse [1] [into [...]`, rejects
                    break;
                }

                Level* sub = Make_Level_At_Core(
                    subrule, P_RULE_SPECIFIER,
                    LEVEL_MASK_NONE
                );

                bool interrupted;
                if (Subparse_Throws(
                    &interrupted,
                    OUT,
                    into,
                    P_INPUT_SPECIFIER,  // harmless if specified API value
                    sub,
                    P_COLLECTION,
                    (P_FLAGS & PF_FIND_MASK)  // PF_ONE_RULE?
                        | (P_FLAGS & PF_REDBOL)
                )){
                    goto return_thrown;
                }

                // !!! ignore interrupted? (e.g. ACCEPT or REJECT ran)

                if (Is_Nulled(OUT)) {
                    i = END_FLAG;
                }
                else {
                    if (VAL_UINT32(OUT) != Cell_Series_Len_Head(into))
                        i = END_FLAG;
                    else
                        i = P_POS + 1;
                }

                if (Is_Api_Value(into))
                    rebRelease(x_cast(REBVAL*, into));  // !!! or use SPARE?

                FRESHEN(OUT);  // restore invariant
                break; }

              default:
                fail (Error_Parse_Rule());
            }
        }
        else if (Is_Block(rule)) {  // word fetched block, or inline block

            Level* sub = Make_Level_At_Core(
                rule, rule_specifier(),
                LEVEL_MASK_NONE
            );

            bool interrupted;
            if (Subparse_Throws(
                &interrupted,
                FRESHEN(SPARE),
                ARG(position),
                SPECIFIED,
                sub,
                P_COLLECTION,
                (P_FLAGS & PF_FIND_MASK)  // no PF_ONE_RULE
                    | (P_FLAGS & PF_REDBOL)
            )){
                return THROWN;
            }

            // Non-breaking out of loop instances of match or not.

            if (Is_Nulled(SPARE))
                i = END_FLAG;
            else {
                assert(Is_Integer(SPARE));
                i = VAL_INT32(SPARE);
            }

            if (interrupted) {  // ACCEPT or REJECT ran
                assert(i != THROWN_FLAG);
                if (i == END_FLAG)
                    Init_Nulled(ARG(position));
                else
                    P_POS = i;
                break;
            }
        }
        else if (false) {
          handle_end:
            count = 0;
            i = (P_POS < cast(REBIDX, P_INPUT_LEN))
                ? END_FLAG
                : P_INPUT_LEN;
        }
        else {
            // Parse according to datatype

            i = Parse_One_Rule(L, P_POS, rule);
            if (i == THROWN_FLAG)
                return THROWN;
        }

        assert(i != THROWN_FLAG);

        // i: indicates new index or failure of the *match*, but
        // that does not mean failure of the *rule*, because optional
        // matches can still succeed when the last match failed.
        //
        if (i == END_FLAG) {  // this match failed
            if (count < mincount) {
                Init_Nulled(ARG(position));  // num matches not enough
            }
            else {
                // just keep index as is.
            }
            break;
        }

        count++;  // may overflow to negative
        if (count < 0)
            count = INT32_MAX;  // the forever case

        // If FURTHER was used then the parse must advance the input; it can't
        // be at the saem position.
        //
        if (P_POS == cast(REBINT, i) and (P_FLAGS & PF_FURTHER)) {
            if (not (P_FLAGS & PF_LOOPING))
                Init_Nulled(ARG(position));  // fail the rule, not loop
            break;
        }

        P_POS = cast(REBLEN, i);
    }

    // !!! This out of bounds check is necessary because GROUP!s execute
    // code that could change the size of the input.  The idea of locking
    // the input and only allowing mutations through PARSE rules has come
    // up...but at the very least, such checks should only be needed right
    // after potential group executions (which includes subrules).
    //
    if (not Is_Nulled(ARG(position)))
        if (P_POS > cast(REBIDX, P_INPUT_LEN))
            Init_Nulled(ARG(position));  // not found


    //==////////////////////////////////////////////////////////////////==//
    //
    // "POST-MATCH PROCESSING"
    //
    //==////////////////////////////////////////////////////////////////==//

    // The comment here says "post match processing", but it may be a
    // failure signal.  Or it may have been a success and there could be
    // a NOT to apply.  Note that failure here doesn't mean returning
    // from SUBPARSE, as there still may be alternate rules to apply
    // with bar e.g. `[a | b | c]`.

  post_match_processing:;

    if (P_FLAGS & PF_STATE_MASK) {
        if (P_FLAGS & PF_NOT) {
            if ((P_FLAGS & PF_NOT2) and not Is_Nulled(ARG(position)))
                Init_Nulled(ARG(position));  // not found
            else {
                Copy_Cell(ARG(position), ARG(input));
                P_POS = begin;
            }
        }

        if (not Is_Nulled(ARG(position))) {
            //
            // Set count to how much input was advanced
            //
            count = (begin > P_POS) ? 0 : P_POS - begin;

            if (P_FLAGS & PF_COPY) {
                REBVAL *sink = Sink_Word_May_Fail(
                    set_or_copy_word,
                    P_RULE_SPECIFIER
                );
                if (Any_Array_Kind(P_TYPE)) {
                    //
                    // Act like R3-Alpha in preserving GROUP! vs. BLOCK!
                    // distinction (which Rebol2 did not).  But don't keep
                    // SET-XXX! or GET-XXX! (like how quoting is not kept)
                    //
                    Init_Array_Cell(
                        sink,
                        Any_Group_Kind(P_TYPE) ? REB_GROUP : REB_BLOCK,
                        Copy_Array_At_Max_Shallow(
                            P_INPUT_ARRAY,
                            begin,
                            count
                        )
                    );
                }
                else if (P_TYPE == REB_BINARY) {
                    Init_Binary(  // R3-Alpha behavior (e.g. not AS TEXT!)
                        sink,
                        Copy_Binary_At_Len(P_INPUT, begin, count)
                    );
                }
                else {
                    assert(Any_String_Kind(P_TYPE));

                    DECLARE_LOCAL (begin_val);
                    Init_Series_Cell_At(begin_val, P_TYPE, P_INPUT, begin);

                    // Rebol2 behavior of always "netural" TEXT!.  Avoids
                    // creation of things like URL!-typed fragments that
                    // have no scheme:// at their head, or getting <bc>
                    // out of <abcd> as if `<b` or `c>` had been found.
                    //
                    Init_Text(
                        sink,
                        Copy_String_At_Limit(begin_val, count)
                    );
                }

                // !!! As we are losing the datatype here, it doesn't make
                // sense to carry forward the quoting on the input.  It
                // is collecting items in a neutral container.  It is less
                // obvious what marking a position should do.
            }
            else if (P_FLAGS & PF_SET) {
                if (count > 1)
                    fail (Error_Parse_Multiple_Set_Raw());

                if (count == 0) {
                    //
                    // !!! Right now, a rule like `set x group!` will leave x
                    // alone if you don't match.  (This is the same as
                    // `maybe set x group!`).  Instead of being a synonym, the
                    // behavior of unsetting x has been considered, and to
                    // require saying `maybe set x group!` to get the no-op.
                    // But `opt x: group!` will set x to null on no match.
                    //
                    // Note: It should be `x: opt group!` but R3-Alpha parse
                    // is hard to get composability on such things.
                    //
                    if (P_FLAGS & PF_TRY)  // don't just leave alone
                        Init_Nulled(
                            Sink_Word_May_Fail(
                                set_or_copy_word,
                                P_RULE_SPECIFIER
                            )
                        );
                }
                else if (Is_Series_Array(P_INPUT)) {
                    assert(count == 1);  // check for > 1 would have errored

                    Copy_Relative_internal(
                        Sink_Word_May_Fail(set_or_copy_word, P_RULE_SPECIFIER),
                        Array_At(P_INPUT_ARRAY, begin)
                    );
                }
                else {
                    assert(count == 1);  // check for > 1 would have errored

                    REBVAL *var = Sink_Word_May_Fail(
                        set_or_copy_word, P_RULE_SPECIFIER
                    );

                    // A Git merge of UTF-8 everywhere put this here,
                    // with no corresponding use of "captured".  It's not
                    // clear what happened--leaving it here to investigate
                    // if a pertinent bug has a smoking gun here.

                    /*
                    DECLARE_LOCAL (begin_val);
                    Init_Series_Cell_At(begin_val, P_TYPE, P_INPUT, begin);
                    Init_Series_Cell(
                        captured,
                        P_TYPE,
                        Copy_String_At_Limit(begin_val, count)
                    );
                    */

                    if (P_TYPE == REB_BINARY)
                        Init_Integer(var, *Binary_At(P_INPUT_BINARY, begin));
                    else
                        Init_Char_Unchecked(
                            var,
                            Get_Char_At(P_INPUT_STRING, begin)
                        );
                }
            }

            if (P_FLAGS & PF_REMOVE) {
                Ensure_Mutable(ARG(position));
                if (count)
                    Remove_Any_Series_Len(ARG(position), begin, count);
                P_POS = begin;
            }

            if (P_FLAGS & (PF_INSERT | PF_CHANGE)) {
                count = (P_FLAGS & PF_INSERT) ? 0 : count;
                if (P_AT_END)
                    fail (Error_Parse_End());

                // new value...comment said "CHECK FOR QUOTE!!"
                rule = Get_Parse_Value(P_SAVE, P_RULE, P_RULE_SPECIFIER);
                FETCH_NEXT_RULE(L);

                // If you say KEEP (whatever) then that acts like /ONLY did
                // and KEEP (spread whatever) will splice
                //
                bool only;
                if (Any_Meta_Kind(VAL_TYPE(rule))) {
                    if (rule != P_SAVE) {  // move to mutable location
                        Derelativize(P_SAVE, rule, P_RULE_SPECIFIER);
                        rule = P_SAVE;
                    }
                    Plainify(P_SAVE);  // take the ^ off
                    only = true;
                }
                else
                    only = false;

                if (not Is_Group(rule))
                    fail ("Only (...) or ^(...) in old PARSE's CHANGE/INSERT");

                DECLARE_STABLE (evaluated);
                Specifier* derived = Derive_Specifier(
                    P_RULE_SPECIFIER,
                    rule
                );

              blockscope {
                Atom(*) atom_evaluated = evaluated;
                if (Do_Any_Array_At_Throws(
                    atom_evaluated,
                    rule,
                    derived
                )){
                    Copy_Cell(OUT, evaluated);
                    goto return_thrown;
                }
                Decay_If_Unstable(atom_evaluated);
              }

                if (Is_Series_Array(P_INPUT)) {
                    REBLEN mod_flags = (P_FLAGS & PF_INSERT) ? 0 : AM_PART;
                    if (not only and Any_Array(evaluated))
                        mod_flags |= AM_SPLICE;

                    // Note: We could check for mutability at the start
                    // of the operation -but- by checking right at the
                    // last minute that allows protects or unprotects
                    // to happen in rule processing if GROUP!s execute.
                    //
                    Array* a = Cell_Array_Ensure_Mutable(ARG(position));
                    P_POS = Modify_Array(
                        a,
                        begin,
                        (P_FLAGS & PF_CHANGE)
                            ? SYM_CHANGE
                            : SYM_INSERT,
                        evaluated,
                        mod_flags,
                        count,
                        1
                    );
                }
                else {
                    P_POS = begin;

                    REBLEN mod_flags = (P_FLAGS & PF_INSERT) ? 0 : AM_PART;

                    P_POS = Modify_String_Or_Binary(  // checks read-only
                        ARG(position),
                        (P_FLAGS & PF_CHANGE)
                            ? SYM_CHANGE
                            : SYM_INSERT,
                        evaluated,
                        mod_flags,
                        count,
                        1
                    );
                }
            }

            if (P_FLAGS & PF_AHEAD)
                P_POS = begin;
        }

        P_FLAGS &= ~PF_STATE_MASK;  // reset any state-oriented flags
        set_or_copy_word = NULL;
    }

    if (Is_Nulled(ARG(position))) {

      next_alternate:

        // If this is just one step, e.g.:
        //
        //     collect x keep some "a" | keep some "b"
        //
        // COLLECT asked for one step, and the first keep asked for one
        // step.  So that second KEEP applies only to some outer collect.
        //
        if (P_FLAGS & PF_ONE_RULE)
            goto return_null;

        if (P_COLLECTION)
            Set_Series_Len(P_COLLECTION, collection_tail);

        FETCH_TO_BAR_OR_END(L);
        if (P_AT_END)  // no alternate rule
            goto return_null;

        // Jump to the alternate rule and reset input
        //
        FETCH_NEXT_RULE(L);
        Copy_Cell(ARG(position), ARG(input));  // P_POS may be null
        begin = P_INPUT_IDX;
    }

    if (P_FLAGS & PF_ONE_RULE)  // don't loop
        goto return_position;

    assert((P_FLAGS & PF_STATE_MASK) == 0);

    begin = P_POS;
    mincount = maxcount = 1;
    goto pre_rule;

  return_position:
    return Init_Integer(OUT, P_POS);  // !!! return switched input series??

  return_null:
    if (not Is_Nulled(ARG(collection)))  // fail -> drop COLLECT additions
      Set_Series_Len(P_COLLECTION, collection_tail);

    return Init_Nulled(OUT);

  return_thrown:
    if (not Is_Nulled(ARG(collection)))  // throw -> drop COLLECT additions
        if (VAL_THROWN_LABEL(LEVEL) != Lib(PARSE_BREAK))  // ...unless
            Set_Series_Len(P_COLLECTION, collection_tail);

    return THROWN;
}


//
//  parse3: native [
//
//  "Parse series according to grammar rules"
//
//      return: "Parse product (return value may be what's passed to ACCEPT)"
//          [any-value?]
//
//      input "Input series to parse"
//          [<maybe> any-series! any-sequence! url!]
//      rules "Rules to parse by"
//          [<maybe> block!]
//      /case "Uses case-sensitive comparison"
//      /redbol "Use Rebol2/Red-style rules vs. UPARSE-style rules"
//  ]
//
DECLARE_NATIVE(parse3)
//
// https://forum.rebol.info/t/1084
{
    INCLUDE_PARAMS_OF_PARSE3;

    REBVAL *input = ARG(input);
    REBVAL *rules = ARG(rules);

    if (Any_Sequence(input)) {
        if (rebRunThrows(
            cast(REBVAL*, SPARE),  // <-- output cell
            Canon(AS), Canon(BLOCK_X), rebQ(input)
        )){
            return THROWN;
        }
        Move_Cell(input, SPARE);
    }
    else if (Is_Url(input)) {
        if (rebRunThrows(
            cast(REBVAL*, SPARE),  // <-- output cell
            Canon(AS), Canon(TEXT_X), input
        )){
            return THROWN;
        }
        Move_Cell(input, SPARE);
    }

    assert(Any_Series(input));

    Element(const*) rules_tail;
    Element(const*) rules_at = Cell_Array_At(&rules_tail, rules);

    // !!! Look for the special pattern `parse ... [collect [x]]` and delegate
    // to a fabricated `parse [temp: collect [x]]` so we can return temp.
    // This hack is just to give a sense of the coming benefit of synthesized
    // parse rules, to help realize why PARSE? vs PARSE vs PARSE* exist.
    if (
        rules_at != rules_tail
        and rules_at + 1 != rules_tail
        and rules_at + 2 == rules_tail
        and Is_Word(rules_at) and Cell_Word_Id(rules_at) == SYM_COLLECT
        and Is_Block(rules_at + 1)
    ){
        Context* frame_ctx = Context_For_Level_May_Manage(level_);
        DECLARE_LOCAL (specific);
        Derelativize(specific, rules_at + 1, P_RULE_SPECIFIER);
        return rebValue(
            "let temp: null",
            "let f: copy @", CTX_ARCHETYPE(frame_ctx),
            "f.rules: [temp: collect", specific, "]",
            "do f",
            "temp"
        );
    }

    if (not Any_Series_Kind(Cell_Heart(input)))
        fail ("PARSE input must be an ANY-SERIES! (use AS BLOCK! for PATH!)");

    Level* sub = Make_Level_At(rules, LEVEL_MASK_NONE);

    bool interrupted;
    if (Subparse_Throws(
        &interrupted,
        FRESHEN(OUT),
        input, SPECIFIED,
        sub,
        nullptr,  // start out with no COLLECT in effect, so no P_COLLECTION
        (REF(case) ? AM_FIND_CASE : 0) | (REF(redbol) ? PF_REDBOL : 0)
        //
        // We always want "case-sensitivity" on binary bytes, vs. treating
        // as case-insensitive bytes for ASCII characters.
    )){
        // Any PARSE-specific THROWs (where a PARSE directive jumped the
        // stack) should be handled here.  ACCEPT is one example.

        Value(const*) label = VAL_THROWN_LABEL(LEVEL);
        if (Is_Frame(label)) {
            if (VAL_ACTION(label) == VAL_ACTION(Lib(PARSE_ACCEPT))) {
                CATCH_THROWN(OUT, LEVEL);
                return OUT;
            }
        }

        return THROWN;
    }

    if (Is_Nulled(OUT)) {  // a match failed (but may be at end of input)
        if (REF(redbol))
            return nullptr;
        return RAISE(Error_Parse_Incomplete_Raw());
    }

    REBLEN index = VAL_UINT32(OUT);
    assert(index <= Cell_Series_Len_Head(input));

    if (index != Cell_Series_Len_Head(input)) {  // didn't reach end of input
        if (REF(redbol))
            return nullptr;
        return RAISE(Error_Parse_Incomplete_Raw());
    }

    // !!! R3-Alpha parse design had no means to bubble up a "synthesized"
    // rule product.  But that's important in the new design.  Hack in support
    // for the single case of when the last rule in the block was <here> and
    // returning the parse position.
    //
    bool strict = true;
    if (
        rules_at != rules_tail  // position on input wasn't []
        and Is_Tag(rules_tail - 1)  // last element processed was a TAG!
        and 0 == CT_String(rules_tail - 1, Root_Here_Tag, strict)  // <here>
    ){
        Copy_Cell(OUT, ARG(input));
        VAL_INDEX_UNBOUNDED(OUT) = index;  // cell guaranteed not REB_QUOTED
        return OUT;
    }

    return TRASH;
}


//
//  parse-accept: native [
//
//  "Accept argument as parse result (Internal Implementation Detail ATM)."
//
//      return: []  ; !!! Notation for divergent function?
//  ]
//
DECLARE_NATIVE(parse_accept)
//
// !!! This was not created for user usage, but rather as a label for the
// internal throw used to indicate "accept".
{
    return RAISE("PARSE-ACCEPT is for internal PARSE use only");
}


//
//  parse-break: native [
//
//  "Break the current parse rule (Internal Implementation Detail ATM)."
//
//      return: []  ; !!! Notation for divergent function?
//  ]
//
DECLARE_NATIVE(parse_break)
//
// !!! This was not created for user usage, but rather as a label for the
// internal throw used to indicate "break".
{
    return RAISE("PARSE-BREAK is for internal PARSE use only");
}


//
//  parse-reject: native [
//
//  "Reject the current parse rule (Internal Implementation Detail ATM)."
//
//      return: []  ; !!! Notation for divergent function?
//  ]
//
DECLARE_NATIVE(parse_reject)
//
// !!! This was not created for user usage, but rather as a label for the
// internal throw used to indicate "reject".
{
    return RAISE("PARSE-REJECT is for internal PARSE use only");
}
