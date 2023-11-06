//
//  File: %sys-action.h
//  Summary: {action! defs AFTER %tmp-internals.h (see: %sys-rebact.h)}
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012-2021 Ren-C Open Source Contributors
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
// As in historical Rebol, Ren-C has several different kinds of functions...
// each of which have a different implementation path inside the system.
// But in Ren-C there is only one user-visible datatype from the user's
// perspective for all of them, which is called ACTION!.
//
// Each ACTION! has an associated C function that runs when it is invoked, and
// this is called the "dispatcher".  A dispatcher may be general and reused
// by many different actions.  For example: the same dispatcher code is used
// for most `FUNC [...] [...]` instances--but each one has a different body
// array and spec, so the behavior is different.  Other times a dispatcher can
// be for a single function, such as with natives like IF that have C code
// which is solely used to implement IF.
//
// The identity array for an action is called its "details".  It has an
// archetypal value for the ACTION! in its [0] slot, but the other slots are
// dispatcher-specific.  Different dispatchers lay out the details array with
// different values that define the action instance.
//
// Some examples:
//
//     USER FUNCTIONS: 1-element array w/a BLOCK!, the body of the function
//     GENERICS: 1-element array w/WORD! "verb" (OPEN, APPEND, etc)
//     SPECIALIZATIONS: no contents needed besides the archetype
//     ROUTINES/CALLBACKS: stylized array (REBRIN*)
//     TYPECHECKERS: the TYPESET! to check against
//
// (See the comments in the %src/core/functionals/ directory for each function
// variation for descriptions of how they use their details arrays.)
//
// Every action has an associated context known as the "exemplar" that defines
// the parameters and locals.  The keylist of this exemplar is reused for
// FRAME! instances of invocations (or pending invocations) of the action.
//
// The varlist of the exemplar context is referred to as a "paramlist".  It
// is an array that serves two overlapping purposes: any *unspecialized*
// slots in the paramlist holds the TYPESET! definition of legal types for
// that argument, as well as the PARAM_FLAG_XXX for other properties of the
// parameter.  But a *specialized* parameter slot holds the specialized value
// itself, which is presumed to have been type-checked upon specialization.
//
//=////////////////////////////////////////////////////////////////////////=//
//
// NOTES:
//
// * Unlike contexts, an ACTION! does not have values of its own, only
//   parameter definitions (or "params").  The arguments ("args") come from an
//   action's instantiation on the stack, viewed as a context using a FRAME!.
//
// * Paramlists may contain hidden fields, if they are specializations...
//   because they have to have the right number of slots to line up with the
//   frame of the underlying function.
//
// * The `misc.meta` field of the details holds a meta object (if any) that
//   describes the function.  This is read by help.  A similar facility is
//   enabled by the `misc.meta` field of varlists.
//
// * By storing the C function dispatcher pointer in the `details` array node
//   instead of in the value cell itself, it also means the dispatcher can be
//   HIJACKed--or otherwise hooked to affect all instances of a function.
//


// Context types use this field of their varlist (which is the identity of
// an ANY-CONTEXT!) to find their "keylist".  It is stored in the REBSER
// node of the varlist Array(*) vs. in the REBVAL of the ANY-CONTEXT! so
// that the keylist can be changed without needing to update all the
// REBVALs for that object.
//
// It may be a simple REBSER* -or- in the case of the varlist of a running
// FRAME! on the stack, it points to a Frame(*).  If it's a FRAME! that
// is not running on the stack, it will be the function paramlist of the
// actual phase that function is for.  Since Frame(*) all start with a
// REBVAL cell, this means NODE_FLAG_CELL can be used on the node to
// discern the case where it can be cast to a Frame(*) vs. Array(*).
//
// (Note: FRAME!s used to use a field `misc.f` to track the associated
// frame...but that prevented the ability to SET-ADJUNCT on a frame.  While
// that feature may not be essential, it seems awkward to not allow it
// since it's allowed for other ANY-CONTEXT!s.  Also, it turns out that
// heap-based FRAME! values--such as those that come from MAKE FRAME!--
// have to get their keylist via the specifically applicable ->phase field
// anyway, and it's a faster test to check this for NODE_FLAG_CELL than to
// separately extract the CTX_TYPE() and treat frames differently.)
//
// It is done as a base-class Node* as opposed to a union in order to
// not run afoul of C's rules, by which you cannot assign one member of
// a union and then read from another.
//
#define BONUS_KeySource_TYPE        Node*
#define BONUS_KeySource_CAST        // none, just use node (NOD() complains)
#define HAS_BONUS_KeySource         FLAVOR_VARLIST

inline static void INIT_BONUS_KEYSOURCE(Array(*) varlist, Node* keysource) {
    if (keysource != nullptr and Is_Node_A_Stub(keysource))
        assert(IS_KEYLIST(SER(keysource)));
    mutable_BONUS(KeySource, varlist) = keysource;
}


//=//// PSEUDOTYPES FOR RETURN VALUES /////////////////////////////////////=//
//
// An arbitrary cell pointer may be returned from a native--in which case it
// will be checked to see if it is thrown and processed if it is, or checked
// to see if it's an unmanaged API handle and released if it is...ultimately
// putting the cell into f->out.
//
// However, pseudotypes can be used to indicate special instructions to the
// evaluator.
//

inline static REBVAL *Init_Return_Signal_Untracked(Cell(*) out, char ch) {
    Reset_Unquoted_Header_Untracked(
        out,
        FLAG_HEART_BYTE(REB_T_RETURN_SIGNAL) | CELL_MASK_NO_NODES
    );
    mutable_BINDING(out) = nullptr;

    PAYLOAD(Any, out).first.u = ch;
  #ifdef ZERO_UNUSED_CELL_FIELDS
    PAYLOAD(Any, out).second.trash = ZEROTRASH;
  #endif
    return cast(REBVAL*, out);
}

#define Init_Return_Signal(out,ch) \
    TRACK(Init_Return_Signal_Untracked((out), (ch)))

inline static bool Is_Bounce_An_Atom(Bounce b)
  { return HEART_BYTE(cast(REBVAL*, b)) != REB_T_RETURN_SIGNAL; }

inline static char VAL_RETURN_SIGNAL(Bounce b) {
    assert(not Is_Bounce_An_Atom(b));
    return PAYLOAD(Any, cast(REBVAL*, b)).first.u;
}

inline static Atom(*) Atom_From_Bounce(Bounce b) {
    assert(Is_Bounce_An_Atom(b));
    return cast(Atom(*), b);
}


// This signals that the evaluator is in a "thrown state".
//
#define C_THROWN 'T'
#define BOUNCE_THROWN \
    cast(Bounce, &PG_R_Thrown)

inline static bool Is_Throwing(Frame(*) frame_) {
    //
    // !!! An original constraint on asking if something was throwing was
    // that only the top frame could be asked about.  But Action_Executor()
    // is called to re-dispatch when there may be a frame above (kept there
    // by request from something like REDUCE).  We relax the constraint to
    // only be able to return *true* to a throw request if there are no
    // frames above on the stack.
    //
    if (not Is_Cell_Erased(&TG_Thrown_Arg)) {
        /*assert(frame_ == TOP_FRAME);*/  // forget even that check
        UNUSED(frame_);  // currently only used for debug build check
        return true;
    }
    return false;
}

#define THROWING Is_Throwing(frame_)


// If Eval_Core gets back an REB_R_REDO from a dispatcher, it will re-execute
// the f->phase in the frame.  This function may be changed by the dispatcher
// from what was originally called.
//
// If EXTRA(Any).flag is not set on the cell, then the types will be checked
// again.  Note it is not safe to let arbitrary user code change values in a
// frame from expected types, and then let those reach an underlying native
// who thought the types had been checked.
//
#define C_REDO_UNCHECKED 'r'
#define BOUNCE_REDO_UNCHECKED \
    cast(Bounce, &PG_R_Redo_Unchecked)

#define C_REDO_CHECKED 'R'
#define BOUNCE_REDO_CHECKED \
    cast(Bounce, &PG_R_Redo_Checked)


// Continuations are used to mitigate the problems that occur when the C stack
// contains a mirror of frames corresponding to the frames for each stack
// level.  Avoiding this means that routines that would be conceived as doing
// a recursion instead return to the evaluator with a new request.  This helps
// avoid crashes from C stack overflows and has many other advantages.  For a
// similar approach and explanation, see:
//
// https://en.wikipedia.org/wiki/Stackless_Python
//
#define C_CONTINUATION 'C'
#define BOUNCE_CONTINUE \
    cast(Bounce, &PG_R_Continuation)


// A dispatcher may want to run a "continuation" but not be called back.
// This is referred to as delegation.
//
#define C_DELEGATION 'D'
#define BOUNCE_DELEGATE \
    cast(Bounce, &PG_R_Delegation)

#define DELEGATE_255 255

// For starters, a simple signal for suspending stacks in order to be able to
// try not using Asyncify (or at least not relying on it so heavily)
//
#define C_SUSPEND 'S'
#define BOUNCE_SUSPEND \
    cast(Bounce, &PG_R_Suspend)


#define INIT_VAL_ACTION_DETAILS                         INIT_VAL_NODE1
#define VAL_ACTION_PARTIALS_OR_LABEL(v)                 SER(VAL_NODE2(v))
#define INIT_VAL_ACTION_PARTIALS_OR_LABEL               INIT_VAL_NODE2


#define ACT_IDENTITY(action) \
    cast(Array(*), ensure(Action(*), (action)))


// An action's "archetype" is data in the head cell (index [0]) of the array
// that is the paramlist.  This is an ACTION! cell which must have its
// paramlist value match the paramlist it is in.  So when copying one array
// to make a new paramlist from another, you must ensure the new array's
// archetype is updated to match its container.
//
// Note that the details array represented by the identity is not guaranteed
// to be SERIES_FLAG_DYNAMIC, so we use SER_AT() here vs. optimize further.
//
#define ACT_ARCHETYPE(action) \
    SER_AT(REBVAL, ACT_IDENTITY(action), 0)


// An action's details array is stored in the archetype, which is the first
// element of the action array...which is *usually* the same thing as the
// action array itself, -but not always-.  Hijackings fiddle with this, and
// a COPY of an action will get the details array of what it copied...not
// itself.  So an archetype represents -an- action, but it may be a hijacked
// action from what it once was (much like a word reference).
//
#define ACT_DETAILS(action) \
    x_cast(Array(*), ACT_ARCHETYPE(action)->payload.Any.first.node)

inline static Context(*) VAL_ACTION_BINDING(noquote(Cell(const*)) v) {
    assert(CELL_HEART_UNCHECKED(v) == REB_ACTION);
    return CTX(BINDING(v));
}

inline static void INIT_VAL_ACTION_BINDING(
    Cell(*) v,
    Context(*) binding
){
    assert(CELL_HEART_UNCHECKED(v) == REB_ACTION);
    mutable_BINDING(v) = binding;
}


// Only the archetype should be asked if it is native (because the archetype
// guides interpretation of the details array).
//
#define Is_Action_Native(a) \
    Get_Action_Flag(VAL_ACTION(ACT_ARCHETYPE(a)), IS_NATIVE)


//=//// PARAMLIST, EXEMPLAR, AND PARTIALS /////////////////////////////////=//
//
// Since partial specialization is somewhat rare, it is an optional splice
// before the place where the exemplar is to be found.
//

#define INODE_Exemplar_TYPE     Context(*)
#define INODE_Exemplar_CAST     CTX
#define HAS_INODE_Exemplar      FLAVOR_DETAILS


inline static option(Array(*)) ACT_PARTIALS(Action(*) a) {
    return ARR(VAL_NODE2(ACT_ARCHETYPE(a)));
}

#define ACT_EXEMPLAR(a) \
    INODE(Exemplar, (a))

// Note: This is a more optimized version of CTX_KEYLIST(ACT_EXEMPLAR(a)),
// and also forward declared.
//
#define ACT_KEYLIST(a) \
    cast(Raw_Keylist*, BONUS(KeySource, ACT_EXEMPLAR(a)))

#define ACT_KEYS_HEAD(a) \
    SER_HEAD(const REBKEY, ACT_KEYLIST(a))

#define ACT_KEYS(tail,a) \
    CTX_KEYS((tail), ACT_EXEMPLAR(a))

#define ACT_PARAMLIST(a)            CTX_VARLIST(ACT_EXEMPLAR(a))

inline static REBPAR *ACT_PARAMS_HEAD(Action(*) a) {
    Array(*) list = CTX_VARLIST(ACT_EXEMPLAR(a));
    return cast(REBPAR*, list->content.dynamic.data) + 1;  // skip archetype
}

#define LINK_DISPATCHER(a)              cast(Dispatcher*, (a)->link.any.cfunc)
#define mutable_LINK_DISPATCHER(a)      (a)->link.any.cfunc

#define ACT_DISPATCHER(a) \
    LINK_DISPATCHER(ACT_IDENTITY(a))

#define INIT_ACT_DISPATCHER(a,cfunc) \
    mutable_LINK_DISPATCHER(ACT_IDENTITY(a)) = cast(CFUNC*, (cfunc))


// The DETAILS array isn't guaranteed to be SERIES_FLAG_DYNAMIC (it may hold
// only the archetype, e.g. with a specialized function).  *BUT* if you are
// asking for elements in the details array, you must know it is dynamic.
//
inline static Value(*) DETAILS_AT(Array(*) details, Length n) {
    assert(n != 0 and n < details->content.dynamic.used);
    return cast(Value(*), details->content.dynamic.data) + n;
}

#define IDX_DETAILS_1 1  // Common index used for code body location

// These are indices into the details array agreed upon by actions which have
// the PARAMLIST_FLAG_IS_NATIVE set.
//
enum {
    // !!! Originally the body was introduced as a feature to let natives
    // specify "equivalent usermode code".  As the types of natives expanded,
    // it was used for things like storing the text source of C user natives...
    // or the "verb" WORD! of a "generic" (like APPEND).  So ordinary natives
    // just store blank here, and the usages are sometimes dodgy (e.g. a user
    // native checks to see if it's a user native if this is a TEXT!...which
    // might collide with other natives in the future).  The idea needs review.
    //
    IDX_NATIVE_BODY = 1,

    IDX_NATIVE_CONTEXT,  // libRebol binds strings here (and lib)

    IDX_NATIVE_MAX
};

enum {
    IDX_INTRINSIC_CFUNC = 1,
    IDX_INTRINSIC_MAX
};


#define KEY_SLOT(dsp)       Data_Stack_At((dsp) - 3)
#define PARAM_SLOT(dsp)     Data_Stack_At((dsp) - 2)
#define TYPES_SLOT(dsp)     Data_Stack_At((dsp) - 1)
#define NOTES_SLOT(dsp)     Data_Stack_At(dsp)

#define PUSH_SLOTS() \
    do { PUSH(); PUSH(); PUSH(); PUSH(); } while (0)


inline static Symbol(const*) KEY_SYMBOL(const REBKEY *key)
  { return *key; }


inline static void Init_Key(REBKEY *dest, const Raw_Symbol* symbol)
  { *dest = symbol; }

#define KEY_SYM(key) \
    ID_OF_SYMBOL(KEY_SYMBOL(key))

#define ACT_KEY(a,n)            CTX_KEY(ACT_EXEMPLAR(a), (n))
#define ACT_PARAM(a,n)          cast_PAR(CTX_VAR(ACT_EXEMPLAR(a), (n)))

#define ACT_NUM_PARAMS(a) \
    CTX_LEN(ACT_EXEMPLAR(a))


//=//// META OBJECT ///////////////////////////////////////////////////////=//
//
// ACTION! details and ANY-CONTEXT! varlists can store a "meta" object.  It's
// where information for HELP is saved, and it's how modules store out-of-band
// information that doesn't appear in their body.

#define mutable_ACT_ADJUNCT(a)     mutable_MISC(DetailsAdjunct, ACT_IDENTITY(a))
#define ACT_ADJUNCT(a)             MISC(DetailsAdjunct, ACT_IDENTITY(a))


inline static Action(*) VAL_ACTION(noquote(Cell(const*)) v) {
    assert(CELL_HEART_UNCHECKED(v) == REB_ACTION);
    REBSER *s = SER(VAL_NODE1(v));
    if (GET_SERIES_FLAG(s, INACCESSIBLE))
        fail (Error_Series_Data_Freed_Raw());
    return ACT(s);
}

#define VAL_ACTION_KEYLIST(v) \
    ACT_KEYLIST(VAL_ACTION(v))


//=//// ACTION LABELING ///////////////////////////////////////////////////=//
//
// When an ACTION! is stored in a cell (e.g. not an "archetype"), it can
// contain a label of the ANY-WORD! it was taken from.  If it is an array
// node, it is presumed an archetype and has no label.
//
// !!! Theoretically, longer forms like `.not.equal?` for PREDICATE! could
// use an array node here.  But since CHAINs store ACTION!s that can cache
// the words, you get the currently executing label instead...which may
// actually make more sense.

inline static option(Symbol(const*)) VAL_ACTION_LABEL(noquote(Cell(const*)) v) {
    assert(CELL_HEART(v) == REB_ACTION);
    REBSER *s = VAL_ACTION_PARTIALS_OR_LABEL(v);
    if (not s)
        return ANONYMOUS;
    if (IS_SER_ARRAY(s))
        return ANONYMOUS;  // archetype (e.g. may live in paramlist[0] itself)
    return SYM(s);
}

inline static void INIT_VAL_ACTION_LABEL(
    Cell(*) v,
    option(Symbol(const*)) label
){
    ASSERT_CELL_WRITABLE_EVIL_MACRO(v);  // archetype R/O
    if (label)
        INIT_VAL_ACTION_PARTIALS_OR_LABEL(v, unwrap(label));
    else
        INIT_VAL_ACTION_PARTIALS_OR_LABEL(v, ANONYMOUS);
}


//=//// ANCESTRY / FRAME COMPATIBILITY ////////////////////////////////////=//
//
// On the keylist of an object, LINK_ANCESTOR points at a keylist which has
// the same number of keys or fewer, which represents an object which this
// object is derived from.  Note that when new object instances are
// created which do not require expanding the object, their keylist will
// be the same as the object they are derived from.
//
// Paramlists have the same relationship, with each expansion (e.g. via
// AUGMENT) having larger frames pointing to the potentially shorter frames.
// (Something that reskins a paramlist might have the same size frame, with
// members that have different properties.)
//
// When you build a frame for an expanded action (e.g. with an AUGMENT) then
// it can be used to run phases that are from before it in the ancestry chain.
// This informs low-level asserts inside of the specific binding machinery, as
// well as determining whether higher-level actions can be taken (like if a
// sibling tail call would be legal, or if a certain HIJACK would be safe).
//
// !!! When ancestors were introduced, it was prior to AUGMENT and so frames
// did not have a concept of expansion.  So they only applied to keylists.
// The code for processing derivation is slightly different; it should be
// unified more if possible.

#define LINK_Ancestor_TYPE              Raw_Keylist*
#define LINK_Ancestor_CAST              KEYS
#define HAS_LINK_Ancestor               FLAVOR_KEYLIST

inline static bool Action_Is_Base_Of(Action(*) base, Action(*) derived) {
    if (derived == base)
        return true;  // fast common case (review how common)

    if (ACT_DETAILS(derived) == ACT_IDENTITY(base))
        return true;  // Covers COPY + HIJACK cases (seemingly)

    REBSER *keylist_test = ACT_KEYLIST(derived);
    REBSER *keylist_base = ACT_KEYLIST(base);
    while (true) {
        if (keylist_test == keylist_base)
            return true;

        REBSER *ancestor = LINK(Ancestor, keylist_test);
        if (ancestor == keylist_test)
            return false;  // signals end of the chain, no match found

        keylist_test = ancestor;
    }
}


//=//// RETURN HANDLING (WIP) /////////////////////////////////////////////=//
//
// The well-understood and working part of definitional return handling is
// that function frames have a local slot named RETURN.  This slot is filled
// by the dispatcher before running the body, with a function bound to the
// executing frame.  This way it knows where to return to.
//
// !!! Lots of other things are not worked out (yet):
//
// * How do function derivations share this local cell (or do they at all?)
//   e.g. if an ADAPT has prelude code, that code runs before the original
//   dispatcher would fill in the RETURN.  Does the cell hold a return whose
//   phase meaning changes based on which phase is running (which the user
//   could not do themselves)?  Or does ADAPT need its own RETURN?  Or do
//   ADAPTs just not have returns?
//
// * The typeset in the RETURN local key is where legal return types are
//   stored (in lieu of where a parameter would store legal argument types).
//   Derivations may wish to change this.  Needing to generate a whole new
//   paramlist just to change the return type seems excessive.
//
// * To make the position of RETURN consistent and easy to find, it is moved
//   to the first parameter slot of the paramlist (regardless of where it
//   is declared).  This complicates the paramlist building code, and being
//   at that position means it often needs to be skipped over (e.g. by a
//   GENERIC which wants to dispatch on the type of the first actual argument)
//   The ability to create functions that don't have a return complicates
//   this mechanic as well.
//
// The only bright idea in practice right now is that parameter lists which
// have a definitional return in the first slot have a flag saying so.  Much
// more design work on this is needed.
//

#define ACT_HAS_RETURN(a) \
    Get_Subclass_Flag(VARLIST, ACT_PARAMLIST(a), PARAMLIST_HAS_RETURN)


// A fully constructed action can reconstitute the ACTION! REBVAL
// that is its canon form from a single pointer...the REBVAL sitting in
// the 0 slot of the action's details.  That action has no binding and
// no label.
//
inline static REBVAL *Init_Action_Core(
    Cell(*) out,
    Action(*) a,
    option(Symbol(const*)) label,  // allowed to be ANONYMOUS
    Context(*) binding  // allowed to be UNBOUND
){
  #if !defined(NDEBUG)
    Extra_Init_Action_Checks_Debug(a);
  #endif
    Force_Series_Managed(ACT_IDENTITY(a));

    Reset_Unquoted_Header_Untracked(out, CELL_MASK_ACTION);
    INIT_VAL_ACTION_DETAILS(out, ACT_IDENTITY(a));
    INIT_VAL_ACTION_LABEL(out, label);
    INIT_VAL_ACTION_BINDING(out, binding);

    return cast(REBVAL*, out);
}

#define Init_Action(out,a,label,binding) \
    TRACK(Init_Action_Core((out), (a), (label), (binding)))


enum {
    ST_ACTION_INITIAL_ENTRY = STATE_0,

    ST_ACTION_FULFILLING_ARGS = 100,  // weird number if dispatcher gets it

    // Using the state byte to convey the next argument should come from OUT
    // serves an additional purpose, because STATE_0 would mean that OUT has
    // to be stale.  This allows the caller to subvert that rule as well as
    // have the enfix-from-out signal without needing a separate flag.
    //
    ST_ACTION_FULFILLING_ENFIX_FROM_OUT,

    // While some special-purpose functions intentionally receive nihil, most
    // don't want to...so we can treat it as an expression barrier--whether
    // it's produced by a COMMA! evaluating to nothing, or otherwise.
    //
    //    foo: func [x [integer! <end>]] [...]
    //
    //    (foo,)  ; sees an end
    //    (foo comment "hi")  ; also sees an end
    //
    //    bar: func [^y [nihil? integer!]] [...]
    //
    //    (bar,)  ; sees an empty pack isotope (nihil) ~[]~
    //    (bar comment "hi")  ; same
    //
    ST_ACTION_BARRIER_HIT,

    ST_ACTION_TYPECHECKING

    // Note: There is no ST_ACTION_DISPATCHING, because if an action is
    // dispatching, the STATE byte belongs to the dispatcher.  Detecting the
    // state of being in dispatch is (`key` == `key_tail`), which tells you
    // that argument enumeration is finished.
};


inline static REBVAL *Maybe_Move_Cell(REBVAL *out, REBVAL *v) {
    if (v == out)
        return out;
    return Move_Cell(out, v);
}

inline static Bounce Native_Thrown_Result(Frame(*) frame_) {
    assert(THROWING);
    FRESHEN(frame_->out);
    return BOUNCE_THROWN;
}


inline static Bounce Native_Void_Result_Untracked(
    Atom(*) out,  // have to pass; comma at callsite -> "operand has no effect"
    Frame(*) frame_
){
    assert(out == frame_->out);
    UNUSED(out);
    assert(not THROWING);
    return Init_Void_Untracked(frame_->out, UNQUOTED_1);
}

inline static Bounce Native_Unmeta_Result(Frame(*) frame_, const REBVAL *v) {
    assert(not THROWING);
    return Meta_Unquotify_Undecayed(Copy_Cell(frame_->out, v));
}

inline static Bounce Native_None_Result_Untracked(
    Atom(*) out,  // have to pass; comma at callsite -> "operand has no effect"
    Frame(*) frame_
){
    assert(out == frame_->out);
    UNUSED(out);
    assert(not THROWING);
    return Init_Void_Untracked(frame_->out, ISOTOPE_0);
}


//=//// ACTIVATIONS (ACTION! Isotopes) /////////////////////////////////////=//
//
// Isotopic forms of actions exist for a couple of reasons.  They are the form
// that when stored in a variable leads to implicit execution by a reference
// from a WORD!...while non-isotopic ACTION! is inert.  This means you cannot
// accidentally run a function with the following code:
//
//     for-each item block [print ["The item's kind is" kind of item]]
//
// That reference to ITEM is guaranteed to not be the isotopic form, since it
// is enumerating over a block.  Various places in the system are geared for
// making it more difficult to assign isotopic actions accidentally.
//
// The other big reason is for a "non-literal" distinction in parameters.
// Historically, functions like REPLACE have chosen to run functions to
// calculate what the replacement should be.  However, that ruled out the
// ability to replace actual function instances--and doing otherwise would
// require extra parameterization.  This lets the isotopic state serve as
// the signal that the function should be invoked, and not searched for.
//

#define Init_Activation(out,a,label,binding) \
    Activatify(Init_Action_Core(TRACK(out), (a), (label), (binding)))

inline static Value(*) Activatify(Value(*) v) {
    assert(IS_ACTION(v) and QUOTE_BYTE(v) == UNQUOTED_1);
    mutable_QUOTE_BYTE(v) = ISOTOPE_0;
    return VAL(v);
}

inline static Cell(*) Deactivate_If_Activation(Cell(*) v) {
    if (Is_Activation(v))
        mutable_QUOTE_BYTE(v) = UNQUOTED_1;
    return v;
}
