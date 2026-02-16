//
//  file: %needful-sinks.hpp
//  summary: "Contravariant type checking and corruption of output parameters"
//  homepage: <needful homepage TBD>
//
//=/////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2015-2025 hostilefork.com
//
// Licensed under the MIT License
//
// https://en.wikipedia.org/wiki/MIT_License
//
//=/////////////////////////////////////////////////////////////////////////=//
//
// SinkWrapper and InitWrapper accept pointers to BASE classes (writing less-
// constrained bits into a more constrained location is the error to catch).
//
// Some base classes are IsUnsafeSinkBase (e.g. Slot), meaning they might hold
// special bit patterns (like getters/setters) that require explicit handling
// before being overwritten.  We block these UNLESS the source is "fresh"
// material--meaning it comes from an Init() or Sink() wrapper, which
// guarantees no trap patterns are present.
//
// This is captured by a single unified SFINAE alias `IfSafeContra<U, T>`
// that checks: contravariant AND (fresh source OR identity OR not-unsafe
// leaf).
//
//=//// NOTES /////////////////////////////////////////////////////////////=//
//
// A. The copy-and-swap idiom doesn't seem to be very helpful here, as we
//    aren't dealing with exceptions and self-assignment has to be handled
//    manually due to the handoff of the corruption_pending flag.
//
//      https://stackoverflow.com/questions/3279543/
//
// B. While it might seem natural to use a base class to share functions
//    between all the wrappers, this should be limited for debug build
//    performance.  Debug builds don't inline function calls, and the
//    wrappers are used almost always by developers.
//
//    ContraWrapper and InitWrapper DO use inheritance: they differ only in
//    how they handle SinkWrapper sources (preserve vs. squash corruption).
//    SinkWrapper stays standalone because corruption tracking pervades
//    every method, making inheritance save nothing.
//
// C. This file names the macros [NeedfulInit NeedfulSink NeedfulExact]
//    instead of [Init Sink Exact].  This is because those short names are
//    particularly likely to be defined in existing codebases...so you can
//    #define these to whatever name is appropriate for your code.
//
// D. When doing CastHook specializations, you should not use reference
//    types.  See the documentation for CastHook for why decltype() has
//    references removed when matching the specialization, even if the convert
//    function wants to take a reference.
//
// E. In the initial design, default constructing things like SinkWrapper<>
//    was not supported.  But in MSVC it seems that some cases (for instance
//    Option(Sink(bool))) will utilize default construction in mechanics for
//    things like passing nullptr, even when a (SinkWrapper<bool> nullptr_t)
//    constructor exists.
//
//    AI seems to think MSVC is on the right side of the standard and is
//    allowed to require a default constructor.  It's not useless to be
//    able to default construct these types, but they can't give semantic
//    meanings to default construction... C builds couldn't have parity.
//
//       { dont(Corrupt_If_Needful(p)); }  // may be zero in global scope
//
// F. C++ doesn't really let you template cast operators, so if we're going
//    to force contravariant conversions for wrapper types the "loophole"
//    you can use is to do the contravariance testing via construction and
//    then ask the type to cast to void*, and then cast to the type.  It's
//    a workaround that seems to work for wrapper types.
//
// G. You can't safely extract ::type outside of a SFINAE context, so this
//    workaround exposes a static member called `enable` that can be used in
//    a `typename` context.
//


//=//// CONTRAVARIANT CONSTRUCTOR SFINAE //////////////////////////////////=//

template<typename U, typename T>
using IfSafeContra = typename std::enable_if<
    IsContravariant<U, T>::value
    and (IsFreshSource<U>::value
        or std::is_same<typename LeafPointee<U>::type, T>::value
        or not IsUnsafeSinkBase<typename LeafPointee<U>::type>::value
    )>::type;


//=//// CONTRA() FOR LIGHTWEIGHT CONTRAVARIANT PASS-THROUGH ///////////////=//
//
// ContraWrapper is the base contravariant wrapper--it checks contravariant
// type compatibility and stores a pointer, but doesn't manage corruption
// state.  When receiving a SinkWrapper source, it preserves any pending
// corruption (doesn't squash it), since Contra() just passes through the
// right to write without guaranteeing the target will be written.
//
// InitWrapper derives from this, overriding only the SinkWrapper handling
// to squash corruption (since Init() guarantees the target gets written).
//

#undef NeedfulContra
#define NeedfulContra(T)  needful::ContraWrapper<T*>

template<typename TP>
struct ContraWrapper {
    using T = remove_pointer_t<TP>;  // T* is clearer than "TP" in this class

    NEEDFUL_DECLARE_WRAPPED_FIELD (T*, p);

    ContraWrapper() {  // compiler might need, see [E]
        dont(Corrupt_If_Needful(p));  // lightweight behavior vs. Sink()
    }

    ContraWrapper(std::nullptr_t) : p {nullptr}
        {}

    // Generic: raw pointers and wrappers (except SinkWrapper, which
    // needs special handling below to preserve corruption).
    //
    template<typename U, IfSafeContra<U, T>* = nullptr>
    ContraWrapper(const U& u) {
        this->p = x_cast(T*, x_cast(void*, u));  // cast workaround [F]
    }

    ContraWrapper(const ContraWrapper& other) : p {other.p}
        {}

    // SinkWrapper: preserve pending corruption (just passing through)
    //
    template<typename U, IfSafeContra<SinkWrapper<U>, T>* = nullptr>
    ContraWrapper(const SinkWrapper<U>& sink) {
        assert(not sink.corruption_pending);  // catch corruption transfers
        this->p = static_cast<T*>(sink.p);
    }

    ContraWrapper& operator=(std::nullptr_t) {
        this->p = nullptr;
        return *this;
    }

    ContraWrapper& operator=(const ContraWrapper& other) {
        if (this != &other) {
            this->p = other.p;
        }
        return *this;
    }

    template<typename U, IfSafeContra<U, T>* = nullptr>
    ContraWrapper& operator=(const U& u) {
        this->p = x_cast(T*, x_cast(void*, u));
        return *this;
    }

    template<typename U, IfSafeContra<SinkWrapper<U>, T>* = nullptr>
    ContraWrapper& operator=(const SinkWrapper<U>& sink) {
        this->p = static_cast<T*>(sink.p);
        // Intentionally DON'T squash corruption_pending.
        return *this;
    }

    explicit operator bool() const { return p != nullptr; }

    operator T*() const { return p; }

    template<typename U>
    explicit operator U*() const
        { return const_cast<U*>(reinterpret_cast<const U*>(p)); }

    T* operator->() const { return p; }
};


//=//// SINK() WRAPPER FOR OUTPUT PARAMETERS //////////////////////////////=//
//
// 1. The original implementation was simpler, by just doing the corruption
//    at the moment of construction.  But this faced a problem:
//
//        bool some_function(Sink(char*) out, char* in) { ... }
//
//        if (some_function(&ptr, ptr)) { ...}
//
//    If you corrupt the data at the address the sink points to, you can
//    actually be corrupting the value of a stack variable being passed as
//    another argument before it's calculated as an argument.  So deferring
//    the corruption after construction is necessary.  It's a bit tricky
//    in terms of the handoffs and such.
//

#undef NeedfulSink
#define NeedfulSink(T)  needful::SinkWrapper<T*>

template<typename TP>
struct SinkWrapper {
    using T = remove_pointer_t<TP>;  // T* is clearer than "TP" in this class

    NEEDFUL_DECLARE_WRAPPED_FIELD (T*, p);

    mutable bool corruption_pending;  // can't corrupt on construct [1]

    SinkWrapper()  // compiler MIGHT need, see [E]
        : corruption_pending {false}
    {
        Corrupt_If_Needful(p);  // pointer itself, not contents!
    }

    SinkWrapper(std::nullptr_t)
        : p {nullptr},
        corruption_pending {false}
    {
    }

    SinkWrapper(Nocast0Struct)  // for Result(Sink(Element))
        : p {nullptr},
        corruption_pending {false}
    {
    }

    // Generic: raw pointers and wrappers (except cross-type SinkWrapper,
    // which needs special corruption-ownership transfer below).
    //
    template<typename U, IfSafeContra<U, T>* = nullptr>
    SinkWrapper(const U& u) {
        this->p = x_cast(T*, x_cast(void*, u));  // cast workaround [F]
        this->corruption_pending = (this->p != nullptr);
    }

    SinkWrapper(const SinkWrapper& other) {
        this->p = other.p;
        this->corruption_pending = (other.p != nullptr);  // corrupt
        other.corruption_pending = false;  // we take over corrupting
    }

    // Cross-type SinkWrapper: must transfer corruption ownership
    //
    template<typename U, IfSafeContra<SinkWrapper<U>, T>* = nullptr>
    SinkWrapper(const SinkWrapper<U>& other) {
        this->p = reinterpret_cast<T*>(other.p);
        this->corruption_pending = (other.p != nullptr);  // corrupt
        other.corruption_pending = false;  // we take over corrupting
    }

    SinkWrapper& operator=(std::nullptr_t) {
        this->p = nullptr;
        this->corruption_pending = false;
        return *this;
    }

    SinkWrapper& operator=(const SinkWrapper& other) {
        if (this != &other) {
            this->p = other.p;
            this->corruption_pending = (other.p != nullptr);  // corrupt
            other.corruption_pending = false;  // we take over corrupting
        }
        return *this;
    }

    template<typename U, IfSafeContra<U, T>* = nullptr>
    SinkWrapper& operator=(const U& u) {
        this->p = x_cast(T*, x_cast(void*, u));
        this->corruption_pending = (this->p != nullptr);  // corrupt
        return *this;
    }

    explicit operator bool() const { return p != nullptr; }

    operator T*() const {  // corrupt before yielding pointer
        if (corruption_pending) {
            Corrupt_If_Needful(*p);  // corrupt pointed-to item
            corruption_pending = false;
        }
        return p;
    }

    template<typename U>
    explicit operator U*() const {  // corrupt before yielding pointer
        if (corruption_pending) {
            Corrupt_If_Needful(*p);  // corrupt pointed-to item
            corruption_pending = false;
        }
        return const_cast<U*>(reinterpret_cast<const U*>(p));
    }

    T* operator->() const {  // handle corruption before dereference
        if (corruption_pending) {
            Corrupt_If_Needful(*p);  // corrupt pointed-to item
            corruption_pending = false;
        }
        return p;
    }

    ~SinkWrapper() {  // make sure we don't leave scope without corrupting
        if (corruption_pending)
            Corrupt_If_Needful(*p);  // corrupt pointed-to item
    }
};


//=//// HOOK TO CORRUPT *POINTER ITSELF* INSIDE SINK(T) ///////////////////=//
//
// Usually when we think about Sinks and corruption, it's about corrupting
// the pointed-to data.  But sometimes we want to corrupt the pointer itself.
//
//     void Perform_Assignment_Maybe(Sink(int) out, bool assign) {
//         if (not assign)
//             Corrupt_If_Needful(out);  /* corrupt the pointer itself */
//
//         *out = 42;  /* we want unguarded write to crash if not assign */
//     }
//
// (This also would happen if we said UNUSED(out))
//
// The default implementation of Corrupt_if_Debug() would corrupt all the
// bytes in a struct.  We don't want to do that for SinkWrapper<T> because
// it would corrupt the corruption_pending flag itself as well...leading
// to a situation where it might think it needs to corrupt the pointed-to
// data when the pointer itself is actually corrupt...which would crash at
// a seemingly random moment.
//
// So we do just a pointer corruption, and clear the corruption_pending flag
// so it doesn't try to corrupt the pointed-to data at the bad pointer.
//
#if NEEDFUL_USES_CORRUPT_HELPER
    template<typename T>
    struct CorruptHelper<SinkWrapper<T*>&> {  // C pointer corrupt fails
      static void corrupt(SinkWrapper<T*>& wrapper) {
        Corrupt_If_Needful(wrapper.p);  // pointer itself (not contents)
        wrapper.corruption_pending = false;
      }
    };
#endif


//=//// INIT() AS (USUALLY) FAST VARIANT OF SINK() ////////////////////////=//
//
// When we write initialization routines, the output is technically a Sink(),
// in the sense that it's intended to be overwritten.  But Sink() has a cost
// since it corrupts the target.  It's unlikely to help catch bugs with
// initialization, because Init_Xxx() routines are typically not code with
// any branches in it that might fail to overwrite the cell.
//
// InitWrapper derives from ContraWrapper because they are nearly identical--
// the only difference is how they handle a SinkWrapper source:
//
//   ContraWrapper: preserves pending corruption (just passing through)
//   InitWrapper: squashes pending corruption (freshly initializing)
//
// BUT if you want to double check the initializations, it should still work
// to make Init() equivalent to Sink() and corrupt the cell.  It's not likely
// to catch many bugs...but it could, so doing it occasionally might be a
// good idea.  Just do:
//
//     #define DEBUG_CHECK_INIT_SINKS  1  // Init() => actually Sink()
//

#if !defined(DEBUG_CHECK_INIT_SINKS)
    #define DEBUG_CHECK_INIT_SINKS  0
#endif

#if DEBUG_CHECK_INIT_SINKS
    #undef NeedfulInit
    #define NeedfulInit(T)  needful::SinkWrapper<T*>  // see notes above
#else
    #undef NeedfulInit
    #define NeedfulInit(T)  needful::InitWrapper<T*>
#endif

template<typename TP>
struct InitWrapper : ContraWrapper<TP> {
    using Base = ContraWrapper<TP>;
    using T = typename Base::T;

    using Base::Base;  // inherit constructors from ContraWrapper

    InitWrapper() : Base() {}  // not inherited by `using` in C++11

    InitWrapper(const InitWrapper& other) = default;

    template<typename U, IfSafeContra<SinkWrapper<U>, T>* = nullptr>
    InitWrapper(const SinkWrapper<U>& sink) {
        this->p = static_cast<T*>(sink.p);
        sink.corruption_pending = false;  // squash corruption (see above)
    }

    using Base::operator=;

    InitWrapper& operator=(const InitWrapper& other) = default;

    template<typename U, IfSafeContra<SinkWrapper<U>, T>* = nullptr>
    InitWrapper& operator=(const SinkWrapper<U>& sink) {
        this->p = static_cast<T*>(sink.p);
        sink.corruption_pending = false;  // squash corruption (see above)
        return *this;
    }
};


//=//// EXACT() FOR FORBIDDING COVARIANT INPUT PARAMETERS /////////////////=//
//
// Exact() prohibits covariance, but but unlike Sink() or Init() it doesn't
// imply corruption, so contravariance doesn't make sense.  It just enforces
// that only the exact type is used.
//
// NOTE: The code below might seem overcomplicated for that stated purpose,
// and simplifications are welcome!  But it has some interoperability with
// Sink() and Init() as well as fitting into Needful's casting framework.
// So it's more complex than a minimal C++11 Exact() implementation would be.
//
// 1. While Sink(T) and Init(T) implicitly add pointers to the type, you have
//    to say Exact(T*) if it's a pointer.  This allows you to use Exact
//    with non-pointer types.
//
//    However, the template -must- be parameterized with the type it is a
//    stand-in for, so it is `SinkWrapper<T*>`, `InitWrapper<T*>`, and
//    `ExactWrapper<T*>`.
//
//    (See needful_rewrap_type() for the reasoning behind this constraint.)
//
// 2. Uses in the codebase the Needful library were written for required that
//    Exact(T*) be able to accept cells with pending corruptions.  I guess
//    the thing I would say that if you want to argue with this design point,
//    you should consider that there's nothing guaranteeing a plain `T*` is
//    not corrupt...so you're not bulletproofing much and breaking some uses
//    that turned out to be important.  It's better to have cross-cutting
//    ways at runtime to notice a given T* is corrupt regardless of Exact().
//
// 3. Non-dependent enable_if conditions work in MSVC, but GCC has trouble
//    with them.  Introducing a dependent type seems to help it along.
//

#undef NeedfulExact
#define NeedfulExact(TP) \
    needful::ExactWrapper<TP>  // * not implicit [1]

template<typename U, typename T>
struct IfExactType2 {
    static constexpr bool value = std::is_same<U, T>::value;
    using enable = typename std::enable_if<value>;  // not ::type [1]
};

template<typename TP>  // TP may or may not be a pointer type
struct ExactWrapper {
    NEEDFUL_DECLARE_WRAPPED_FIELD (TP, p);

    using MTP = needful_unconstify_t(TP);  // mutable type

    using T = remove_pointer_t<MTP>;

    template<typename U>
    using IfExactType
        = typename IfExactType2<needful_unconstify_t(U), T>::enable::type;

    ExactWrapper() = default;  // compiler MIGHT need, don't corrupt [E]

    ExactWrapper(std::nullptr_t) : p {nullptr}
        {}

    template<
        typename U,
        typename D = TP,  // [3]
        typename = enable_if_t<
            std::is_pointer<D>::value
        >,
        IfExactType<U>* = nullptr
    >
    ExactWrapper(U* u) : p {x_cast(TP, u)}
        {}

    template<
        typename UP,
        typename D = TP,  // [3]
        typename = enable_if_t<
            not std::is_pointer<D>::value
        >,
        IfExactType<UP>* = nullptr
    >
    ExactWrapper(UP u) : p {u}
        {}

    template<
        typename U,
        typename D = TP,  // [3]
        typename = enable_if_t<
            std::is_pointer<D>::value
        >,
        IfExactType<U>* = nullptr
    >
    ExactWrapper(const ExactWrapper<U*>& other)
        : p {other.p}
        {}

    template<
        typename UP,
        typename D = TP,  // [3]
        typename = enable_if_t<
            not std::is_pointer<D>::value
        >,
        IfExactType<UP>* = nullptr
    >
    ExactWrapper(const ExactWrapper<UP>& other)
        : p {other.p}
        {}

    ExactWrapper(const ExactWrapper& other) : p {other.p}
        {}

    template<typename U, IfExactType<U>* = nullptr>
    ExactWrapper(const SinkWrapper<U*>& sink)
        : p {static_cast<TP>(sink)}
    {
        dont(assert(not sink.corruption_pending));  // must allow corrupt [2]
    }

    template<typename U, IfExactType<U>* = nullptr>
    ExactWrapper(const InitWrapper<U*>& init)
        : p {static_cast<TP>(init.p)}
        {}

    template<typename U, IfExactType<U>* = nullptr>
    ExactWrapper(const ContraWrapper<U*>& contra)
        : p {static_cast<TP>(contra.p)}
        {}

    ExactWrapper& operator=(std::nullptr_t) {
        this->p = nullptr;
        return *this;
    }

    ExactWrapper& operator=(const ExactWrapper& other) {
        if (this != &other) {
            this->p = other.p;
        }
        return *this;
    }

    template<typename U, IfExactType<U>* = nullptr>
    ExactWrapper& operator=(U* ptr) {
        this->p = static_cast<TP>(ptr);
        return *this;
    }

    template<typename U, IfExactType<U>* = nullptr>
    ExactWrapper& operator=(const SinkWrapper<U*>& sink) {
        dont(assert(not sink.corruption_pending));  // must allow corrupt [2]
        this->p = static_cast<TP>(sink);  // not sink.p (flush corruption)
        return *this;
    }

    template<typename U, IfExactType<U>* = nullptr>
    ExactWrapper& operator=(const InitWrapper<U*>& init) {
        this->p = static_cast<TP>(init.p);
        return *this;
    }

    template<typename U, IfExactType<U>* = nullptr>
    ExactWrapper& operator=(const ContraWrapper<U*>& contra) {
        this->p = static_cast<TP>(contra.p);
        return *this;
    }

    explicit operator bool() const { return p != nullptr; }

    operator TP() const { return p; }

    template<typename U>
    explicit operator U*() const
        { return const_cast<U*>(reinterpret_cast<const U*>(p)); }

    TP operator->() const { return p; }
};
