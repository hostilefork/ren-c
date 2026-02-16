//
//  file: %needful-contra.hpp
//  summary: "Contravariant type checking and corruption of output parameters"
//  homepage: <needful homepage TBD>
//
//=/////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2015-2026 hostilefork.com
//
// Licensed under the MIT License
//
// https://en.wikipedia.org/wiki/MIT_License
//
//=/////////////////////////////////////////////////////////////////////////=//
//
// Needful's concept of contravariance is based on a very stylized usage of
// inheritance, in which classes in a derivation hierarchy are all using the
// same underlying bit patterns.  The only reason they're using inheritance
// is to get compile-time checking of constraints on those bits, where the
// subclasses represent more constrained bit patterns than their bases.
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
// C. This file names the macros [NeedfulContra NeedfulSink NeedfulInit]
//    instead of [Contra Sink Init].  This is because those short names are
//    particularly likely to be defined in existing codebases...so you can
//    #define these to whatever name is appropriate for your code.
//
// D. In the initial design, default constructing things like SinkWrapper<>
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
// E. C++ doesn't really let you template cast operators, so if we're going
//    to force contravariant conversions for wrapper types the "loophole"
//    you can use is to do the contravariance testing via construction and
//    then ask the type to cast to void*, and then cast to the type.  It's
//    a workaround that seems to work for wrapper types.
//


//=//// STRICTER BASE CHECKING ////////////////////////////////////////////=//
//
// The stylized contravariance needs Plain-Old-Data (POD) C structs, that are
// standard-layout where no fields are added in derivation.  This is the only
// way that the "dangerous"-looking casts performed by Sink() and Init() are
// safe.  So we check for standard-layout and size-equality on base and
// derived classes before allowing them to be used this way.
//

template<typename B, typename D, typename = void>
struct IsSameLayoutBase : std::false_type {};

template<typename B, typename D>  // stricter version of is_base_of<>
struct IsSameLayoutBase<B, D,
    typename std::enable_if<
        std::is_base_of<B, D>::value
    >::type>
{
    static_assert(
        std::is_standard_layout<B>::value
            and std::is_standard_layout<D>::value
            and sizeof(B) == sizeof(D),
        "IsSameLayoutBase: types must be same-sized standard layout classes"
    );
    static constexpr bool value = std::is_base_of<B, D>::value;
};


//=//// CONTRAVARIANT TRAIT ///////////////////////////////////////////////=//
//
// 1. Sink(T) and Init(T) want to enable contravariant conversions for
//    wrapped types, but only "safe" wrappers.
//
//    An example unsafe wrapper would be Option(T), because Sink(Option(T))
//    would run the risk of trying to write bytes into a nullptr disengaged
//    state.  However, this is really the exception and not the rule: Needful
//    wrappers are just providing some debug instrumentation and no function,
//    which means that nullability is the *only* property to worry about.
//
//    So we default to saying wrappers are contravariant, and really Option(T)
//    is the only known exception at this time.
//
// 2. We Recursively unwrap if W is also a wrapper, to handle double-wrapping
//    like OnStackPointer<InitWrapper<Stable*>> -> InitWrapper<Element*>
//

template<typename T>  // assume wrappers are contravariant by default [1]
struct IsContravariantWrapper : std::true_type {};

template<
    typename UP,
    typename T,
    bool IsWrapper = HasWrappedType<UP>::value  // default: not a wrapper [1]
>
struct IsContravariant {
    using U = remove_pointer_t<UP>;  // Note: UP may or may not be a pointer

    static constexpr bool value =
        std::is_same<UP, T*>::value or (
            std::is_pointer<UP>::value and std::is_class<T>::value
                ? IsSameLayoutBase<U, T>::value
                : false
        );
};

template<typename U, typename T>
struct IsContravariant<U, T, /* bool IsWrapper = */ true> {
    using WP = typename U::wrapped_type;
    using W = remove_pointer_t<WP>;

    static constexpr bool value =  // recursively unwrap [2]
        IsContravariantWrapper<U>::value
        and (HasWrappedType<W>::value
            ? IsContravariant<WP, T>::value  // recurse if still wrapped
            : (std::is_class<W>::value
               and std::is_class<T>::value
               and IsSameLayoutBase<W, T>::value));
};


// IsFreshSource: True when the source represents freshly-initialized material
// that has no "trap" bit patterns to worry about.  Only Init() and Sink()
// wrappers qualify.  Propagates through outer wrappers (e.g. OnStack).
//
template<typename T, bool = HasWrappedType<T>::value>
struct IsFreshSource : std::false_type {};

template<typename T>
struct IsFreshSource<SinkWrapper<T>, true> : std::true_type {};

template<typename T>
struct IsFreshSource<InitWrapper<T>, true> : std::true_type {};

template<typename T>  // other wrappers: propagate to inner type
struct IsFreshSource<T, true> : IsFreshSource<typename T::wrapped_type> {};


//=//// "SAFE" CONTRAVARIANT TRAIT ////////////////////////////////////////=//

template<typename T>
struct IsUnsafeSinkBase : std::false_type {};

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
// type compatibility and stores a pointer.
//
// 1. If a plain ContraWrapper receives a SinkWrapper source, it ensures that
//    corruption is not pending.  This is distinct from what InitWrapper does,
//    which is to suppress the corruption (presuming that the initialization
//    code will overwrite the cell).

#undef NeedfulContra
#define NeedfulContra(T)  needful::ContraWrapper<T*>

template<typename TP>
struct ContraWrapper {
    using T = remove_pointer_t<TP>;  // T* is clearer than "TP" in this class

    NEEDFUL_DECLARE_WRAPPED_FIELD (T*, p);

    ContraWrapper()  // compiler might need, see [D]
        {}

    ContraWrapper(std::nullptr_t) : p {nullptr}
        {}

    ContraWrapper(const ContraWrapper& other) : p {other.p}
        {}

    template<typename U, IfSafeContra<U, T>* = nullptr>
    ContraWrapper(const U& u) {
        this->p = x_cast(T*, x_cast(void*, u));  // cast workaround [E]
    }

    template<typename U, IfSafeContra<SinkWrapper<U>, T>* = nullptr>
    ContraWrapper(const SinkWrapper<U>& sink) {
        assert(not sink.corruption_pending);  // catch corruption transfer [1]
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
        this->p = x_cast(T*, x_cast(void*, u));  // [E]
        return *this;
    }

    template<typename U, IfSafeContra<SinkWrapper<U>, T>* = nullptr>
    ContraWrapper& operator=(const SinkWrapper<U>& sink) {
        assert(not sink.corruption_pending);  // catch corruption transfer [1]
        this->p = static_cast<T*>(sink.p);
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
// 2. Retargeting the pointer of a Sink(T) could be prohibited, but that would
//    make it less flexible than a pointer.  Reassignment can be useful, e.g.
//    if you get an Option(Sink(T)) as nullptr as an argument, you might want
//    to default it for internal purposes...even though the caller isn't
//    expecting a value back.
//
//    So rather than disallow overwriting the pointer in a Sink, it just
//    triggers another corruption if you assign a non-nullptr.
//

#undef NeedfulSink
#define NeedfulSink(T)  needful::SinkWrapper<T*>

template<typename TP>
struct SinkWrapper {
    using T = remove_pointer_t<TP>;  // T* is clearer than "TP" in this class

    static_assert(
        not std::is_const<T>::value,
        "Sink(T) cannot sink const pointee (Sink(const T*) is okay)"
    );

    NEEDFUL_DECLARE_WRAPPED_FIELD (T*, p);

    mutable bool corruption_pending;  // can't corrupt on construct [1]

    SinkWrapper()  // compiler MIGHT need, see [D]
        : corruption_pending {false}
    {
        Corrupt_If_Needful(p);  // pointer itself in this case, not contents!
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

    template<typename U, IfSafeContra<U, T>* = nullptr>
    SinkWrapper(const U& u) {
        this->p = x_cast(T*, x_cast(void*, u));  // cast workaround [E]
        this->corruption_pending = (this->p != nullptr);
    }

    template<typename U, IfSafeContra<SinkWrapper<U>, T>* = nullptr>
    SinkWrapper(const SinkWrapper<U>& other) {
        this->p = static_cast<T*>(other.p);  // safe: IfSafeContra guarantees
        this->corruption_pending = (other.p != nullptr);  // corrupt
        other.corruption_pending = false;  // we take over corrupting
    }

    SinkWrapper(const SinkWrapper& other) {  // same-type copy constructor
        this->p = other.p;
        this->corruption_pending = (other.p != nullptr);  // corrupt
        other.corruption_pending = false;  // we take over corrupting
    }

    SinkWrapper& operator=(std::nullptr_t) {  // `=` allowed [2]
        this->p = nullptr;
        this->corruption_pending = false;
        return *this;
    }

    SinkWrapper& operator=(const SinkWrapper& other) {  // `=` allowed [2]
        if (this != &other) {
            this->p = other.p;
            this->corruption_pending = (other.p != nullptr);  // corrupt
            other.corruption_pending = false;  // we take over corrupting
        }
        return *this;
    }

    template<typename U, IfSafeContra<U, T>* = nullptr>
    SinkWrapper& operator=(const U& u) {  // `=` allowed [2]
        this->p = x_cast(T*, x_cast(void*, u));  // [E]
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

    static_assert(
        not std::is_const<T>::value,
        "Init(T) cannot init const pointee (Init(const T*) is okay)"
    );

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
