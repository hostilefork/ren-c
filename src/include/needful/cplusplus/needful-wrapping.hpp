//
//  file: %needful-wrapping.hpp
//  summary: "Helpers for wrapped type detection and rewrapping"
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
// Needful's goal is to bring C++ as a service to codebases whose semantic
// behaviors are all accomplished with C.
//
// This means that any wrapper classes used in a C++ build are only for
// type checking and assertions.  Hence they are always "thin" proxies for
// some single wrapped implementation type.  That narrowness makes it
// possible for us to provide efficient and automatic metaprogramming
// abilities for these wrapped types (e.g. mutability casts).
//
// What we do is have all such classes expose a static member called
// `::wrapped_type`, which we then leverage to make several metaprogramming
// operations automatic--without the wrapper class author having to get
// involved and write specializations.  All they have to do is permit
// explicit casting to the wrapped type, and Needful can do the rest.
//
//=//// NOTES /////////////////////////////////////////////////////////////=//
//
// A. In order for the metaprogramming assistance to work, the wrapped_type
//    must be *the* type you are templated against.  e.g. don't do this:
//
//        template<typename T>
//        struct MyWrapper {
//           using wrapped_type = T*;
//           T* pointer;
//           ...
//        };
//
//     When you didn't synchronize `wrapped_type` with the actual type then
//     the intelligence to extract the type via an explicit cast and then
//     use `needful_rewrap_type()` to build a new wrapper with a different
//     inner type won't work.
//
//     If you want to do something like this, you can hide the pointer with
//     a macro.  You'll be using a macro anyway, because Needful is for C
//     codebases, so you wouldn't have <> in source anyway!
//
//         #if defined(__cplusplus)
//             #define MyWrap(T) MyWrapper<T*>
//         #else
//             #define MyWrap(T) T*
//         #endif
//


//=//// WRAPPER CLASS DEFINITION ///////////////////////////////////////////=//
//
// This is better than just `using wrapped_type = T;` because it helps people
// reading the code naviate to this definition and read the rationale for
// why this convention is used.  It also means you can't cheat--the bits
// are the same as the C definition.
//
// 1. If you have a base class which has the actual storage of the wrapped
//    type, and a derived class which just adjusts the access for it, then
//    this is a special case where the derived class may need to override
//    the wrapped_type to be in sync with its template parameter.
//

#define NEEDFUL_DECLARE_WRAPPED_FIELD(T,name) \
    static_assert(std::is_standard_layout<T>::value, \
        "Needful wrapped types must be standard-layout"); \
    using wrapped_type = T; \
    T name

#define NEEDFUL_OVERRIDE_WRAPPED_FIELD_TYPE(T) /* derived class may use */ \
    static_assert(std::is_standard_layout<T>::value, \
        "Needful wrapped types must be standard-layout"); \
      using wrapped_type = T


//=//// WRAPPER CLASS DETECTION ///////////////////////////////////////////=//
//
// Uses SFINAE (Substitution Failure Is Not An Error) to detect if a type T
// has a nested type called `wrapped_type`. This enables generic code to
// distinguish between "wrapper" types (which define `wrapped_type`) and
// regular types.
//
// The trick is to declare two overloads of `test`: one that is valid only if
// `T::wrapped_type` exists, and a fallback. The result is a compile-time
// boolean constant, `HasWrappedType<T>::value`, that is true if T has a
// `wrapped_type`, and false otherwise.
//
// This pattern is robust and avoids hard errors for types that do not have
// the member, making it ideal for metaprogramming and generic code.
//
// 1. Any type exposing `wrapped_type` must be standard-layout, because
//    NEEDFUL_EXTRACT_INNER relies on reinterpret_cast to the first member
//    (guaranteed only for standard-layout structs).  Enforcing this here
//    catches violations at the earliest possible point--when the wrapper
//    type is first examined--rather than at each individual cast site.
//

template<typename T>
struct HasWrappedType {
  private:
    template<typename U>
    static auto test(int) -> decltype(
        typename U::wrapped_type{},
        std::true_type{}
    );
    template<typename>
    static std::false_type test(...);

  public:
    static constexpr bool value = decltype(test<T>(0))::value;

    static_assert(  // standard-layout needed for first-member aliasing [1]
        not value or std::is_standard_layout<T>::value,
        "Wrapper types (with wrapped_type) must be standard-layout"
    );
};


//=//// UNWRAPPING: GET THE INNER TYPE OF A WRAPPER /////////////////////=//

template<typename T, bool = HasWrappedType<T>::value>
struct UnwrappedType
  { using type = T; };

template<typename T>
struct UnwrappedType<T, true>
  { using type = typename T::wrapped_type; };

#define needful_unwrapped_type(T) \
    typename needful::UnwrappedType<T>::type

template<typename T>
struct UnwrappedIfWrappedType {
    using type = conditional_t<
        HasWrappedType<T>::value,
        needful_unwrapped_type(T),
        T
    >;
};

#define needful_unwrapped_if_wrapped_type(T) \
    typename needful::UnwrappedIfWrappedType<T>::type


//=//// REWRAP AN INNER TYPE WITH THE SAME TEMPLATE ///////////////////////=//
//
// This allows you to generically "re-wrap" a type with the same template as
// the original wrapper, but with a different inner type. This is a common
// metaprogramming pattern sometimes called "rebind" in C++ libraries.
//
// 1. Wrapper here is a "template template parameter":
//
//      https://en.cppreference.com/w/cpp/language/template_parameters.html
//

template<typename WrapperType, typename NewInnerType>
struct RewrapHelper;

template<
    template<typename> class Wrapper,
    typename OldInner,
    typename NewInner
>
struct RewrapHelper<Wrapper<OldInner>, NewInner> {
    using type = Wrapper<NewInner>;
};

template<typename Wrapper, typename NewWrapped>
struct RewrapHelper<const Wrapper, NewWrapped> {  // const needs forwarding
    using type = const typename RewrapHelper<Wrapper, NewWrapped>::type;
};

#define needful_rewrap_type(WrapperType, NewInnerType) \
    typename needful::RewrapHelper<WrapperType, NewInnerType>::type


//=//// LEAF TYPE IN WRAPPER LAYERS ///////////////////////////////////////=//
//
// LeafPointee: Extract the innermost pointee type through wrapper layers.
//
//     LeafPointee<Slot*>::type             => Slot
//     LeafPointee<ExactWrapper<Slot*>>     => Slot
//     LeafPointee<OnStack(Init(Slot))>     => Slot
//
template<typename T, bool = HasWrappedType<T>::value>
struct LeafPointee { using type = remove_pointer_t<T>; };

template<typename T>
struct LeafPointee<T, true> : LeafPointee<typename T::wrapped_type> {};


//=//// SEMANTIC VS. NON-SEMANTIC WRAPPERS ////////////////////////////////=//
//
// Wrappers fall into two categories that matter for casting behavior:
//
// * "Semantic" wrappers carry meaning beyond the raw type--their wrapping
//   is part of the data's contract.  Option(T) tracks engaged/disengaged
//   state; Result(T) signals that an error may have occurred.  Casting
//   should preserve these wrappers: `cast(U, Result(T))` --> `Result(U)`.
//
// * "Non-semantic" wrappers describe parameter-passing conventions rather
//   than data state: Sink(T) marks a write-through parameter, Need(T) marks
//   a non-null contract, Exact(T) blocks implicit conversions, etc.  Once
//   you cast, you're done with the convention, so cast extracts the inner
//   value: `cast(U, sink_value)` --> `U`.
//
// Specialize `IsWrapperSemantic` to `true_type` for your wrapper if
// cast() should auto-preserve it.  The default is false (extract).
//

template<typename>
struct IsWrapperSemantic : std::false_type {};


//=//// BASIC TYPE DETECTION //////////////////////////////////////////////=//
//
// C++ splits its types into "scalar" (arithmetic + enum + pointer + member
// pointer + nullptr_t) and "compound" (everything else).  But scalar lumps
// pointers together with integers and enums, which is wrong for our cast
// dispatch: pointer-to-pointer casts need reinterpret_cast and CastHook
// validation, while int-to-enum or bool-to-int are fine with static_cast.
//
// IsBasicType captures the subset of types where static_cast is always
// valid and no hook dispatch is needed: fundamentals (int, bool, float...)
// and enums.  For wrapped types, it looks through the wrapper to classify
// based on the inner type (e.g. Need(int) is basic, Need(Foo*) is not).
//
// This drives SFINAE in the h_cast() overloads:
//   - basic -> basic:  overload 1 (constexpr static_cast, no hooks)
//   - non-basic -> basic:  overload 2 (hooks, but basic target)
//   - everything else:  overloads 3/4/5 (reinterpret_cast territory)
//

template<typename T>
struct IsBasicType {
    static constexpr bool value = std::is_fundamental<T>::value
        or std::is_enum<T>::value
        or (HasWrappedType<T>::value and (
            std::is_fundamental<needful_unwrapped_type(T)>::value
            or std::is_enum<needful_unwrapped_type(T)>::value
        ));
};


//=//// INNER EXTRACTION FROM WRAPPERS ////////////////////////////////////=//
//
// Needful wrappers are standard-layout structs whose first (and only) data
// member is the wrapped value.  C++ guarantees that a pointer to a
// standard-layout struct can be reinterpret_cast to a pointer to its first
// member (and vice versa):
//
//   https://en.cppreference.com/w/cpp/language/data_members#Standard-layout
//
// This is how the casting wrapped conversion operators (which may have
// side effects—e.g. Sink's corruption semantics) can directly access the
// inner value for casting purposes.  The static_assert on is_standard_layout
// in each overload ensures the guarantee actually holds.
//

#define NEEDFUL_EXTRACT_INNER(InnerType, wrapper) \
    reinterpret_cast<const InnerType&>(wrapper)
