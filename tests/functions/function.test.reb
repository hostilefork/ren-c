; datatypes/function.r
(action? does ["OK"])
(not frame? 1)
(frame! = kind of unrun does ["OK"])
; minimum
(frame? unrun does [])

; !!! literal form no longer supported
;
~???~ !! (load "#[action! [[] []]]")

; return-less return value tests
(
    f: does [null]
    null? f
)
(
    f: does [:abs]
    :abs = f
)
(
    a-value: #{}
    f: does [a-value]
    same? a-value f
)
(
    a-value: charset ""
    f: does [a-value]
    same? a-value f
)
(
    a-value: []
    f: does [a-value]
    same? a-value f
)
(
    a-value: blank!
    f: does [a-value]
    same? a-value f
)
(
    f: does [1/Jan/0000]
    1/Jan/0000 = f
)
(
    f: does [0.0]
    0.0 == f
)
(
    f: does [1.0]
    1.0 == f
)
(
    a-value: me@here.com
    f: does [a-value]
    same? a-value f
)
(
    f: does [trap [1 / 0]]
    error? f
)
(
    a-value: %""
    f: does [a-value]
    same? a-value f
)
(
    a-value: does []
    f: does [:a-value]
    same? :a-value f
)
(
    a-value: first [:a]
    f: does [:a-value]
    (same? :a-value f) and (:a-value == f)
)
(
    f: does [#"^M"]
    #"^M" == f
)
(
    f: does [0]
    0 == f
)
(
    f: does [1]
    1 == f
)
(
    f: does [#a]
    #a == f
)
(
    a-value: first ['a/b]
    f: does [:a-value]
    :a-value == f
)
(
    a-value: first ['a]
    f: does [:a-value]
    :a-value == f
)
(
    f: does [true]
    true = f
)
(
    f: does [false]
    false = f
)
(
    f: does [$1]
    $1 == f
)
(
    f: does [:append]
    same? :append f
)
(
    f: does [_]
    blank? f
)
(
    a-value: make object! []
    f: does [:a-value]
    same? :a-value f
)
(
    a-value: first [()]
    f: does [:a-value]
    same? :a-value f
)
(
    f: does [get '+]
    same? get '+ f
)
(
    f: does [0x0]
    0x0 == f
)
(
    a-value: 'a/b
    f: does [:a-value]
    :a-value == f
)
(
    a-value: make port! http://
    f: does [:a-value]
    port? f
)
(
    f: does [/a]
    /a == f
)
(
    a-value: first [a/b:]
    f: does [:a-value]
    :a-value == f
)
(
    a-value: first [a:]
    f: does [:a-value]
    :a-value == all [:a-value]
)
(
    a-value: ""
    f: does [:a-value]
    same? :a-value f
)
(
    a-value: make tag! ""
    f: does [:a-value]
    same? :a-value f
)
(
    f: does [0:00]
    0:00 == f
)
(
    f: does [0.0.0]
    0.0.0 == f
)
(
    f: does [()]
    void' = ^ f
)
(
    f: does ['a]
    'a == f
)
; two-function return tests
(
    g: func [return: [integer!] f [action?]] [f [return 1] 2]
    1 = g :do
)
; BREAK out of a function
(
    null? repeat 1 [
        f: does [break]
        f
        2
    ]
)
; THROW out of a function
(
    1 = catch [
        f: does [throw 1]
        f
        2
    ]
)

~zero-divide~ !! (
    error? trap [
        f: does [1 / 0 2]  ; "error out" of a function
        f
        2
    ]
)

; The BREAK designates breaking the outer repeat (definitional BREAK)
(
    null = repeat 1 [
        f: lambda [x] [
            either x = 1 [
                repeat 1 [f 2]
                x
            ] [break]
        ]
        f 1
    ]
)
; THROW out leaves a "running" function in a "clean" state
(
    did all [
        null? catch [
            f: lambda [x] [
                either x = 1 [
                    catch [f 2]
                    x
                ] [throw 1]
            ]
            result: f 1
        ]
        result = 1
    ]
)

; "error out" leaves a "running" function in a "clean" state
(
    f: lambda [x] [
        either x = 1 [
            error? trap [f 2]
            x = 1
        ] [1 / 0]
    ]
    f 1
)

; Argument passing of "hard literal arguments"
[
    (
        hard: func ['x] [return :x]
        true
    )

    (10 == hard 10)
    ('a == hard a)
    (the 'a == hard 'a)
    (the :a == hard :a)
    (the a: == hard a:)
    (the (10 + 20) == hard (10 + 20))
    (
        o: context [f: 10]
        the :o.f == hard :o.f
    )
]

; Argument passing of "medium literal arguments"
[
    (
        medium: func [':x <with> got] [got: :x, return 1000]
        Lmedium: enfix :medium

        got: null
        test: lambda [expr [block!]] [
            got: '~trash~
            compose [(do expr), (:got)]
        ]
        true
    )

    ([1000, 1] = test [medium 1])
    ([1000, a] = test [medium a])
    ([1000, 'a] = test [medium 'a])
    ([1000, 304] = test [medium :(300 + 4)])
    ([1000, (300 + 4)] = test [medium (300 + 4)])
    ([1000, o.f] = test [medium o.f])
    (
        o: context [f: 304]
        [1000, 304] = test [medium :o.f]
    )

    ; Key point on which MEDIUM and SOFT differ, enfix quote handling
    (
        +Q: enfix lambda ['x [<end> integer!] y] [if x [x + y] else [<null>]]
        [<null>, 10] = test [medium 10 +Q 20]
    )

    ([1001, 2] = test [1 + 2 Lmedium])
    ([1001, <hi>] = test [1 + :(first [<hi>]) Lmedium])

]

; Argument passing of "soft literal arguments"
[
    (
        soft: lambda [:x <with> got] [got: :x, 1000]
        Lsoft: enfix :soft

        got: null
        test: func [expr [block!]] [
            got: '~trash~
            return compose [(do expr), (:got)]
        ]
        true
    )

    ([1000, 1] = test [soft 1])
    ([1000, a] = test [soft a])
    ([1000, 'a] = test [soft 'a])
    ([1000, a:] = test [soft a:])
    ([1000, 304] = test [soft :(300 + 4)])
    ([1000, (300 + 4)] = test [soft (300 + 4)])
    ([1000, o.f] = test [soft o.f])
    (
        o: context [f: 304]
        [1000, 304] = test [soft :o.f]
    )

    ; Key point on which MEDIUM and SOFT differ, enfix quote handling
    (
        +Q: enfix lambda ['x y] [x + y]
        [1000, 30] = test [soft 10 +Q 20]
    )

    ([1000, 3] = test [1 + 2 Lsoft])
    ([1000, 6] = test [1 + :(2 + 3) Lsoft])
]

; basic test for recursive action invocation
(
    i: 0
    countdown: lambda [n] [if n > 0 [i: i + 1, countdown n - 1]]
    countdown 10
    i = 10
)

; In Ren-C's specific binding, a function-local word that escapes the
; function's extent cannot be used when re-entering the same function later
;
; !!! Review: Fix these tests.

~expect-arg~ !! (
    f: func [code value] [return either blank? code ['value] [do code]]
    f-value: f blank blank
    f compose [2 * (f-value)] 21  ; re-entering same function
)
~expect-arg~ !! (
    f: func [code value] [return either blank? code ['value] [do code]]
    g: func [code value] [return either blank? code ['value] [do code]]
    f-value: f blank blank
    g compose [2 * (f-value)] 21  ; re-entering different function
)

[#19
    ~bad-parameter~ !! (
        f: func [/r [integer!]] [return x]
        2 == f/r/r 1 2  ; Review: could be a syntax for variadic refinements?
    )
]

[#27
    ~unassigned-attach~ !! (
        error? trap [(kind of) 1]
    )
]

; inline function test
[#1659 (
    f: does :(reduce [unrun does [true]])
    f
)]

; Second time f is called, `a` has been cleared so `a [d]` doesn't recapture
; the local, and `c` holds the `[d]` from the first call.  This succeeds in
; R3-Alpha for a different reason than it succeeds in Ren-C; Ren-C has
; closure semantics for functions so the c: [d] where d is 1 survives.
; R3-Alpha recycles variables based on stack searching (non-specific binding).
(
    a: lambda [b] [
        a: null  comment "erases a so only first call saves c"
        c: b
    ]
    f: lambda [d] [
        a [d]
        do c
    ]
    did all [
        1 = f 1
        1 = f 2
    ]
)
[#2025
    ~unassigned-attach~ !! (
        assert [unset? 'x, unset? 'y]

        body: [return x + y]
        f: func [x] body
        g: func [y] body
        (f 1)
    )
]

[#2044 (
    o: make object! [f: func [x] [return 'x]]
    p: make o []
    not same? o.f 1 p.f 1
)]

(
    o1: make object! [x: {x} o2: make object! [y: {y}]]
    outer: {outer}
    n: 20

    f: function [
        /count [integer!]
        <in> o1 o1.o2
        <with> outer
        <static> static (10 + n)
    ][
        count: default [2]
        data: reduce [count x y outer static]
        return case [
            count = 0 [reduce [data]]
            true [
               append (f/count count - 1) data
            ]
        ]
    ]

    f = [
        [0 "x" "y" "outer" 30]
        [1 "x" "y" "outer" 30]
        [2 "x" "y" "outer" 30]
    ]
)

; Duplicate arguments or refinements.
[
    ~dup-vars~ !! (func [a b a] [return 0])
    ~dup-vars~ !! (function [a b a] [return 0])
    ~dup-vars~ !! (func [/test /test] [return 0])
    ~dup-vars~ !! (function [/test /test] [return 0])
]

; /LOCAL is an ordinary refinement in Ren-C
(
    a-value: func [/local [integer!]] [return local]
    1 == a-value/local 1
)

[#539 https://github.com/metaeducation/ren-c/issues/755 (
    f: func [return: <none>] [
        use [x] [return none]
        42
    ]
    none' = ^ f
)]

(
    foo: lambda [^arg [<opt> pack? <end> <void> integer!]] [arg]
    did all [
        (the '1020) = (foo 1020)
        nihil' = (foo comment "HI")
        null' = (foo any [1 > 2, 3 > 4])
        null? (foo)
    ]
)
