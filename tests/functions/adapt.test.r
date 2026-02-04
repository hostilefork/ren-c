; better-than-nothing ADAPT tests

(
    x: 10
    foo: adapt any/ [x: 20]
    foo [1 2 3]
    x = 20
)
(
    capture: ~
    foo: adapt any/ [capture: block]
    all [
        foo [1 2 3]
        capture = [1 2 3]
    ]
)
(
    v: copy []
    append-v: specialize append/ [
        series: v
    ]
    adapted-append-v: adapt append-v/ [
        value: to integer! value
    ]
    adapted-append-v "10"
    adapted-append-v "20"
    v = [10 20]
)

; RETURN is not available at the time of the prelude
[
    (
        captured-x: ~
        foo: func [x] [return "available now"]
        bar: adapt foo/ [
            captured-x: x
            assert [not has binding of $x 'return]
        ]
        all [
            "available now" = bar 1020
            captured-x = 1020
        ]
    )
]

; A function's locals are not visible to an ADAPT, only the functions on its
; interface.
(
    y: <outside>
    test: func [x {y}] [return ^y]
    adapted: adapt test/ [assert [y = <outside>]]
    trash? adapted 10
)

; PURE functions default to their adaptation being pure, and IMPURE functions
; default to it being impure--at least on the interface.  You can turn this
; off but the phase will still be updated to make it take effect
;
[(
    bad1: impure does [1]
    bad2: impure does [2]
    add1: pure func [x] [return x + bad1]
    okay
)

    (all {
        temp: ~
        add3: adapt add1/ [temp: x: x + bad2]
        e: sys.util/recover [add3 1018]
        e.id = 'impure-call
        e.arg1 = 'bad2  ; adapt prelude pure, as adaptee was pure
        void? ^temp
    })

    (all {
        temp: ~
        add3: pure:off adapt add1/ [temp: x: x + bad2]
        e: sys.util/recover [add3 1018]
        e.id = 'impure-call
        e.arg1 = 'bad1  ; not BAD2 (adapt prelude agnostic, PURE:OFF)
        temp = 1020
    })
]
