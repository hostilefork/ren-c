; %pure.test.r
;
; Leveraging the same mechanisms that drive virtual binding, we are able to
; have a binding "creep down" the code as it executes, giving contextual
; information regarding pure functional contexts.
;
; Some natives are marked IMPURE due to having side-effects outside of any of
; their particular parameters (like WRITE-STDOUT), or depending on things that
; are not parameters (like NOW).  But if a native does a mutation that is
; narrowed to its parameters, then that's guided by the <const> bit.
;
; So in "PURE" contexts, what happens is that any variables outside a pure
; context that are read must be FINAL.  Internally to the pure region, it's
; assumed that the pure invocation itself constified everything that needed
; to be const, and changes that happen after that are just driven by absence
; or presence of the const bit.
;
; This means PURE functions can do things like create temporary series and
; mutate them.


; PURE function results are not discardable
[
    (ten-twenty: pure does [1000 + 20], okay)
    ~discarded-value~ !! (ten-twenty 304)
]

; Basic tests of locked things
;
; 1. A somewhat complicated mechanic is required for the pattern of
;
;      word!: word! word!  ; for instance `value: type of`
;
;    In general, a WORD! is consulted and asked if it wants to take the left
;    hand side literally.  But this pure function example shows that we do
;    not want TYPE to be offered the opportunity to take VALUE literally, as
;    it isn't part of the pure scope.  We want OF to get the first choice.
;    Mechanics peculiar to SET-WORD and this pattern are used to make that
;    offer.
[
    (all {
        slow-is-word?: pure lambda [value] [word! = type of value]
        slow-is-word? 'abba
        not slow-is-word? 1020
    })

    (all {
        slow-is-word?: pure lambda [value] [
            value: type of value  ; lookback not offered to TYPE [1]
            word! = value
        ]
        slow-is-word? 'abba
        not slow-is-word? 1020
    })
]


; Basic prevention of calling an IMPURE function from a PURE context.
; Test calling directly and indirectly through a function that's not IMPURE
[(
    /bad: impure does [~(unreachable)~]
    f-bad: final make frame! bad/

    direct: ~  ; we change what this does (maybe pure, maybe not marked)
    indirect: pure func [] [return /direct]

    for-each direct-body compose2:deep ${} [
        (does [bad])
        (does [(bad)])

        (does [eval {$(bad)}])  ; composed group not pure, but should error

        (does [eval [bad]])
        (does [eval bad/])

        (does [run bad/])

        (does [eval f-bad])
        (does [let f: make frame! bad/, eval f])
    ][
        unprotect $direct
        /direct: eval direct-body  ; not PURE for starters
        for-each name [indirect direct] [
            if name = 'direct [  ; change to PURE when calling directly
                unprotect $direct
                direct: pure direct/
            ]
            let e: sys.util/recover [eval get meta name]
            any [
                not error? cond e
                e.id != 'impure-call
                e.arg1 <> 'bad
            ] then [
                print ["Did not get expected error."]
                print ["Call was:" name]
                print ["Direct's body was:" mold direct-body]
                print ["Result was:" mold reify e]
                panic
            ]
        ]
    ]
    okay
)]


[(
    /square-of: func [x] [
        if now:weekday > 5 [  ; 6 = Saturday, 7 = Sunday
           return "I don't do math on weekends!"
        ]
        return x * x
    ]
    okay
)

    (did match [integer! text!] 1 + square-of 2)
    ~impure-call~ !! (1 + eval pure does [square-of 2])
]


(all {
    x: 10
    foo: pure does [let y, elide y: 10, x: 20]  ; can't write x
    e: sys.util/recover [foo]
    e.id = 'pure-non-final
    e.arg1 = 'x
})
(all {
    x: 10
    foo: pure does [let y, elide y: 10, x]  ; can't read x, either
    e: sys.util/recover [foo]
    e.id = 'pure-non-final
    e.arg1 = 'x
})
(all {
    x: final [a b c]
    foo: pure does [let y, elide y: 10, x]  ; okay if it's final
    foo = [a b c]
})
~series-frozen~ !! (all {
    x: final [a b c]
    foo: pure does [let y, elide y: 10, append x 'd]
    foo
})
