; %elide.test.r
;
; ELIDE is an evaluating variation of COMMENT.  It evaluates its
; argument, but discards the result and synthesizes GHOST!
;
; ELIDE will panic if you give it an ERROR! antiform, but also if you
; give it a PACK! that contains ERROR! antiforms.  However, it doesn't
; care if it can produce a value or not.  So it's more permissive than
; DECAY (which will panic if you give it something like a HEAVY VOID since
; that's an empty PACK! that can't give you a value, or GHOST!, or a
; PACK! with a PACK! in the first slot.)

(ghost? elide "a")
('~,~ = lift elide "a")
(ghost? eval [elide "a"])
('~,~ = lift eval [elide "a"])

(1 = eval [elide "a" 1])
(1 = eval [1 elide "a"])

(ghost? elide ^ghost)
(ghost? elide pack [1 2 3])
(ghost? elide pack [pack [1 2] 3])
(ghost? elide pack [3 pack [1 2]])

~zero-divide~ !! (elide 1 / 0)
~zero-divide~ !! (elide pack [1 / 0 2 3])
~zero-divide~ !! (elide pack [pack [1 / 0 2] 3])
~zero-divide~ !! (elide pack [3 pack [1 / 0 2]])

(
    x: ~
    all [
        ghost? elide elide elide x: 1020
        x = 1020
    ]
)

~no-value~ !! (
    evaluate evaluate [1 elide "a" + elide "b" 2 * 3 panic "too far"]
)
(
    code: [1 elide "a" elide "b" + 2 * 3 panic "too far"]
    pos: evaluate:step code
    pos: evaluate:step pos
    pos = [elide "b" + 2 * 3 panic "too far"]
)
(
    [pos val]: evaluate:step [
        1 + 2 * 3 elide "a" elide "b" panic "too far"
    ]
    all [
        val = 9
        pos = [elide "a" elide "b" panic "too far"]
    ]
)

(
    y: ~
    x: ~
    x: 1 + 2 * 3
    elide (y: x)

    all [x = 9, y = 9]
)
~no-value~ !! (
    y: ~
    x: ~
    x: 1 + elide (y: 10) 2 * 3  ; non-interstitial, no longer legal
)

; ELIDE-IF-VOID variation

(equal? 304 300 + 4 elide-if-void ())
(equal? 304 300 + 4 elide-if-void ~()~)
(equal? 1020 elide-if-void 1000 + 20)
(error? elide-if-void fail "passthru")
('~['10 '20]~ = lift elide-if-void pack [10 20])

; Usermode ELIDE-IF-VOID variant, to show you could write it yourself.
[(
    u-elide-if-void: vanishable func [
        "Argument is evaluative, but discarded if heavy void (or void)"

        return: [any-value?]
        ^value "Evaluation product to be ignored"
            [any-value?]  ; ghost! is passed through
    ][
        if '~()~ = lift ^value [return ()]
        return ^value
    ]
    ok
)
    (equal? 304 300 + 4 u-elide-if-void ())
    (equal? 304 300 + 4 u-elide-if-void ~()~)
    (equal? 1020 u-elide-if-void 1000 + 20)
    (error? u-elide-if-void fail "passthru")
    ('~['10 '20]~ = lift u-elide-if-void pack [10 20])
]
