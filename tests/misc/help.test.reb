; %help.test.reb
;
; !!! HELP and SOURCE have become more complex in Ren-C, due to the appearance
; of function compositions, and an attempt to have their "meta" information
; guide the help system--to try and explain their relationships, while not
; duplicating unnecessary information.  e.g. a function with 20 parameters
; that is specialized to have one parameter fixed, should not need all
; other 19 parameter descriptions to be copied onto a new function.
;
; The code trying to sort out these relationships has come along organically,
; and involves a number of core questions--such as when to use null vs. blank.
; It has had a tendency to break, so these tests are here even though they
; spew a large amount of output, in the interests of making HELP stay working.

(nihil? help)
(nihil? help help)
(nihil? help system)
(nihil? help to)
(nihil? help "to-")
(nihil? help "to")
(nihil? help void)
(nihil? help xxx)
(nihil? help function)

(
    for-each w words of lib [
        dump w
        if quasi? ^(get/any w) [continue]
        if action? get w
            (compose/deep [assert [none? help (w)]])
        else [
            if not issue? get w [ comment "don't open web browser"
                assert [nihil? help (get w)]
            ]
        ]
    ]
    true
)
(
    nihil? source ||   ; Was once a tricky case, SOURCE of a barrier
)
(
    for-each w words of lib [
        dump w
        if quasi? ^(get/any w) [continue]
        if action? get w
            (compose/deep [assert [nihil? source (w)]])
    ]
    true
)

[https://github.com/metaeducation/ren-c/issues/1106
    (nihil? help "any")
]

(nihil? about)
