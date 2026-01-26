(
    success: ~
    if 1 > 2 [success: 'false] else [success: 'true]
    true? success
)
(
    success: ~
    if 1 < 2 [success: 'true] else [success: 'false]
    true? success
)
(
    success: ~
    if not 1 > 2 [success: 'true] else [success: 'false]
    true? success
)
(
    success: ~
    if not 1 < 2 [success: 'false] else [success: 'true]
    true? success
)
(
    success: ~
    if ok (does [success: 'true])
    true? success
)
(
    success: 'true
    if null (does [success: 'false])
    true? success
)

[https://github.com/metaeducation/ren-c/issues/510 (
    c: func [return: [integer!] i] [
        return if i < 15 [30] else [4]
    ]

    d: func [return: [integer!] i] [
        return (if i < 15 [30] else [4])
    ]

    all [
        30 = c 10
        4 = c 20
        30 = d 10
        4 = d 20
    ]
)]

; Literals need to account for infix deferral
(
    foo: func [return: [integer!] y] [return the 1 then (x -> [x + y])]
    bar: func [return: [integer!] y] [return 1 then (x -> [x + y])]
    all [
        11 = foo 10
        1 = bar 10
    ]
)

; The above should work whether you use a GROUP! or not (-> quote left wins)
(
    foo: func [return: [integer!] y] [return the 1 then (x -> [x + y])]
    bar: func [return: [integer!] y] [return 1 then (x -> [x + y])]
    all [
        11 = foo 10
        1 = bar 10
    ]
)

(
    null? (~null~ then [~(unreachable)~])
)

(~ then [okay])

; Although branches can be triggered by heavy null, if functions ask to
; receive the value it is decayed, so they do not have to be ^META.  But if
; they *are* ^META then the true state is passed through.
[
    (~(~null~)~ then (x -> [(lift ^x) = '~null~]))
    (~(~null~)~ then (^x -> [(lift ^x) = '~(~null~)~]))
    ('~(~null~)~ then (x -> [^x = '~(~null~)~]))
    ('~(~null~)~ then (^x -> [^x = '~(~null~)~]))

    (catch [~(~null~)~ also (x -> [throw ((lift ^x) = '~null~)])])
    (catch [~(~null~)~ also (^x -> [throw ((lift ^x) = '~(~null~)~)])])
    (catch ['~null~ also (^x -> [throw (^x = '~null~)])])
]

[
    ~no-arg~ !! (else [~unused~])
    (() then [okay])  ; VOID! with THEN is legal...
    (1000 + 20 () then [okay])

    (^void then [okay])
    (1020 = (1000 + 20 ghostly (^void else [~(unreachable)~])))

    ((^void) then [okay])
    (void? (1000 + 20 ((^void) else [~(unreachable)~])))

    (eval [] then [okay])
    (void? eval [] else [~(unreachable)~])
]

[
    (foo: func [] [return ^void], ok)
    (foo then [okay])
    (1020 = (1000 + 20 ghostly (foo else [~(unreachable)~])))
]

[
    (foo: lambda [] [if null [~(unreachable)~]], ok)
    (foo else [okay])
    (null? (1000 + 20 foo then [~(unreachable)~]))
]

[
    (foo: lambda [] [], ok)
    (foo then [okay])
    (1020 = (1000 + 20 ghostly (foo else [~(unreachable)~])))
]

; https://forum.rebol.info/t/2176
[
    (null = (null else (^void)))
    (void? (^void else (^void)))

    (3 = (if ok [1 + 2] then (^void)))
]

; THEN passes FAILURE! through, ELSE will panic on failure if not handled by
; an action branch that is meta-paramterized
[
    (failure? 1 / 0 then [~(unreachable)~])
    ~zero-divide~ !! (
        assert [not failure? (1 / 0 else [~(unreachable)~])]
    )
    ~zero-divide~ !! (
        assert [not failure? (1 / 0 else 'unreachable)]
    )
    ~zero-divide~ !! (
        assert [not failure? (1 / 0 else (e -> [~(unreachable)~]))]
    )
    ~zero-divide~ !! (
        assert [not failure? (1 / 0 else (does [~(unreachable)~]))]
    )
    (failure? (1 / 0 else (^e -> [^e])))
]

; THENCE is prefix reversed THEN
[
    (3 = thence [1 + 2] okay)
    (null? thence [1 + 2] null)
    (void? thence [1 + 2] ())
]

; IF DID is prefix THEN
; IF DIDN'T is prefix ELSE
[
    (
        did*: enclose did/ lambda [f] {
            result: eval f
            assert [result = not didn't ^f.value]
        }
        didn't*: enclose didn't/ lambda [f] {
            result: eval f
            assert [result = not did ^f.value]
        }
    )
    (did* 1020)
    (did* ~[]~)
    (didn't* ())
    (didn't* null)
    (did* heavy null)
    (didn't* fail "hi")
]
