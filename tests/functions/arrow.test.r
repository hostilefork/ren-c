; %arrow.test.r
;
; ARROW is a lighter notation for LAMBDA which also has the ability to
; unpack PACK!s.

(
    foo: (-> [? + 20])  ; ? is implied variable name if none given
    1020 = foo 1000
)

(
    foo: (_ -> [304])  ; right now `_` is just "no name input"
    304 = foo 1000
)

(
    packsum: ~(a b)~ -> [a + b]
    1020 = packsum pack [1000 20]
)

(
    reflector: ~(^a ^b ^c)~ -> [reduce [lift ^a lift ^b lift ^c]]

    all [
        ['1 '2 '3] = reflector pack [1 2 3]
        ['1 '2 ~] = reflector pack [1 2]
        [~ ~ ~] = reflector pack []
    ]
)

; Usermode arrow implementation (to show that anyone can do it...)
;
; https://rebol.metaeducation.com/t/arrow-function-written-in-usermode/2651
[(
    uarrow: lambda [
       []: [action!]
       @(spec) "Ordinary spec, single arg spec, or ~(args to unpack)~"
            [<hole> _ word! 'word! ^word! :word! block! ~group!~]
       @(body) "Code to execute (will not be deep copied)"
            [block! fence!]
    ][
        either quasiform? ^spec [  ; ~(a b)~ -> [a + b] unpacks PACK!
            spec: as block! unquasi spec
            lambda [^pack [pack!]] (compose ${
                f: copy '(lambda spec (body))
                set words of f ^pack
                eval f
            })
        ][
            lambda blockify any [spec '?] (body)
        ]
    ]
)

    (
        packsum: uarrow ~(a b)~ [a + b]
        1020 = packsum pack [1000 20]
    )

    (
        reflector: uarrow ~(^a ^b ^c)~ [reduce [lift ^a lift ^b lift ^c]]

        all [
            ['1 '2 '3] = reflector pack [1 2 3]
            ['1 '2 ~] = reflector pack [1 2]
            [~ ~ ~] = reflector pack []
        ]
    )
]
