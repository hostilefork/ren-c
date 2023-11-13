; %enfix.test.reb

(isotope! = kind of :+)
(true = enfix? :+)

(enfix? :+)
~expect-arg~ !! (enfix? 1)
(activation? get '+)

; #1934
(3 = do reduce [1 unrun get '+ 2])
~no-arg~ !! (do reduce [unrun :+ 1 2])


(
    foo: :+
    did all [
        enfix? :foo
        3 = (1 foo 2)
    ]
)
(
    set 'foo enfix :add
    did all [
        enfix? :foo
        1 foo 2 = 3
    ]
)
(
    set 'postfix-thing enfix lambda [x] [x * 2]
    all [
       enfix? :postfix-thing
       20 = (10 postfix-thing)
    ]
)

~no-arg~ !! (do reduce [unrun get '+ 1 2])  ; enfix no argument


; Only hard-quoted parameters are <skip>-able
~???~ !! (
    lambda [x [<skip> integer!] y] [<unreachable>]
)

[
    (
        skippy: lambda ['x [<skip> integer!] y] [reduce [any [x _] y]]
        lefty: enfix :skippy
        true
    )

    ([_ "hi"] = skippy "hi")
    ([10 "hi"] = skippy 10 "hi")

    ([_ "hi"] = lefty "hi")
    ([1 "hi"] = 1 lefty "hi")

    ; Enfixed skipped left arguments mean that a function will not be executed
    ; greedily...it will run in its own step, as if the left was an end.
    (
        var: ~
        block: [<tag> lefty "hi"]
        did all [
            <tag> = evaluate/next block 'block
            [lefty "hi"] = block
            [_ "hi"] = evaluate/next block 'block
            [] = block
        ]
    )

    ; Normal operations quoting rightward outrank operations quoting left,
    ; making the left-quoting operation see nothing on the left, even if the
    ; type matched what it was looking for.
    (
        unset 'var
        block: [the 1 lefty "hi"]
        did all [
            1 = evaluate/next block 'block
            [lefty "hi"] = block
            [_ "hi"] = evaluate/next block 'block
            [] = block
        ]
    )

    ([_ "hi"] = any [false null lefty "hi"])
]


; ->- is the "SHOVE" operation.  It lets any ACTION!, including one dispatched
; from PATH!, receive its first argument from the left.  It uses the parameter
; conventions of that argument.

; NORMAL parameter
;
(9 = (1 + 2 ->- multiply 3))
(7 = (add 1 2 ->- multiply 3))
(7 = (add 1 2 ->- (:multiply) 3))

; :HARD-QUOTE parameter
(
    x: null
    x: ->- default [10 + 20]
    x: ->- default [1000000]
    x = 30
)

; SHOVE should be able to handle refinements and contexts.
[
    (did obj: make object! [
        magic: enfix lambda [a b /minus] [
            either minus [a - b] [a + b]
        ]
    ])

    ~???~ !! (1 obj.magic 2)  ; must use shove

    (3 = (1 ->- obj.magic 2))
    (-1 = (1 ->- obj.magic/minus 2))
]


; PATH! cannot be directly quoted left, must use ->-

[
    (
        left-the: enfix :the
        o: make object! [i: 10 f: does [20]]
        true
    )

    ~literal-left-tuple~ !! (o.i left-the)
    (o.i ->- left-the = 'o.i)

    ~literal-left-tuple~ !! (o.f left-the)
    (o.f ->- left-the = 'o.f)
]

; Rather than error when SET-WORD! or SET-PATH! are used as the left hand
; side of a -> operation going into an operation that evaluates its left,
; the value of that SET-WORD! or SET-PATH! is fetched and passed right, then
; written back into the variable.

(
    x: 10
    x: me + 20
    x = 30
)

; !!! ME is currently macro-based and double evaluates the group...once for
; the GET and once for the SET.  This is undesirable.
(
    o: make object! [x: 10]
    count: 0
    o.(count: count + 1 'x): me + 20
    (o.x = 30) and (count = 2)  ; !!! count should only be 1
)


; Right enfix always wins over left, unless the right is at array end

((the ->-) = first [->-])
((the ->- the) = 'the)
('x = (x >- the))
(1 = (1 ->- the))

(1 = (1 >- the))
('x = (x >- the))

; "Precedence" manipulation via >- and ->-

(9 = (1 + 2 ->- multiply 3))
(9 = (1 + 2 >- multiply 3))
(9 = (1 + 2 >-- lib.* 3))
(9 = (1 + 2 ->- lib.* 3))

(7 = (add 1 2 * 3))
(7 = (add 1 2 ->- lib.* 3))
(7 = (add 1 2 >- lib.* 3))

~expect-arg~ !! (10 ->- lib.= 5 + 5)
~expect-arg~ !! (10 >- lib.= 5 + 5)
(10 >-- lib.= 5 + 5)
(10 >- = (5 + 5))

~no-arg~ !! (
    add 1 + 2 >- multiply 3
)
(
    x: add 1 + 2 3 + 4 >- multiply 5
    x = 38
)
(-38 = (negate x: add 1 + 2 3 + 4 >- multiply 5))

~no-arg~ !! (
    divide negate x: add 1 + 2 3 + 4 >- multiply 5
)
(-1 = (divide negate x: add 1 + 2 3 + 4  2 >- multiply 5))


(
    (x: add 1 add 2 3 |> lib.* 4)
    x = 24
)

(
    count: 0
    o: make object! [x: null]
    nuller: function [y] [return null]
    o.(count: count + 1, first [x]): my nuller
    did all [
        :o.x = null
        count = 2
    ]
)

[
    https://github.com/metaeducation/ren-c/issues/581

    (
        foo: func [] [
            fail "foo should not run, it's prefix and runs on *next* step"]
        did all [
            1020 == evaluate/next [1020 foo 304] 'pos
            pos == [foo 304]
        ]
    )(
        enfoo: enfix func [] [return <enfoo>]
        did all [
            <enfoo> == evaluate/next [1020 enfoo 304] 'pos
            pos = [304]
        ]
        comment "0-arity function, but enfixed so runs in *same* step"
    )

    (
        ifoo: func ['i [<skip> integer!]] [
            fail "ifoo should not run, it tests <skip> on *next* step"
        ]
        did all [
            ignored: func [return: [issue!]] [
                ignored: null
                return #ignored
            ]
            did all [
                #ignored == var: evaluate/next [ignored ifoo 304] 'pos
                var == #ignored
                pos = [ifoo 304]
                null? ignored
            ]
        ]
    )(
        enifoo: enfix func ['i [<skip> integer!]] [
            fail [
                {enifoo should not run; when arguments are skipped this}
                {defers the enfix until the next evaluator step.  Otherwise}
                {`case [1 = 1 [print "good"]] else [print "bad"]`}
                {would print both `good` and `bad`.}
            ]
        ]
        did all [
            ignored: func [return: [issue!]] [
                ignored: null
                return #ignored
            ]
            did all [
                var: evaluate/next [ignored enifoo 304] 'pos
                pos = [enifoo 304]
                var == #ignored
                null? ignored
            ]
        ]
    )(
        enifoo: enfix lambda ['i [<skip> integer!]] [compose '<enifoo>/(i)]
        did all [
            did all [
                var: evaluate/next [1020 enifoo 304] 'pos
                pos = [304]
                var == '<enifoo>/1020
            ]
            comment {
                When arguments are not skipped, the behavior should be the
                same as a non-skippable enfix function
            }
        ]
    )

    (
        bar: func [return: [nihil?]] [bar: null, return nihil]
        did all [
            var: evaluate/next [1020 bar 304] 'pos
            pos = [bar 304]
            var == 1020
            activation? :bar
            bar
            null? bar
        ]
        comment {Invisible normal arity-0 function should run on next eval}
    )(
        enbar: enfix func [left] [enbar: null, return left]
        did all [
            var: evaluate/next [1020 enbar 304] 'pos
            pos = [304]
            var == 1020
            null? enbar
        ]
        comment {Invisible enfix arity-0 function should run on same step}
    )

    (
        ibar: func ['i [<skip> integer!]] [ibar: null]
        did all [
            ignored: func [return: [issue!]] [
                ignored: null
                return #ignored
            ]
            did all [
                var: evaluate/next [ignored ibar 304] 'pos
                pos = [ibar 304]
                var == #ignored
                null? ignored
            ]
            comment {skip irrelevant (tests right on *next* step)}
        ]
    )(
        enibar: enfix func [return: <void> 'i [<skip> integer!]] [
            fail {
                When arguments are skipped, this defers the enfix until the
                next evaluator step.  Doing otherwise would mean that
                `case [1 = 1 [print "good"]] else [print "bad"]` would
                print both `good` and `bad`.
            }
        ]
        did all [
            kept: func [return: [issue!]] [
                kept: null
                return #kept
            ]
            did all [
                var: evaluate/next [kept enibar 304] 'pos
                pos = [enibar 304]
                var == #kept
                null? kept
            ]
        ]
    )(
        enibar: enfix func [return: [integer!] 'i [<skip> integer!]] [
            enibar: null
            return i
        ]
        did all [
            did all [
                var: evaluate/next [1020 enibar 304] 'pos
                pos = [304]
                var == 1020
                null? enibar
            ]
            comment {
                When arguments are not skipped, the behavior should be the
                same as a non-skippable enfix function
            }
        ]
    )
]

; Parameters in-between soft quoted functions (one trying to quote right and
; one trying to quote left) will be processed by the right hand function
; first.
[
    (
        rightq: lambda [:x] [compose [<rightq> was (x)]]
        leftq: enfix lambda [:y] [compose [<leftq> was (y)]]

        [<rightq> was [<leftq> was foo]] = rightq foo leftq
    )(
        rightq: lambda [:x] [compose [<rightq> was (x)]]
        leftq: enfix lambda ['y] [compose [<leftq> was (y)]]

        [<rightq> was [<leftq> was foo]] = rightq foo leftq
    )

    ((1 then x -> [x * 10]) = 10)
]


(
    a: [304]
    a.1: me / 2
    a = [152]
)
