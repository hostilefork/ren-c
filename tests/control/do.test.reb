; functions/control/do.r

; !!! Should DO be able to return void?
[
    (void' = ^(do []))
    (nihil' = ^ (eval []))
    (nihil' = ^(eval []))

    (void? do [])
    (nihil? (eval []))
    (3 = (1 + 2 eval []))
    (3 = (1 + 2 unmeta ^ eval []))

    (''30 = ^ (10 + 20 eval []))
    (void' = ^ (10 + 20 eval [void]))
    (''30 = ^ (10 + 20 eval [comment "hi"]))
    (''30 = ^ (10 + 20 eval make frame! :nihil))

    (didn't do [null])
    ('~[~null~]~ = ^ do [if true [null]])
    (void' = ^ do [if false [<a>]])
    (void' = ^ do [10 + 20 if false [<a>]])

    (all [
        x: <overwritten>
        nihil' = x: ^ comment "HI" comment "HI"  ; not eval'd in same step
        x = nihil'
    ])

    (all [
        x: <overwritten>
        void' = (x: ^(comment "HI") ^ do [comment "HI"])
        nihil' = x
    ])

    (nihil' = (10 + 20 ^(eval [])))
    (nihil' = (10 + 20 ^(eval [comment "hi"])))
    (void' = (10 + 20 ^(eval make frame! lambda [] [void])))
    (null' = ^(eval [null]))
    ('~[~null~]~ = ^(eval [if true [null]]))

    (30 = (10 + 20 eval []))
    (30 = (10 + 20 eval [comment "hi"]))
    (30 = (10 + 20 eval make frame! :nihil))
    (null' = ^(eval [null]))
    ('~[~null~]~ = ^ eval [heavy null])
    ('~[~null~]~ = ^ eval [if true [null]])

    ; Try standalone ^ operator so long as we're at it.
    (nihil' = ^ eval [])
    (nihil' = ^ eval [comment "hi"])
    (nihil' = ^ eval make frame! :nihil)
    (void' = ^ do [void])

    (null' = ^ eval [null])
    (null' = ^(eval [null]))
    (null' = ^ (eval [null]))
    (null' = meta eval [null])

    ('~[~null~]~ = ^ eval [heavy null])
    ('~[~null~]~ = ^(eval [heavy null]))
    ('~[~null~]~ = ^ (eval [heavy null]))
    ('~[~null~]~ = meta eval [heavy null])

    ('~[~null~]~ = ^ eval [if true [null]])
]


[
    (''3 = ^ (1 + 2 eval [comment "HI"]))
    (nihil' = ^ eval [comment "HI"])

    (3 = (1 + 2 eval [comment "HI"]))
    (nihil? eval [comment "HI"])

    (
        y: <overwritten>
        x: (1 + 2 y: (void eval [comment "HI"]))
        all [
            void? x
            voided? $y
        ]
    )
]

(
    success: false
    do [success: true]
    success
)
(
    a-value: to binary! "Rebol [] 1 + 1"
    2 == do a-value
)
; do block start
(:abs = do [:abs])
(
    a-value: #{}
    same? a-value do reduce [a-value]
)
(
    a-value: charset ""
    same? a-value do reduce [a-value]
)
(
    a-value: []
    same? a-value do reduce [a-value]
)
(same? blank! do reduce [blank!])
(1/Jan/0000 = do [1/Jan/0000])
(0.0 == do [0.0])
(1.0 == do [1.0])
(
    a-value: me@here.com
    same? a-value do reduce [a-value]
)
(error? do [trap [1 / 0]])
(
    a-value: %""
    same? a-value do reduce [a-value]
)
(
    a-value: does []
    same? :a-value do [:a-value]
)
(
    a-value: first [:a-value]
    :a-value == do reduce [:a-value]
)
(NUL == do [NUL])

(0 == do [0])
(1 == do [1])
(#a == do [#a])
(
    a-value: first ['a/b]
    :a-value == do [:a-value]
)
(
    a-value: first ['a]
    :a-value == do [:a-value]
)
(~true~ == do [~true~])
(~false~ == do [~false~])
($1 == do [$1])
(same? :append do [:append])
(null? do [~null~])
(
    a-value: make object! []
    same? :a-value do reduce [:a-value]
)
(
    a-value: first [()]
    same? :a-value do [:a-value]
)
(same? get $+ do [get $+])
(0x0 == do [0x0])
(
    a-value: 'a/b
    :a-value == do [:a-value]
)
(
    a-value: make port! http://
    port? do reduce [:a-value]
)
(/a == do [/a])
(
    a-value: first [a/b:]
    :a-value == do [:a-value]
)
(
    a-value: first [a:]
    :a-value == do [:a-value]
)
(
    a-value: ""
    same? :a-value do reduce [:a-value]
)
(
    a-value: make tag! ""
    same? :a-value do reduce [:a-value]
)
(0:00 == do [0:00])
(0.0.0 == do [0.0.0])
(void' = ^ do [()])
('a == do ['a])

; !!! At time of writing, DO of an ERROR! is like FAIL; it is not definitional,
; and can only be caught with SYS.UTIL.RESCUE.  Should it be?  Or should a
; DO of an ERROR! just make it into a definitional error?
;
~zero-divide~ !! (error? trap [do trap [1 / 0] 1])

(
    a-value: first [(2)]
    2 == do as block! :a-value
)
(
    a-value: "Rebol [] 1"
    1 == do :a-value
)
(void? do "Rebol []")
(1 = do "Rebol [] 1")
(3 = do "Rebol [] 1 2 3")

; RETURN stops the evaluation
(
    f1: func [return: [integer!]] [do [return 1 2] 2]
    1 = f1
)
; THROW stops evaluation
(
    1 = catch [
        do [
            throw 1
            2
        ]
        2
    ]
)
; BREAK stops evaluation
(
    null? repeat 1 [
        do [
            break
            2
        ]
        2
    ]
)

; evaluate block tests
(
    success: false
    evaluate/next [success: true success: false] $pos
    success and (pos = [success: false])
)
(
    value: evaluate/next [1 2] $b
    all [
        1 = value
        [2] = b
    ]
)
(
    all [
        nihil? evaluate/next [] $pos
        pos = null
    ]
)
(
    value: evaluate/next [trap [1 / 0]] $pos
    all [
        error? value
        pos = []
    ]
)
(
    f1: func [return: [integer!]] [evaluate [return 1 2] 2]
    1 = f1
)
; recursive behaviour
(1 = do [do [1]])
(1 = do "Rebol [] do [1]")
(1 == 1)
(3 = reeval unrun :reeval unrun :add 1 2)
; infinite recursion for block
(
    x: 0
    blk: [x: x + 1, if x = 2000 [throw <deep-enough>] do blk]
    <deep-enough> = catch blk
)

; This was supposed to test infinite recursion in strings, but it doesn't
; work using module isolation.  Review.
;
[#1896
    ~unassigned-attach~ !! (
        str: "Rebol [] do str"
        do str
    )
]

; infinite recursion for evaluate
(
    x: 0
    blk: [x: x + 1, if x = 2000 [throw <deep-enough>] b: evaluate blk]
    <deep-enough> = catch blk
)

; evaluating quoted argument
(
    rtest: lambda ['op [word!] 'thing] [reeval op thing]
    -1 = rtest negate 1
)
