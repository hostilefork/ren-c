; functions/control/either.r
(
    either true [success: true] [success: false]
    success
)
(
    either false [success: false] [success: true]
    success
)
(1 = either true [1] [2])
(2 = either false [1] [2])

('~[~null~]~ = ^ either true [null] [1])
('~[~null~]~ = ^ either false [1] [null])

(error? either true [trap [1 / 0]] [])
(error? either false [] [trap [1 / 0]])

; RETURN stops the evaluation
(
    f1: func [return: [integer!]] [
        either true [return 1 2] [2]
        2
    ]
    1 = f1
)
(
    f1: func [return: [integer!]] [
        either false [2] [return 1 2]
        2
    ]
    1 = f1
)
; THROW stops the evaluation
(
    1 == catch [
        either true [throw 1 2] [2]
        2
    ]
)
(
    1 == catch [
        either false [2] [throw 1 2]
        2
    ]
)
; BREAK stops the evaluation
(
    null? repeat 1 [
        either true [break 2] [2]
        2
    ]
)
(
    null? repeat 1 [
        either false [2] [break 2]
        2
    ]
)

[
    ; Recursive behavior

    (2 = either true [either false [1] [2]] [])
    (1 = either false [] [either true [1] [2]])

    ; Infinite recursion

    (<deep-enough> = catch [
        depth: 0
        do blk: [
            depth: me + 1
            if depth = 1000 [throw <deep-enough>]
            either true (blk) []
        ]
    ])
    (<deep-enough> = catch [
        depth: 0
        do blk: [
            depth: me + 1
            if depth = 1000 [throw <deep-enough>]
            either false [] (blk)
        ]
    ])
]

[
    ; This exercises "deferred typechecking"; even though it passes through a
    ; step where there is a void in the condition slot, that's not the final
    ; situation since the equality operation will be run later, so the test
    ; has to wait.

    (
        takes-2-logics: func [x [logic?] y [logic?]] [return x]
        infix-voider: enfix func [return: [quasi-word?] x y] [
            return '~bad~
        ]
        true
    )

    (takes-2-logics ('~bad~) = '~bad~ false)

    ~expect-arg~ !! (takes-2-logics true infix-voider true false)
]

; Soft Quoted Branching
; https://forum.rebol.info/t/soft-quoted-branching-light-elegant-fast/1020
(
    [1 + 2] = either true '[1 + 2] [3 + 4]
)(
    7 = either false '[1 + 2] [3 + 4]
)(
    1020 = either true '1020 '304
)

; THE-XXX! Branching (TBD)
;(
;    j: 304
;    304 = either true @j [fail "Shouldn't run"]
;)(
;    o: make object! [b: 1020]
;    1020 = either true @o/b [fail "Shouldn't run"]
;)(
;    var: <something>
;    did all [
;        304 = either false @(var: <something-else> [1000 + 20]) [300 + 4]
;        var = <something>
;        1020 = if true @(var: <something-else> [1000 + 20]) [300 + 4]
;        var = <something-else>
;    ]
;)
