; functions/control/if.r
(
    success: false
    if true [success: true]
    success
)
(
    success: true
    if false [success: false]
    success
)
(1 = if true [1])

(void' = ^ if false [])
('~[']~ = ^ if true [])

(error? if true [trap [1 / 0]])
; RETURN stops the evaluation
(
    f1: func [return: [integer!]] [
        if true [return 1 2]
        2
    ]
    1 = f1
)
; condition datatype tests; action
(if get @abs [true])
; binary
(if #{00} [true])
; bitset
(if make bitset! "" [true])

; literal blocks were once illegal in Ren-C as conditions, but that check
; was based on CELL_FLAG_UNEVALUATED, which was removed when blocks became
; "evaluative" for their bindings.
[
    (if [] [true])
    (if identity [] [true])
]

; datatype
(if blank! [true])
; typeset
(if any-number! [true])
; date
(if 1/1/0000 [true])
; decimal
(if 0.0 [true])
(if 1.0 [true])
(if -1.0 [true])
; email
(if me@rt.com [true])
(if %"" [true])
(if does [] [true])
(if first [:first] [true])
; integer
(if 0 [true])
(if 1 [true])
(if -1 [true])
(if #a [true])
(if first ['a/b] [true])
(if first ['a] [true])
(if true [true])
(void' = ^ if false [true])
(if $1 [true])
(if (specialize :of [property: 'type]) [true])
(true = if blank [true])
(if make object! [] [true])
(if get @+ [true])
(if 0x0 [true])
(if first [()] [true])
(if 'a/b [true])
(if make port! http:// [true])
(if /a [true])
(if first [a/b:] [true])
(if first [a:] [true])
(if "" [true])
(if to tag! "" [true])
(if 0:00 [true])
(if 0.0.0 [true])
(if  http:// [true])
(if 'a [true])

; recursive behaviour

('~[']~ = ^ if true [if false [1]])
(void? if true [2 if false [1]])
(1 = if true [if true [1]])

; infinite recursion
(<deep-enough> = catch [
    depth: 0
    blk: [depth: me + 1, if depth = 1000 [throw <deep-enough>], if true (blk)]
    do blk
])

(
    success: false
    if not false [success: true]
    success
)
(
    success: true
    if not true [success: false]
    success
)
(1 = if not false [1])

(void' = ^ if not true [1])
('~[~null~]~ = ^ if not false [null])

(error? if not false [trap [1 / 0]])

; RETURN stops the evaluation
(
    f1: func [return: [integer!]] [
        if not false [return 1 2]
        2
    ]
    1 = f1
)

; Soft Quoted Branching
; https://forum.rebol.info/t/soft-quoted-branching-light-elegant-fast/1020
(
    [1 + 2] = if true '[1 + 2]
)(
    1020 = if true '1020
)

; THE-XXX! Branching
;
; !!! At one point this allowed cases of @word, @pa/th, and @tu.p.le as a
; shorthand for `if xxx [:word]` etc.
;
;    j: 304
;    304 = if true @j
;
;    o: make object! [b: 1020]
;    1020 = if true @o/b
;
; Might be good for code golf?  (Even better would be to wedge in tailored
; support for plain WORD! etc.)
[(
    x: ~
    did all [
        <else> = if true [x: <branch>, null] *else [<else>]
        x = <branch>
    ]
)

; Feature being considered: @(...) groups do not run unconditionally to make
; branch arguments, but only if the branch is taken.
;(
;    var: <something>
;    did all [
;        void? if false @(var: <something-else> [null])
;        var = <something>
;        null? if true @(var: <something-else> [null])
;        var = <something-else>
;    ]
;)
]
