; functions/control/switch.r
(
    11 = switch 1 [
        1 [11]
        2 [12]
    ]
)
(
    12 = switch 2 [
        1 [11]
        2 [12]
    ]
)

((meta null) = ^ switch 2 [1 []])
('~[~null~]~ = ^ switch 1 [1 [null]])

(
    cases: reduce [1 head of insert copy [] trap [1 / 0]]
    error? switch 1 cases
)

[#2242 (
    11 = run func [return: [integer!]] [switch:all 1 [1 [return 11 88]] 99]
)]

(t: 1, 1 = switch t [(t)])
(1 = switch 1 [1])


; SWITCH/ALL gives the last branch result, but prioritizes fallout

(<b> = switch:all 10 [5 + 5 [<a>] 5 + 5 [<b>]])
(<b> = switch:all 10 [0 + 0 [<a>] 5 + 5 [<b>]])
(<a> = switch:all 10 [5 + 5 [<a>] 0 + 0 [<b>]])
((meta null) = ^ switch:all 10 [0 + 0 [<a>] 0 + 0 [<b>]])

(<fallout> = switch:all 10 [5 + 5 [<a>] 5 + 5 [<b>] <fallout>])
(<fallout> = switch:all 10 [0 + 0 [<a>] 5 + 5 [<b>] <fallout>])
(<fallout> = switch:all 10 [5 + 5 [<a>] 0 + 0 [<b>] <fallout>])
(<fallout> = switch:all 10 [0 + 0 [<a>] 0 + 0 [<b>] <fallout>])


; New feature for specifying comparison functions

(<yep> = switch:predicate 10 [20 [<nope>] 5 [<yep>]] :greater?)

; New feature for doing typeset matches
[
    ("COOL!" = switch:type <asdf> [any-string?/ ["COOL!"]])
    ("COOL!" = switch:type "asdf" [any-string?/ ["COOL!"]])

    (null? switch 1020 [any-string?/ [panic ~<unreachable>~]])
]
