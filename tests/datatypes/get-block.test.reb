; %get-block.test.reb
;
; GET-BLOCK! really just calls REDUCE at the moment.  So instead of saying:
;
;     >> repend [a b c] spread [1 + 2 3 + 4]
;     == [a b c 3 7]
;
; You can write:
;
;     >> append [a b c] spread :[1 + 2 3 + 4]
;     == [a b c 3 7]
;

(get-block! = kind of first [:[a b c]])
(get-tuple! = kind of first [:[a b c].d])

(
    a: 10 b: 20
    [10 20] = :[a b]
)

([a b c 3 7] = append [a b c] spread :[1 + 2 3 + 4])


; If used as a branch, the REDUCE only happens if the branch should run.
[(
    x: ~
    y: <y>
    z: if true :[elide x: <x>, 1 + 2 3 + 4] else :[elide y: ~, 3 + 4, 1 + 2]
    all [
        z = [3 7]
        x = <x>
        y = <y>
    ]
)]
