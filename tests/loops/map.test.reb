; %loops/map.test.reb
;
; MAP is a new generalized form of loop which implicitly collects the result
; of running its body branch.  The body branch is collected according to the
; same rules as APPEND: blocks are spliced, quoted items have a quote level
; removed then are collected as-is, inert items are collected as-is,
; evaluative items trigger an error, and blanks opt-out.  The exception is
; that NULL is treated the same as blank, opting out of the result.
;
; MAP-EACH is a legacy construct which is equivalent to running MAP with a
; ^META branch: items are collected as-is, except for NULL which opts out.

; "return bug"
(
    integer? reeval does [map-each 'v [] [] 1]
)

; PATH! is immutable, but MAP should work on it
(
    [a 1 b 1 c 1] = map 'x each 'a/b/c [spread reduce [x 1]]
)

; BLANK! is legal for slots you don't want to name variables for:
[(
    [5 11] = map [_ a b] each [1 2 3 4 5 6] [a + b]
)]

; ACTION!s are called repeatedly util NULL is returned
(
    /make-one-thru-five: func [
        return: [~null~ integer!]
    ]
    bind construct [count: 0] [
        if count = 5 [return null]
        return count: count + 1
    ]
    [10 20 30 40 50] = map 'i :make-one-thru-five [
        i * 10
    ]
)(
    /make-one-thru-five: func [
        return: [~null~ integer!]
    ]
    bind construct [count: 0] [
        if count = 5 [return null]
        return count: count + 1
    ]
    [[1 2] [3 4] [5]]  = map [a b] :make-one-thru-five [
        compose $() [(a) (maybe b)]
    ]
)

; MAP uses APPEND rules, so SPREAD works
[
    ([1 <haha!> 2 <haha!>] = map 'x each [1 2] [spread reduce [x <haha!>]])
    ([1 <haha!> 2 <haha!>] = map 'x each [1 2] [spread :[x <haha!>]])

    ([[1 <haha!>] [2 <haha!>]] = map 'x each [1 2] [:[x <haha!>]])
    (['[1 <haha!>] '[2 <haha!>]] = map 'x each [1 2] ^[:[x <haha!>]])

    ; void opts out, as a twist on APPEND's rule disallowing it.
    ;
    ([[1 <haha!>] [3 <haha!>]] = map 'x each [1 2 3] [
        if x <> 2 [:[x <haha!>]]
    ])
]

; ^META blocks collect the meta variation of the evaluation, including voids
[
    (['1 ~void~ '3] = map 'x each [1 2 3] ^[if x <> 2 [x]])
    (['1 ~[~void~]~ '3] = map 'x each [1 2 3] ^[if x = 2 [void] else [x]])
]

; MAP-EACH works with ANY-CONTEXT? now
(
    [x 10 y 20] = map-each [key val] make object! [x: 10 y: 20] [
        spread reduce [key val]
    ]
)

; BLANK! acts same as empty block, void opts out and generates BREAK signal
[
    ([] = map-each 'x [] [fail])
    ([] = map-each 'x _ [fail])
    (null? map-each 'x ~void~ [fail])

    ~expect-arg~ !! (map-each 'x '~ [fail])
    ~expect-arg~ !! (map-each 'x ~ [fail])
]
