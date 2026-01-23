; %for-parallel.loop.test.r
;
; Function requested by @gchiu, serves as another test of loop composition.

[
    (for-parallel: lambda [
        vars [block!]
        ^blk1 [<void> any-series?]
        ^blk2 [<void> any-series?]
        body [block!]
    ] {
        [vars context]: wrap:set compose vars
        body: overbind context body
        until [(empty? ^blk1) and (empty? ^blk2)] [
            (vars): pack [^blk1.1 ^blk2.1]

            attempt body else [
                break  ; if pure NULL it was a BREAK
            ]

            ; They either did a CONTINUE the ATTEMPT caught, or body reached
            ; the end.  ELIDE the increment, so body evaluation is result.
            ;
            elide try blk1: next blk1
            elide try blk2: next blk2
        ]
    }, ok)

    (heavy-void? for-parallel [x y] [] [] [panic])
    ([1 2] = collect [for-parallel [^x y] [] [1 2] [keep ^x, keep y]])
    ([a b] = collect [for-parallel [x ^y] [a b] [] [keep x, keep ^y]])

    ((lift null) = lift for-parallel [x y] [a b] [1 2] [if x = 'b [break]])
    ('~(~null~)~ = lift for-parallel [x y] [a b] [1 2] [null])

    ('z = for-parallel [x y] [a b] [1 2] [if x = 'b [continue 'z]])
    ([a b 2] = collect [
        for-parallel [x y] [a b] [1 2] [keep x if y = '1 [continue] keep y]
    ])

    ([[a 1] [b 2]] = collect [
        assert [
            20 = for-parallel [x y] [a b] [1 2] [
                keep reduce [x y]
                y * 10
            ]
        ]
    ])

    ([[a 1] [b 2] [c ~,~]] = collect [
        assert [
            <exhausted> = for-parallel [x ^y] [a b c] [1 2] [
                keep reduce [x reify ^y]
                if :y [y * 10] else [<exhausted>]
            ]
        ]
    ])

    ([[a 1] [b 2] [~,~ 3]] = collect [
        assert [
            30 = for-parallel [^x y] [a b] [1 2 3] [
                keep reduce [reify ^x y]
                y * 10
            ]
        ]
    ])
]
