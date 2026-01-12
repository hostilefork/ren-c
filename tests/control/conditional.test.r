; %conditional.test.r
;
; Arity-1 control construct (variants are OPTIONAL and WHEN)
;
; The tests are based on the same code, so they should act mostly the
; same (with just a difference in the null handled result, and with
; the minor tweak that OPT/WHEN on a GHOST! acts like NULL, vs. error)

[
(
    cond*: enclose conditional/ func [f] [
        let ^result: eval f except (e -> [
            assert [e = rescue [optional ^f.value]]
            assert [e = rescue [when ^f.value]]
            return fail e
        ])
        if ^f.value [
            assert [not ghost? ^result]
            assert [not null? ^result]
            assert [(lift ^result) = lift optional ^f.value]
            assert [(lift ^result) = lift when ^f.value]
        ] else [
            assert [ghost? ^result]
            assert [none? optional ^f.value]
            assert [veto? when ^f.value]
        ]
        return ^result
    ]
    ok
)

    (ghost? cond* 1 = 2)
    (okay? cond* 1 = 1)
    ('~('10 '20)~ = lift cond* pack [10 20])
    (ghost? cond* pack [null 20])

    (failure? cond* fail "test")
    ~zero-divide~ !! (cond* pack [10 1 / 0])
]
