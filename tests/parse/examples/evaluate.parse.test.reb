; %evaluate.parse.test.reb
;
; Reimagination of R3-Alpha's DO, which let you treat the input block as code
; and advance through a step of evaluation.
;
; https://forum.rebol.info/t/replacing-r3-parses-do-rule-w-parses-evaluate/1478

[
    (parse-evaluate: combinator [
        {Run the evaluator one step to advance input, and produce a result}
        return: "Result of one evaluation step"
            [any-value?]
    ][
        if tail? input [return raise "PARSE-EVALUATE attempted at series tail"]

        return evaluate/next input $remainder
    ]
    true)

(
    keeper-saver: func [
        return: [block!]
        @saved [block!]
        input [block!]
        <local> mode value
    ][
        saved: copy []

        mode: #save
        return parse input [collect [
            some [
                mode: ['<K> (#keep) | '<S> (#save) | tag! (fail "BAD MODE")]
                |
                [',]  ; skip over commas
                |
                [value: parse-evaluate] [
                    :(mode = #keep) keep (value)
                    |
                    :(mode = #save) (if did value [append saved value])
                ]
            ]
        ]]
    ]
    true
)

    (all [
        [35 13 23] = [k s]: keeper-saver [
            1 + 2
            <K> (3 + 4) * 5 if true [6 + 7]
            <S> 7 + 8, if false [9 + 10] else ["save me!"] <K>, 11 + 12
        ]
        k = [35 13 23]
        s = [3 15 "save me!"]
    ])
]
