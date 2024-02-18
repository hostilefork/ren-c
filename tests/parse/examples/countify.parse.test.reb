; %countify.parse.test.reb
;
; This was a strange little function that was written as a demo of how parse
; rules could be generated and used.  So long as it was written off the cuff
; for that demo, it was turned into a test.

[(
    countify: func [things data] [
        let counts: make map! []
        let rules: collect [
            for-each t things [
                counts.(t): 0
                keep t
                keep compose/deep $(counts.(t): me + 1)
                keep/line '|
            ]
            keep 'false
        ]
        parse data (compose/deep [
            try some [(spread rules)]  ; could also be `try some [rules]`
        ]) except [
            return <outlier>
        ]
        return collect [
            for-each [key value] counts [
                keep key
                keep value
            ]
        ]
    ]
    true
)(
    ["a" 3 "b" 3 "c" 3] = countify ["a" "b" "c"] "aaabccbbc"
)(
    <outlier> = countify ["a" "b" "c"] "aaabccbbcd"
)]
