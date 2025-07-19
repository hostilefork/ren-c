; %parse-group.test.reb
;
; GROUP! are value-bearing rules that do not advance the input and get their
; argument literally from a EVAL evaluation.  They always succeed.

(10 = parse [aaa] ['aaa (10)])
~parse-incomplete~ !! (parse [aaa] [(10)])

(
    three: 3
    all [
        3 == parse "" [x: (three)]
        x = 3
    ]
)
(
    all [
        3 == parse "" [x: (1 + 2)]
        x = 3
    ]
)

('z = parse [x z] ['x (assert [true]) 'z])
