; %parse-binary.test.reb
;
; This is generally intended for when the parse rule is a BINARY!, but other
; misc tests related to binary input can go here too.

; General BINARY! matching
[
    (void? parse #{} [])

    (#{0A} == parse #{0A} [#{0A}])
    (#"^/" == parse #{0A} [#"^/"])
    (raised? parse #{0A} [#{0B}])
    (#{0B} == parse #{0A0B} [#{0A} #{0B}])
    (#{0A0B} == parse #{0A0B} [#{0A0B}])
    (#{0A} == parse #{0A} [[#{0A}]])
    (#{0B} == parse #{0A0B} [[#{0A}] #{0B}])
    (#{0B} == parse #{0A0B} [#{0A} [#{0B}]])
    (#{0B} == parse #{0A0B} [[#{0A}] [#{0B}]])
    (#{0A} == parse #{0A} [#{0B} | #{0A}])
    (raised? parse #{0A0B} [#{0B} | #{0A}])
    (#{0A} == parse #{0A} [[#{0B} | #{0A}]])
    (raised? parse #{0A0B} [[#{0B} | #{0A}]])
    (#{0B} == parse #{0A0B} [[#{0A} | #{0B}] [#{0B} | #{0A}]])
    (
        res: 0
        all [
            1 == parse #{} [(res: 1)]
            res = 1
        ]
    )
    (
        res: 0
        all [
            1 == parse #{0A} [#{0A} (res: 1)]
            res = 1
        ]
    )
    (
        res: 0
        all [
            raised? parse #{0A} [#{0B} (res: 1)]
            res = 0
        ]
    )
    (
        res: 0
        all [
            1 == parse #{} [[(res: 1)]]
            res = 1
        ]
    )
    (
        res: 0
        all [
            1 == parse #{0A} [[#{0A} (res: 1)]]
            res = 1
        ]
    )
    (
        res: 0
        all [
            raised? parse #{0A} [[#{0B} (res: 1)]]
            res = 0
        ]
    )
    (
        res: 0
        all [
            3 == parse #{0A0B} [
                #{0A} (res: 1) [#"^L" (res: 2) | #{0B} (res: 3)]
            ]
            res = 3
        ]
    )
    (
        res: 0
        all [
            raised? parse #{0A0B} [#{0A} (res: 1) [#{0C} (res: 2) | #{0D} (res: 3)]]
            res = 1
        ]
    )
    (raised? parse #{0A0A} [repeat 1 [#{0A}]])
    (#{0A} == parse #{0A0A} [repeat 2 [#{0A}]])
    (raised? parse #{0A0A} [repeat 3 [#{0A}]])

    (raised? parse #{0A0A} [repeat ([1 1]) [#{0A}]])
    (#{0A} == parse #{0A0A} [repeat ([1 2]) [#{0A}]])
    (#{0A} == parse #{0A0A} [repeat ([2 2]) [#{0A}]])
    (#{0A} == parse #{0A0A} [repeat ([2 3]) [#{0A}]])
    (raised? parse #{0A0A} [repeat ([3 4]) [#{0A}]])
    (raised? parse #{0A0A} [repeat ([1 1]) #{0A}])
    (#{0A} == parse #{0A0A} [repeat ([1 2]) #{0A}])
    (#{0A} == parse #{0A0A} [repeat ([2 2]) #{0A}])
    (#{0A} == parse #{0A0A} [repeat ([2 3]) #{0A}])
    (raised? parse #{0A0A} [repeat ([3 4]) #{0A}])
    (raised? parse #{0A0A} [repeat ([1 1]) <any>])
    (10 == parse #{0A0A} [repeat ([1 2]) <any>])
    (10 == parse #{0A0A} [repeat ([2 2]) <any>])
    (10 == parse #{0A0A} [repeat ([2 3]) <any>])
    (raised? parse #{0A0A} [repeat ([3 4]) <any>])

    (raised? parse #{0A0A} [repeat 1 #{0A}])
    (#{0A} == parse #{0A0A} [repeat 2 #{0A}])
    (raised? parse #{0A0A} [repeat 3 #{0A}])
    (raised? parse #{0A0A} [repeat 1 <any>])
    (10 == parse #{0A0A} [repeat 2 <any>])
    (raised? parse #{0A0A} [repeat 3 <any>])
    (10 == parse #{0A} [<any>])
    (11 == parse #{0A0B} [<any> <any>])
    (11 == parse #{0A0B} [<any> [<any>]])
    (11 == parse #{0A0B} [[<any>] [<any>]])
    (#{0A} == parse #{0A0A} [some [#{0A}]])
    (raised? parse #{0A0A} [some [#{0A}] #{0B}])
    (10 == parse #{0A0A0B0A0B0B0B0A} [some [<any>]])
    (#{0A} == parse #{0A0A0B0A0B0B0B0A} [some [#{0A} | #{0B}]])
    (raised? parse #{0A0A0B0A0B0B0B0A} [some [#{0A} | #"^L"]])
    (#{0A} == parse #{0A0A} [some [#{0A}]])
    (null = parse #{0A0A} [some [#{0A}] try some [#{0B}]])
    (#{0B} == parse #{0A0A0B0B} [repeat 2 #{0A} repeat 2 #{0B}])
    (raised? parse #{0A0A0B0B} [repeat 2 #{0A} repeat 3 #{0B}])
    (#{0B} == parse #{0A0A0B0B} [some #{0A} some #{0B}])
    (raised? parse #{0A0A0B0B} [some #{0A} some #"^L"])
    (#"^L" == parse #{0B0A0A0A0C} [<any> some [#{0A}] #"^L"])
]


[#394 (
    #{22} == parse #{001122} [#{00} #{11} #{22}]
)]

; "BINARY! extraction"
[
    (
        res: ~
        all [
            10 == parse #{0A} [res: <any>]
            res = 10
        ]
    )
    (
        res: ~
        all [
            #{0A} == parse #{0A} [res: #{0A}]
            res = #{0A}
        ]
    )
    (
        res: ~
        res2: ~
        all [
            #{0A} == parse #{0A} [res: res2: #{0A}]
            res = #{0A}
            res2 = #{0A}
        ]
    )
    (
        res: ~
        all [
            #{0A} == parse #{0A0A} [res: repeat 2 #{0A}]
            res = #{0A}
        ]
    )
    (
        res: '~before~
        all [
            raised? parse #{0A0A} [res: repeat 3 #{0A}]
            res = '~before~
        ]
    )
    (
        res: ~
        all [
            #{0A} == parse #{0A} [res: [#{0A}]]
            res = #{0A}
        ]
    )
    (
        wa: [#{0A}]
        res: ~
        all [
            #{0A} == parse #{0A} [res: wa]
            res = #{0A}
        ]
    )
    (
        wa: [#{0A}]
        res: ~
        all [
            #{0A} == parse #{0A0A} [res: repeat 2 wa]
            res = #{0A}
        ]
    )
    (
        wa: [#{0A}]
        res: ~
        all [
            11 == parse #{0A0A0B} [<any> res: #{0A} <any>]
            res = #{0A}
        ]
    )
    (
        res: ~
        all [
            11 == parse #{0A0A0B} [<any> res: [#{0A} | #{0B}] <any>]
            res = #{0A}
        ]
    )
    (
        res: '~before~
        all [
            raised? parse #{0A} [res: [#"^L" | #{0B}]]
            res = '~before~
        ]
    )
    (
        res: ~
        all [
            #"^L" == parse #{0B0A0A0A0C} [<any> res: some #{0A} #"^L"]
            res = #{0A}
        ]
    )
    (
        wa: [#{0A}]
        res: ~
        all [
            #"^L" == parse #{0B0A0A0A0C} [<any> res: some wa #"^L"]
            res = #{0A}
        ]
    )
]
