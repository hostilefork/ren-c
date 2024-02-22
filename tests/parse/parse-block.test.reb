; %parse-block.test.reb
;
; BLOCK! is the fundamental rule category of UPARSE, but it's also a combinator.
; This means it can be overridden and hooked.
;
; It is one of the combinators that has to get involved with managing the
; meaning of the `pending` return value, because it splits its contents into
; conceptual "parser alternate groups".  Mere success of one parser in the
; group does not mean it is supposed to add its collected matter to the
; pending list...the entire group must succeed


; No-op rule of empty block should always match.
[
    (void? parse "" [])
    (void? parse "" [[]])
    (void? parse "" [[[]]])

    (void? parse [] [])
    (void? parse [] [[[]]])
    (raised? parse [x] [])
    (raised? parse [x] [[[]]])
    ('x = parse [x] [[] 'x []])
]


; Returns last value
[
    (
        wa: ['a]
        true
    )
    (
        res: 0
        all [
            'a == parse [a] [res: wa]
            res = 'a
        ]
    )
    (
        res: 0
        all [
            'a == parse [a a] [res: repeat 2 wa]
            res = 'a
        ]
    )
]

; | means alternate clause
[
    (raised? parse [a b] ['b | 'a])
    (#a == parse [#a] [[#b | #a]])
    (raised? parse [a b] [['b | 'a]])
    ('b == parse [a b] [['a | 'b] ['b | 'a]])
]


; a BLOCK! rule combined with SET-WORD! will evaluate to the last value-bearing
; result in the rule.  This provides compatibility with the historical idea
; of doing Redbol rules like `set var [integer! | text!]`, but in that case
; it would set to the first item captured from the original input...more like
; `copy data [your rule], (var: first data)`.
;
; https://forum.rebol.info/t/separating-parse-rules-across-contexts/313/6
[
    (2 = parse [1 2] [[integer! integer!]])
    ("a" = parse ["a"] [[integer! | text!]])
]


; A BLOCK! rule is allowed to return NULL, distinct from failure
[
    (
        x: ~
        all [
            null == parse [1] [x: [integer! try text!]]
            x = null
        ]
    )

    (
        x: ~
        all [
            null == parse [1] [integer! x: [(null)]]
            x = null
        ]
    )
]


; INLINE SEQUENCING is the idea of using || to treat everyting on the left
; as if it's in a block.
;
;    ["a" | "b" || "c"] <=> [["a" | "b"] "c"]
[
    ("c" = parse "ac" ["a" | "b" || "c"])
    ("c" = parse "bc" ["a" | "b" || "c"])
    (raised? parse "xc" ["a" | "b" || "c"])

    ("c" = parse "ac" ["a" || "b" | "c"])
    ("b" = parse "ab" ["a" || "b" | "c"])
    (raised? parse "ax" ["a" || "b" | "c"])
]

[
    (
        x: parse "aaa" [some "a" (null)] except [fail "Shouldn't be reached"]
        x = null
    )
    (
        did-not-match: false
        parse "aaa" [some "b"] except [did-not-match: true]
        did-not-match
    )
]


[#1672 (  ; infinite recursion
    x: 0
    a: [(x: x + 1, if x = 200 [throw <deep-enough>]) a]
    <deep-enough> = catch [parse [] a]
)]


[
    (
        res: ~
        all [
            'b == parse [a a b] [<any> res: ['a | 'b] <any>]
            res = 'a
        ]
    )
    (
        res: '~before~
        all [
            raised? parse [a] [res: ['c | 'b]]
            res = '~before~
        ]
    )
]
