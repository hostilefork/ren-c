; %parse-datatype.test.reb
;
; If a TYPE-XXX! is used in a BLOCK! rule, it means the item at the current
; parse position needs to match that type.  It returns the matched value.
;
; If a TYPE-XXX! is used in a text or binary rule, that is interpreted as a
; desire to TRANSCODE the input.
;
; !!! This feature needs more definition, e.g. to be able to transcode things
; that don't end with space or end of input.  For instance, think about how
; to handle the below rule if it was `1020****` and having a `some "*"` rule
; at the tail as well.

(123 = parse "123" [integer!])

[
    (
        res: ~
        all [
            'a == parse [a] [res: word!]
            res = 'a
        ]
    )
    (
        res: ~
        res2: ~
        all [
            'a == parse [a] [res: res2: 'a]
            res = 'a
            res2 = 'a
        ]
    )
]


(
    all [
        let [t i]
        1020 == parse "***{A String} 1020" [some "*", t: text!, i: integer!]
        t = {A String}
        i = 1020
    ]
)

[
    (123 == parse [a 123] ['a integer!])
    ~parse-mismatch~ !! (parse [a 123] ['a char?!])

    (123 == parse [a 123] [['a] [integer!]])
    ~parse-mismatch~ !! (parse [a 123] ['a [char?!]])

    (123 == parse [123] [&any-number?])
    ~parse-mismatch~ !! (parse [123] [&any-string?])

    (123 == parse [123] [[&any-number?]])
    ~parse-mismatch~ !! (parse [123] [[&any-string?]])
]

[
    (
        res: ~
        all [
            3 == parse [a 123] [
                'a (res: 1) [char?! (res: 2) | integer! (res: 3)]
            ]
            res = 3
        ]
    )
    (
        res: ~
        all [
            raised? parse [a 123] [
                'a (res: 1) [char?! (res: 2) | text! (res: 3)]
            ]
            res = 1
        ]
    )
]


; !!! Ren-C is currently letting you put quotes in URLs unescaped; this is
; because browsers render quotes in the URL bar.  For instance, this shows
; quotes in the URL bar:
;
;    https://en.wikipedia.org/wiki/%22Heroes%22_(David_Bowie_album)
;
; And this shows an accented e:
;
;    https://en.wikipedia.org/wiki/Herg%C3%A9
;
; But browsers will escape the quotes when you copy and paste.  This behavior
; is becoming more standard, and the jury is still out on exactly when and
; how transformations of this should be taking place in the language (the
; logic in the browsers is fuzzy.)
;
[https://github.com/red/red/issues/4682

    (
        bin: #{68747470733A2F2F6578616D706C652E6F726722}
        let x
        bin = parse to binary! {https://example.org"} [
            x: across url!
            (assert [{https://example.org"} == as text! to url! x])
        ]
    )
    ({"} == parse to binary! {a@b.com"} [
        let x: across email! (assert [a@b.com == to email! to text! x])
        {"}
    ])
]

[https://github.com/red/red/issues/4678
    ('_ = parse to binary! "_" [blank!])

    ~parse-mismatch~ !! (parse to binary! "#(" [blank!])
    ~parse-mismatch~ !! (parse to binary! "(" [blank!])
    ~parse-mismatch~ !! (parse to binary! "[" [blank!])
]

; QUOTED! needs to be recognized
[
    ((the 'x) == parse ['x] [quoted!])
    ((the '[]) == parse ['''_ ''() '[]] [repeat 3 quoted!])
]

[https://github.com/red/red/issues/4863
    ('word == parse to-binary "word" [word!])
    ('word == parse to-binary "   word" [word!])

    (123 == parse to-binary "123" [integer!])
    ~parse-mismatch~ !! (parse to-binary "123.456" [integer!])
    (123 == parse to-binary "    123" [integer!])

    ([hello 123 world] == parse to-binary "hello 123 world" [
        collect [keep word!, keep integer!, keep word!]
    ])
    ([hello 123 world] == parse to-binary "hello 123 world" [
        collect [keep word!, space, keep integer!, space, keep word!]
    ])
]
