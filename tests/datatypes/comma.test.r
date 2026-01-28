; comma.test.r
;
; COMMA! has the unusual rendering behavior of sticking to the thing on its
; left hand side.  It can be used in dialects for arbitrary purposes, but the
; thing it is used for in PARSE and the evaluator is to mark interstitial
; points between expressions.  This helps for readability, and it is
; enforced by errors if the comma turns out to be mid-expressions

(comma? first [,])
(comma? ',)

; Despite rendering with their left hand side, commas are part of the count.
;
(3 = length of [a, b])

; If [(#)] is valid, then [#,] needs to have parity
;
(2 = length of load "#,")

; Commas that occur at interstitials are legal
;
(7 = all [1 + 2, 3 + 4])

; Commas during argument gathering look like end of input
;
~no-arg~ !! (
    all [1 +, 2 3 + 4]
)

; Commas are invisible and hence do not erase an evaluation value
;
(3 = eval [1 + 2,])

; There must be space or other delimiters after commas.
(
    for-each [text] ["a,b" "a,, b" ",,"] [
        e: rescue [load text]
        assert [e.id = 'scan-invalid]
    ]
    ok
)

; Due to the spacing rule, the traditional "comma means decimal point" rule
; is still able to work--though it is less desirable for it to do so.
;
(1.1 = transcode:one "1,1")

; R3-Alpha's PARSE implementation was not particularly orderly in terms of
; holding the state of expressions, so detecting the interstitial points is
; a bit of a hack...but it's in as a proof of concept pending a better
; PARSE reorganization.
;
("aaabbb" = parse3 "aaabbb" [some "a", some "b"])
~expression-barrier~ !! (
    "aaabbb" = parse3 "aaabbb" [some, "a" some "b"]
)

; Commas are "hard delimiters", so they won't be picked up in URL!
(
    commafied: [a, 1, <b>, #c, %def, http://example.com, "test",]
    normal: [a 1 <b> #c %def http://example.com "test"]

    remove-each 'x commafied [comma? x]
    commafied = normal
)

; We want commas to behave the same as reaching the end of a feed
;
; (At one time this was relaxed for literal parameters; maybe <comma> should
; specifically mean that you want to get commas literally?  TBD.)

[
    (test-normal: lambda [x [<hole> any-value?]] [
        reduce [lift x, boolean hole? $x]
    ])

    ([~null~ true] = test-normal,)
    ([~null~ true] = test-normal)
    ([~null~ true] = (test-normal))

    ([~null~ false] = test-normal null)

    ~???~ !! (test-normal ())
]

[
    (test-meta: lambda [^x [<hole> any-value?]] [
        reduce [lift ^x, boolean hole? $x]
    ])

    ([~null~ true] = test-meta,)
    ([~null~ true] = test-meta)
    ([~null~ true] = (test-meta))

    ([~null~ false] = test-meta null)

    ([~ false] = test-meta ())
]

[
    (test-literal: lambda [@x [<hole> any-element?]] [
        reduce [lift ^x, boolean hole? $x]
    ])

    ([~null~ true] = test-literal,)
    ([~null~ true] = test-literal)
    ([~null~ true] = (test-literal))

    (['null false] = test-literal null)

    (['() false] = test-literal ())
]

; When a COMMA! is hit in evaluation, it holds up the pipeline until the
; function fulfillment is complete...so several arguments can become holes
(
    test-two-holes: lambda [x [<hole> integer!] y [<hole> integer!]] [
        reduce [boolean hole? $x, boolean hole? $y]
    ]
    all {
        result: ~
        1020 = eval [result: test-two-holes, 1020]
        result = [true true]
    }
)

~need-non-end~ !! (x:,)
