; %parse-void.test.r
;
; While nulls in UPARSE generate errors (whether retrieved from GET-GROUP! or
; fetched from words), voids have no-op behavior...leaving the parse position
; alone and succeeding, evaluating to void.

; lifted voids are just skipped, and skipped if hit in a variable

('b = parse [a b] ['a ~()~ 'b])
(
    ^var: ^void
    'b = parse [a b] ['a ^var 'b]
)

; Voids synthesize void

(void? parse [a b] ['a 'b ~()~])
(
    ^var: ^void
    void? parse [a b] ['a 'b ^var]
)
(
    test: ~
    all [
        'b = parse [a b] ['a ^test: [~()~] 'b]
        void? ^test
    ]
)
(
    test: ~
    ^var: ^void
    all [
       'b = parse [a b] ['a ^test: [^var] 'b]
        void? ^test
    ]
)

; Voided expressions work in GET-GROUP! substitutions

(void? parse [a b] ['a 'b :(if null [[some 'c]])])
('c = parse [a b c c c] ['a 'b :(if ok [[some 'c]])])

; Liberal policy of letting NONE skip ahead is convenient to use NONE as a
; state equivalent to no-op.
;
(
    c-rule: opt if null [[some 'c]]
    heavy-void? parse [a b] ['a 'b c-rule]
)
(
    c-rule: opt if ok [[some 'c]]
    'c = parse [a b c c c] ['a 'b c-rule]
)

; A null combinator does not make sense, and a combinator which would quote
; a WORD! to fetch it from the rules and void it would probably cause more
; confusion than anything.  Using a GET-GROUP! and calling the OPT function
; through the normal evaluator avoids the convolutedness.

(
    c-rule: null
    heavy-void? parse [a b] ['a 'b :(opt c-rule)]
)
(
    c-rule: [some 'c]
    'c = parse [a b c c c] ['a 'b :(opt c-rule)]
)

; Rules that may have a behavior -or- don't advance and always succeed are
; tricky to use.  But UPARSE does have some tools for it.  Here's a sample of
; how you might mix FURTHER, SOME, and OPT.

(
    prefix: null
    suffix: ")"

    ")" = parse "aaa)))" [
        opt some further inline (opt prefix)
        some "a"
        opt some further inline (opt suffix)
    ]
)

; Voids synthesized from plain GROUP! also do not vanish

(3 = parse [x] ['x (1 + 2) | 'y (10 + 20)])
(void? parse [x] ['x (void) | 'y (10 + 20)])
