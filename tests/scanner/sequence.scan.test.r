;
; %sequence.scan.test.r
;
; Check structure of loaded sequences by analogy.
;
;   * [block] represents PATH!      ; a/b -> [a b]
;   * {fence} represents CHAIN!     ; a:b -> {a b}
;   * (group) represents TUPLE!     ; a.b -> (a b)
;
; But you can also put blocks/fences/groups into sequences, so we use a trick.
; Any actual sequences would be turned into a FENCE!, so we can annotate the
; lists as being sequences.  CHAIN! is chosen for its looks...
;
;   * :[block]: represents BLOCK!   ; a:[b] -> {a :[b]:}
;   * :{fence}: represents FENCE!   ; a:{b} -> {a :{b}:}
;   * :(group): represents GROUP!   ; a:(c) -> {a :(c):}
;
; Each test starts with a string to transcode, then `->`, and then the one or
; more transformed representations of what the string is expected to load as.
;
; If the test is supposed to produce an error, that is denoted as `!!`
; followed by a QUASI-WORD of the error ID.
;
; Testing in this fashion is important beyond just checking structural
; ambiguities.  Simpler tests like ["/a" -> /a] could just mirror the error,
; e.g. by loading the test incorrectly as ["/a" -> / a] and then validating
; against its own bad load.
;

( ; <-- test files must be in GROUP!, makes superfluous indentation... review

tests: [
    "#"  ->  #
    "#:#"  ->  {# #}

    "/"  ->  /
    "//"  ->  //
    "///"  ->  ///

    "."  ->  .
    ".."  ->  ..
    "..."  ->  ...

    ":"  ->  :
    "::"  ->  ::
    ":::"  ->  :::

    "/a"  ->  [_ a]
    "//a"  !!  ~bad-sequence-space~
    "a/"  ->  [a _]
    "a//"  !!  ~bad-sequence-item~
    "/a/"  ->  [_ a _]
    "//a//"  !!  ~bad-sequence-space~

    "(a b)/c"  ->  [:(a b): c]
    "(a b) /c"  ->  :(a b):  [_ c]

    "a.b/c.d"  ->  [(a b) (c d)]
    "a/b.c/d"  ->  [a (b c) d]

    "/a.b/c.d/"  ->  [_ (a b) (c d) _]
    ".a/b.c/d."  ->  [(_ a) (b c) (d _)]

    "./a"  ->  [. a]
    "/.a"  ->  [_ (_ a)]

    "[a].(b)"  ->  (:[a]: :(b):)

    "a.. b"  !!  ~bad-sequence-item~
    "a.. /b"  !!  ~bad-sequence-item~
    "a../b"  !!  ~bad-sequence-item~

    "/./(a b)/./"  ->  [_ . :(a b): . _]

    "a.1.(x)/[a b c]/<d>.2"  ->  [(a 1 :(x):) :[a b c]: (<d> 2)]

    "~/projects/"  ->  [~ projects _]
    "~a~.~b~/~c~"  ->  [(~a~ ~b~) ~c~]

    === INTERNAL SIGIL TESTS ===

    ; There should be many more of these, as well as tests of the outer
    ; sigil application.

    "a/$"  ->  [a $]
    "a/$ $/b"  ->  [a $]  $[_ b]

    "a/$b"  ->  [a $b]
    "$a/$b/"  ->  $[a $b _]

    "$a/b.$c"  ->  $[a (b $c)]

    === INTERNAL QUOTE TESTS ===

    ; Again there should be many more of these.

    "a/'b"  ->  [a 'b]
    "a/'b/"  ->  [a 'b _]

    "a/b.'c"  ->  [a (b 'c)]

    === COMMA TESTS ===

    "/a/, b."  ->  [_ a _] , (b _)

    === RUNE TESTS ===

    "/#a"  ->  [_ #a]

    === BAD PATH ELEMENT TESTS ===

    ; TUPLE! can go in PATH! but not vice-versa.  Besides that, only
    ; INTEGER!, WORD!, GROUP!, BLOCK!, TEXT!, TAG!, and their quasiforms
    ; are currently allowed in either sequence form.

    "blk/#{}"  !!  ~bad-sequence-item~

    === CHAIN TESTS ===

    ; Various places do less exhaustive testing, they should be moved

    "2022:"  ->  {2022 _}
    ":2022"  ->  {_ 2022}

    === CHAIN INSIDE OF PATH ===

    "a/:b"  ->  [a {_ b}]
    "a/:b/c"  ->  [a {_ b} c]

    === TAG AMBIGUITY RESOLUTION ===

    ; When it comes to dots and slashes, TAG!s win; in particular to make
    ; it possible to use them as alternate representations of filenames
    ; e.g. `<../foo/bar.txt>`.  This means WORD!s containing < and > are
    ; not legal in paths or tuples...only tags.

    "<.>"  ->  <.>
    ">.<"  !!  ~scan-invalid~
    ".<."  !!  ~scan-invalid~
    ".>>."  !!  ~scan-invalid~
    "a.b.<c>"  ->  (a b <c>)
    "<a>.b.c"  ->  (<a> b c)
    ".<tag>."  ->  (_ <tag> _)

    "</>"  ->  </>
    ">/<"  !!  ~scan-invalid~
    "/</"  !!  ~scan-invalid~
    "/>>/"  !!  ~scan-invalid~
    "a/b/<c>"  ->  [a b <c>]
    "<a>/b/c"  ->  [<a> b c]
    "/<tag>/"  ->  [_ <tag> _]
]


; Mutators are functions that mutate a copy of the test string, and then
; return a function whose job it is to describe how that mutation should
; affect the result.  This gives us a force multiplier, so we don't have to
; write things out like:
;
;    "#:#"  -> {# #}
;    "#:#,"  -> {# #} ,
;    "#:#:#"  -> {# # #}
;    "#:#:#,"  -> {# # #} ,
;
; Instead we can just describe something that adds a comma character to the
; text, and then that adds a COMMA! value to the transformation.

mutators: reduce [
    ;
    ; identity mutator: we do FOR-EACH on mutators, and so there's one for
    ; "as is" that's the initial test
    ;
    func [text] [
        ; no adjustment

        return lambda [items [block!]] [
            items  ; expect results as-is written in tests
        ]
    ]

    ; comma mutator: any source with a comma character glued on the end should
    ; scan as that thing with a COMMA! after it
    ;
    func [text] [
        append text ","

        return lambda [items [block!]] [
            compose [(spread items),]
        ]
    ]

    ; block mutator: brackets enclosing anything we scan should scan as that
    ; stuff spliced into a block.  But remember that real blocks are :[block]:
    ; in the transformation (since blocks represent paths)
    ;
    func [text] [
        insert text "["
        append text "]"

        return lambda [items [block!]] [
            compose:deep [:[(spread items)]:]
        ]
    ]
]


transform: lambda [
    "Turn sequences into lists for validation testing (e.g. a/b -> [a b])"

    value [any-element?]
][
    let mapping: static [
        make map! [
            path! block!
            chain! fence!
            tuple! group!
        ]
    ]

    case [
        any-sigiled? value [
            decorate (sigil of value) transform plain value
        ]
        any-list? value [
            let deep: as (type of value) map-each 'item value [
                transform item
            ]
            join chain! [_ deep _]   ; so like :[block]:
        ]
        any-sequence? value [
            as mapping.(type of value) map-each 'item value [
                transform item
            ]
        ]
        <else> [value]
    ]
]


iter: tests
until [tail? iter] [
    ;
    ; Skip `=== XXX ===` comment lines
    ;
    if iter.1 = '=== [
        insist [iter: my next, new-line? iter]
    ]

    let original-text: const ensure text! iter.1
    iter: my next

    let mode: ensure ~[!! ->]~ iter.1
    iter: my next

    let original-expected: const collect [
        while [not any [tail? iter, new-line? iter]] [
            keep iter.1
            iter: my next
        ]
    ]

    for-each mut mutators [
        let text: copy original-text

        let /adjust: mut // [text]  ; modifies text, returns adjuster function

        let expected: switch mode [
            '-> [
                adjust original-expected
            ]
            '!! [
                original-expected  ; !!! error mode adjustments?
            ]
        ]

        let scanned: transcode text except (e -> [
            if mode <> '!! [
                panic ["Unexpected failure on" @text "->" @(e.id)]
            ]
            if expected.1 <> quasi e.id [
                panic [
                    "Error mismatch" @text "->" @e.id "and not" @(expected.1)
                ]
            ]
            all [
                :expected.2
                :expected.2 <> e.arg1
            ] then [
                panic [
                    "Error argument mismatch on" @text "->" @(e.arg1)
                        "and not" @(iter.1)
                    ]
            ]
            continue
        ])

        if mode = '!! [
            panic ["Unexpected success on" @text "->" (mold spread items)]
        ]

        let items: map-each value scanned [transform value]

        if items <> expected [
            print ["Transformation mismatch for" text]
            print #
            print ["    Scanned as:" mold scanned]
            print #
            print ["Transformed to:" mold items]
            print ["      Expected:" mold expected]

            panic ["Transformation mismatch for" text]
        ]
    ]
]

ok  ; <-- tests must signal ~okay~ in order to pass

)  ; <-- tests must be in GROUP!, makes superfluous indentation... review
