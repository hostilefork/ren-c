;
; Check structure of loaded tuples and paths by analogy.
;
;   * BLOCK! represents PATH!
;   * GROUP! represents TUPLE!
;
; Since you can also put BLOCK! and GROUP! in paths:
;
;   * META-BLOCK! represents BLOCK!
;   * META-GROUP! represents GROUP!
;
; Each test starts with a string to transcode, then `->`, and then the one or
; more transformed representations of what the string is expected to load as.
;
; Testing in this fashion is important beyond just checking structural
; ambiguities.  Simpler tests like ["/a" -> /a] could just mirror the error,
; e.g. by loading the test incorrectly as ["/a" -> / a] and then validating
; against its own bad load.
;

[(
    tests: [
        "/"  ->  /
        "//"  ->  //
        "///"  ->  ///

        "."  ->  .
        ".."  ->  ..
        "..."  ->  ...

        "/a"  ->  [_ a]
        "//a"  !!  ~scan-invalid~
        "a/"  ->  [a _]
        "a//"  !!  ~scan-invalid~
        "/a/"  ->  [_ a _]
        "//a//"  !!  ~scan-invalid~

        "(a b)/c"  ->  [^(a b) c]
        "(a b) /c"  ->  ^(a b)  [_ c]

        "a.b/c.d"  ->  [(a b) (c d)]
        "a/b.c/d"  ->  [a (b c) d]

        "/a.b/c.d/"  ->  [_ (a b) (c d) _]
        ".a/b.c/d."  ->  [(_ a) (b c) (d _)]

        "./a"  ->  [. a]
        "/.a"  ->  [_ (_ a)]

        "[a].(b)"  ->  (^[a] ^(b))

        "a.. b"  !!  ~scan-invalid~
        "a.. /b"  !!  ~scan-invalid~
        "a../b"  !!  ~scan-invalid~

        "/./(a b)/./"  ->  [_ . ^(a b) . _]

        "a.1.(x)/[a b c]/<d>.2"  ->  [(a 1 ^(x)) ^[a b c] (<d> 2)]

        "~/projects/"  ->  [~ projects _]
        "~a~.~b~/~c~"  ->  [(~a~ ~b~) ~c~]

        === COMMA TESTS ===

        "/a/, b."  -> [_ a _] , (b _)

        === BAD PATH ELEMENT TESTS ===

        ; TUPLE! can go in PATH! but not vice-versa.  Besides that, only
        ; INTEGER!, WORD!, GROUP!, BLOCK!, TEXT!, TAG!, and their quasiforms
        ; are currently allowed in either sequence form.

        "/#a"  !!  ~scan-invalid~
        "blk/#{}"  !!  ~scan-invalid~

        === CHAIN TESTS ===

        ; Various places do less exhaustive testing, they should be moved

        "2022:"  ->  @[2022 _]
        ":2022"  ->  @[_ 2022]

        === CHAIN INSIDE OF PATH ===

        "a/:b"  ->  [a @[_ b]]
        "a/:b/c"  ->  [a @[_ b] c]

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


    transform: func [
        {Turn PATH!/TUPLE!s into BLOCK!/GROUP!s for validation testing}

        value [element?]
        <local> mtype
        <static> mapping (reduce [
            path! block!
            tuple! group!
            chain! the-block!
            block! meta-block!
            group! meta-group!
        ])
    ][
        mtype: select/skip/case mapping (type of get/any $value) 2
        if mtype [
            value: to mtype collect [
                count-up 'index (length of value) [
                    keep (transform pick value index)
                ]
            ]
        ]
        return get/any $value
    ]


    iter: tests
    while [not tail? iter] [
        ;
        ; Skip `=== XXX ===` comment lines
        ;
        if iter.1 = '=== [
            until [iter: my next, new-line? iter]
        ]

        let text: ensure text! iter.1
        iter: my next

        let items
        trap [
            items: transcode text
        ] then error -> [
            if iter.1 <> '!! [
                fail ["Unexpected failure on" mold text "->" error.id]
            ]
            iter: my next
            if iter.1 <> quasi error.id [
                fail ["Error mismatch" mold text "->" error.id "and not" iter.1]
            ]
            iter: my next
            any [
                tail? iter
                new-line? iter
            ] then [
                continue
            ]
            if error.arg1 <> iter.1 [
                fail [
                    "Error argument mismatch on" mold text "->" error.arg1
                        "and not" iter.1
                    ]
            ]
            iter: my next
            continue
        ]

        assert [iter.1 = '->]
        iter: my next

        let compares: copy []

        let !!failure!!: does [
            print [mold text "=>" mold items "vs." mold compares]
            fail ["Transformation mismatch for" text]
        ]

        let start: 'yes
        for-each 'v items [
            append compares iter.1

            all [
                no? start
                any [tail? iter, new-line? iter]
            ] then [
                fail ["Transcode produced unexpected results for:" text]
            ]

            start: 'no

            let t: transform v  ; turns path/tuples to block/group structure

            if t <> iter.1 [
                print ["Expected:" mold iter.1]
                print ["Produced:" mold t]
                !!failure!!
            ]
            iter: my next
        ]

        if not new-line? iter [
            append compares iter.1
            !!failure!!
        ]
    ]

    ok
)]
