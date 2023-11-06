REBOL [
    System: "Ren-C Core Extraction of the Rebol System"
    Title: "Common Parsers for Tools"
    Type: module
    Name: Common-Parsers
    Rights: {
        Rebol is Copyright 1997-2015 REBOL Technologies
        REBOL is a trademark of REBOL Technologies

        Ren-C is Copyright 2015-2018 MetaEducation
    }
    License: {
        Licensed under the Apache License, Version 2.0
        See: http://www.apache.org/licenses/LICENSE-2.0
    }
    Author: "@codebybrett"
    Version: 2.100.0
    Needs: 2.100.100
    Purpose: {
        These are some common routines used by the utilities
        that build and test the system.
    }
]

import <bootstrap-shim.r>

c-lexical: import <c-lexicals.r>
import <text-lines.reb>
import <parsing-tools.reb>

decode-key-value-text: function [
    {Decode key value formatted text.}
    return: [block!]
    text [text!]
][
    data-fields: [
        opt some [
            position:  ; <here>
            data-field
            | newline
        ]
    ]

    data-field: [
        data-field-name eof:  ; <here>
        [
            #" " to newline opt some [
                newline not data-field-name not newline to newline
            ]
            | opt some [newline opt newline 2 20 #" " to newline]
        ] eol: (emit-stuff) newline
    ]

    data-field-char: charset [#"A" - #"Z" #"a" - #"z"]
    data-field-name: [
        some data-field-char
        opt some [#" " some data-field-char] #":"
    ]

    emit-stuff: func [
        return: <none>
        <local> key
    ][
        key: replace/all copy/part position eof #" " #"-"
        remove back tail-of key
        append meta spread reduce [
            to word! key
            trim/auto copy/part eof eol
        ]
    ]

    stuff: copy []

    parse2 text data-fields else [
        fail [
            {Expected key value format on line} (text-line-of position)
            {and lines must end with newline.}
        ]
    ]

    return new-line/all/skip stuff true 2
]

load-until-blank: function [
    {Load rebol values from text until double newline.}
    text [text!]
    /next {Return values and next position.}
] [
    wsp: compose [some (charset { ^-})]

    res: null  ; !!! collect as SET-WORD!s for locals, evolving...
    rebol-value: parsing-at x [
        ;
        ; !!! SET-BLOCK! not bootstrap
        ;
        attempt [transcode/next x 'res] else [res: null]
        res
    ]

    terminator: [opt wsp newline opt wsp newline]

    parse2 text [
        some [not terminator rebol-value]
        opt wsp opt [newline opt newline] position:  ; <here>
        to end
    ] then [
        values: load copy/part text position
        return reduce [values position]
    ]

    return null
]


collapse-whitespace: [some [change some white-space (space) | skip] end]
bind collapse-whitespace c-lexical/grammar


export proto-parser: context [

    unsorted-buffer: ~
    file: ~

    emit-fileheader: null
    emit-proto: null
    emit-directive: null

    parse-position: ~
    notes: ~
    lines: ~
    proto-id: ~
    proto-arg-1: ~
    data: ~
    eoh: ~ ; End of file header.

    count: ~

    process: func [return: <none> text] [
        parse2 text grammar/rule
    ]

    grammar: context bind [

        rule: [
            parse-position:  ; <here>
            opt fileheader
            opt some [
                parse-position:  ; <here>
                segment
            ]
        ]

        fileheader: [
            (data: null)
            doubleslashed-lines
            and is-fileheader
            eoh:  ; <here>
            (
                emit-fileheader data
            )
        ]

        segment: [
            (proto-id: proto-arg-1: null)
            format-func-section
            | span-comment
            | line-comment opt some [newline line-comment] newline
            | opt wsp directive
            | other-segment
        ]

        directive: [
            copy data [
                ["#ifndef" | "#ifdef" | "#if" | "#else" | "#elif" | "#endif"]
                opt some [not newline c-pp-token]
            ] eol
            (
                emit-directive data
            )
        ]

        ; We COPY/DEEP here because this part gets invasively modified by
        ; the source analysis tools.
        ;
        other-segment: copy/deep [thru newline]

        ; we COPY/DEEP here because this part gets invasively modified by
        ; the source analysis tools.
        ;
        format-func-section: copy/deep [
            doubleslashed-lines
            ahead is-intro
            function-proto opt some white-space
            function-body
            (
                ; EMIT-PROTO doesn't want to see extra whitespace (such as
                ; when individual parameters are on their own lines).
                ;
                parse2 proto collapse-whitespace
                proto: trim proto
                assert [find proto "("]

                if find proto "()" [
                    print [
                        proto
                        newline
                        {C-Style no args should be foo(void) and not foo()}
                        newline
                        http://stackoverflow.com/q/693788/c-void-arguments
                    ]
                    fail "C++ no-arg prototype used instead of C style"
                ]

                ; Call the EMIT-PROTO hook that the client provided.  They
                ; receive the stripped prototype as a formal parameter, but
                ; can also examine state variables of the parser to extract
                ; other properties--such as the processed intro block.
                ;
                emit-proto proto
            )
        ]

        function-body: #"{"

        doubleslashed-lines: [copy lines some ["//" thru newline]]

        is-fileheader: parsing-at position [
            all [  ; note: not LOGIC!, a series
                lines: attempt [decode-lines lines {//} { }]
                did parse2 lines [copy data to {=///} to end]
                data: attempt [load-until-blank trim/auto data]
                data: attempt [
                    if set-word? first data/1 [data/1] else [false]
                ]
                position ; Success.
            ]
        ]

        is-intro: parsing-at position [
            all [  ; note: not LOGIC!, a series
                lines: attempt [decode-lines lines {//} { }]
                data: load-until-blank lines
                if data [ data: attempt [
                    ;
                    ; !!! The recognition of Rebol-styled comment headers
                    ; originally looked for SET-WORD!, but the syntax for
                    ; doing export uses a WORD! (EXPORT) before the SET-WORD!
                    ;
                    ; http://www.rebol.net/r3blogs/0300.html
                    ;
                    ; It's hacky to just throw it in here, but the general
                    ; consensus is that the build process needs to be made
                    ; much simpler.  It really should be going by seeing it
                    ; is a DECLARE_NATIVE() vs. worrying too much about the text
                    ; pattern in the comment being detected.
                    ;
                    if any [
                        set-word? first data/1
                        'export = first data/1
                    ][
                        notes: data/2
                        data/1
                    ] else [
                        false
                    ]
                ] ]
                position ; Success.
            ]
        ]

        ; http://blog.hostilefork.com/kinda-smart-pointers-in-c/
        ;
        ;     TYPEMACRO(*) Some_Function(TYPEMACRO(const*) value, ...)
        ;     { ...
        ;
        typemacro-parentheses: [
            "option(" opt [identifier "(" thru ")"] thru ")"
            | "Sink(" opt [identifier "(" thru ")"] thru ")"
            | "option(Sink(" opt [identifier "(" thru ")"] thru "))"
            | "(*)" | "(const*)"
            | "(const *)" (fail "use (const*) not (const *)")
            | "(Cell(const*))"
            | "(Cell(const*) )" (fail "use (Cell(const*)) not (Cell(const*) )")
        ]

        function-proto: [
            copy proto [
                not white-space
                some [
                    typemacro-parentheses
                    | [
                        not "(" not "="
                        [white-space | copy proto-id identifier | skip]
                    ]
                ]
                "("
                opt some white-space
                opt [
                    not typemacro-parentheses
                    not ")"
                    copy proto-arg-1 identifier
                ]
                opt some [typemacro-parentheses | not ")" [white-space | skip]]
                ")"
            ]
        ]

    ] c-lexical/grammar
]

export rewrite-if-directives: function [
    {Bottom up rewrite conditional directives to remove unnecessary sections.}
    return: <none>
    position
][
    until [
        parse2 position [
            (rewritten: false)
            some [
                [
                    change ["#if" thru newline "#endif" thru newline] ("")
                    | change ["#elif" thru newline "#endif"] ("#endif")
                    | change ["#else" thru newline "#endif"] ("#endif")
                ] (rewritten: true)
                :position  ; seek

              | thru newline
            ]
            end
        ]
        not rewritten
    ]
]
