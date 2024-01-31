REBOL [
    System: "REBOL [R3] Language Interpreter and Run-time Environment"
    Title: "Common Code for Emitting Text Files"
    Type: module
    Name: Common-Emitter
    Rights: {
        Copyright 2016-2018 Ren-C Open Source Contributors
        REBOL is a trademark of REBOL Technologies
    }
    License: {
        Licensed under the Apache License, Version 2.0
        See: http://www.apache.org/licenses/LICENSE-2.0
    }
    Purpose: {
        While emitting text files isn't exactly rocket science, it can help
        to have a few sanity checks on the process.

        The features added here vs. writing lines oneself are:

        * Some awareness of C constructs and the automatic conversion of
          Rebol symbols to valid C identifiers, as well as a not-too-invasive
          method for omitting commas from the ends of enum or initializer
          lists (they're not legal in standard C or C++).

        * Not allowing whitespace at the end of lines, tab characters in the
          output, some abilities to manage indentation.

        * Automatically generating C comment headers or Rebol headers, and
          including "DO NOT EDIT" warnings on temporary files.

        * Being able to use the information of the file and title being
          generated to provide notifications of what work is currently in
          progress to make errors easier to locate.
    }
]

if trap [:import/into] [  ; See %import-shim.r
    do load append copy system/script/path %import-shim.r
]

import <bootstrap-shim.r>

import <common.r>  ; for REPO-DIR
import <platforms.r>  ; for BOOT-VERSION

export cscape: func [
    {Escape Rebol expressions in templated C source, returns new string}

    return: "${} TO-C-NAME, $<> UNSPACED, $[]/$() DELIMIT closed/open"
        [text!]
    template "${Expr} case as-is, ${expr} lowercased, ${EXPR} is uppercased"
        [block!]
    <local> col prefix suffix mode pattern
][
    assert [text? last template]

    let string: trim/auto copy last template

    ; As we process the string, we CHANGE any substitution expressions into
    ; an INTEGER! for doing the replacements later with REWORD (and not
    ; being ambiguous).
    ;
    let num: 1
    let num-text: to text! num  ; CHANGE won't take GROUP! to evaluate, #1279

    let list: collect* [
        parse2 string [(col: 0), start:  ; <here>
        opt some [
            [
                (prefix: null suffix: null)
                finish:  ; <here>

                "${" change [copy expr: [to "}"]] (num-text) skip (
                    mode: #cname
                    pattern: unspaced ["${" num "}"]
                )
                    |
                "$<" change [copy expr: [to ">"]] (num-text) skip (
                    mode: #unspaced
                    pattern: unspaced ["$<" num ">"]
                )
                    |
                (prefix: copy/part start finish)
                "$[" change [copy expr: [to "]"]] (num-text) skip (
                    mode: #delimit
                    pattern: unspaced ["$[" num "]"]
                )
                copy suffix: to newline
                    |
                (prefix: copy/part start finish)
                "$(" change [copy expr: [to ")"]] (num-text) skip (
                    mode: #delimit
                    pattern: unspaced ["$(" num ")"]
                )
                copy suffix: remove to newline
            ] (
                keep compose [
                    (pattern else ['~null~]) (col) (mode) (expr)
                    (prefix else ['~null~]) (suffix else ['~null~])
                ]
                num: num + 1
                num-text: to text! num
            )
                |
            newline
            (col: 0 prefix: null suffix: null)
            start:  ; <here>
                |
            skip (col: col + 1)
        ]]
    ] else [
        return string
    ]

    list: unique/case list

    let substitutions: collect [
        for-each item list [
            ;
            ; SET no longer takes BLOCK!, and bootstrap executable doesn't have
            ; SET-BLOCK! so no UNPACK.
            ;
            pattern: degrade item/1
            col: item/2
            mode: item/3
            expr: item/4
            prefix: degrade item/5
            suffix: degrade item/6

            let any-upper: did find/case expr charset [#"A" - #"Z"]
            let any-lower: did find/case expr charset [#"a" - #"z"]
            keep maybe pattern

            ; With binding being case-sensitive, we lowercase the expression.
            ; Since we do the lowercasing before the load, embedded string
            ; literals will also wind up being lowercase.  It would be more
            ; inconvenient to deep traverse the splice after loading to only
            ; lowercase ANY-WORD!s, so this is considered fine
            ;
            ; !!! Needs LOAD-ALL shim hack for bootstrap since /ALL deprecated
            ;
            let code: transcode lowercase expr

            code: cscape-inside template code

            if null? let sub: do code [  ; shim null, e.g. blank!
                print mold template
                print mold code
                fail "Substitution can't be NULL (shim BLANK!)"
            ]

            sub: degrade switch mode [  ; still want to make sure mode is good
                #cname [
                    ; !!! The #prefixed scope is unchecked for valid global or
                    ; local identifiers.  This is okay for cases that actually
                    ; are prefixed, like `cscape {SYM_${...}}`.  But if there
                    ; is no prefix, then the check might be helpful.  Review.
                    ;
                    (to-c-name/scope :sub #prefixed) else [  ; can vanish
                        '~null~
                    ]
                ]
                #unspaced [
                    case [
                        void? :sub ['~null~]
                        block? sub [unspaced sub]
                    ] else [
                        form sub
                    ]
                ]
                #delimit [
                    if void? :sub [
                        '~null~
                    ] else [
                        delimit (unspaced [maybe :suffix newline]) sub
                    ]
                ]
                fail ["Invalid CSCAPE mode:" mode]
            ]
            assert [not void? :sub]  ; turned to "null" by now (shim's BLANK!)

            ; We want to recognize lines that had substitutions that all
            ; vanished, and remove them (distinctly from lines left empty
            ; on purpose in the template.  We need to put some kind of signal
            ; to get that behavior.
            ;
            sub: default [copy "/* _ */"]  ; replaced in post phase

            case [
                all [any-upper, not any-lower] [
                    uppercase sub
                ]
                all [any-lower, not any-upper] [
                    lowercase sub
                ]
            ]

            ; If the substitution started at a certain column, make any line
            ; breaks continue at the same column.
            ;
            let indent: unspaced collect [
                keep newline
                keep maybe :prefix
            ]
            replace/all sub newline indent

            keep sub
        ]
    ]

    for-each [pattern replacement] substitutions [
        replace string pattern replacement
    ]

    ; BLANK! in CSCAPE tries to be "smart" about omitting the item from its
    ; surrounding context, including removing lines when blank output and
    ; whitespace is all that ends up on them.  If the user doesn't want the
    ; intelligence, they should use "".
    ;
    ; !!! REMOVE was buggy and unpredictable in R3-Alpha PARSE, and the
    ; bootstrap executable inherited that.  Collect a list of lines to kill
    ; and do it in a phase after the parse.  (Probably just don't use PARSE.)
    ;
    let kill-lines: copy []
    let allwhite
    let start-line
    let end-line
    parse2 string [
        (allwhite: true) start-line:  ; <here>
        opt some [
            space
            |
            newline
            [
                ; PARSE arity-1 IF deprecated in Ren-C, but :(...) with logic
                ; not available in the bootstrap build.
                ;
                end-line:  ; <here>
                (if allwhite and (end-line != next start-line) [
                    insert kill-lines start-line  ; back to front for delete
                    insert kill-lines end-line
                ])
            ]
            (allwhite: true) start-line:  ; <here>
            |
            [
                "/* _ */"
                |
                (allwhite: false)  ; has something not a newline or space in it
                skip
            ]
        ]
        end
    ]

    for-each [start end] kill-lines [
        remove/part start end
    ]

    replace/all string "/* _ */" ""

    return string
]


export boot-version: load-value %../src/boot/version.r

export make-emitter: func [
    {Create a buffered output text file emitter}

    return: [object!]
    title "Title for the comment header (header matches file type)"
        [text!]
    file "Filename to be emitted... .r/.reb/.c/.h/.inc files supported"
        [file!]
    /temporary "DO-NOT-EDIT warning (automatic if file begins with 'tmp-')"

    <with>
    system  ; The `System:` SET-WORD! below overrides the global for access
][
    if not let by: system/script/header/file [
        fail [
            "File: should be set in the generating scripts header section"
            "so that generated files have a comment on what made them"
        ]
    ]

    print unspaced [{Generating "} title {" (via } by {)}]

    let stem: split-path file

    temporary: to-logic any [
        temporary
        did parse2 stem ["tmp-" to end]
    ]

    let is-c: did parse2 stem [thru [".c" | ".h" | ".inc"] end]

    let is-js: did parse2 stem [thru ".js" end]

    let e: make object! compose [
        ;
        ; NOTE: %make-headers.r directly manipulates the buffer, because it
        ; wishes to merge #ifdef/#endif cases
        ;
        ; !!! Should the allocation size be configurable?
        ;
        buf-emit: make text! 32000

        file: (file)
        title: (title)

        emit: func [
            {Write data to the emitter using CSCAPE templating (see HELP)}

            return: [~]
            template [text! char?! block!]
            <with> buf-emit
        ][
            case [  ; no switch/type in bootstrap
                text? template [
                    append buf-emit trim/auto copy template
                ]
                char? template [
                    append buf-emit template
                ]
            ] else [
                append buf-emit cscape template
            ]
        ]

        write-emitted: func [
            return: [~]
            /tabbed
            <with> file buf-emit
        ][
            if newline != last buf-emit [
                probe skip (tail-of buf-emit) -100
                fail "WRITE-EMITTED needs NEWLINE as last character in buffer"
            ]

            if let tab-pos: find buf-emit tab [
                probe skip tab-pos -100
                fail "tab character passed to emit"
            ]

            if tabbed [
                replace/all buf-emit spaced-tab tab
            ]

            print [{WRITING =>} file]

            write-if-changed file buf-emit

            ; For clarity/simplicity, emitters are not reused.
            ;
            file: ~
            buf-emit: ~
        ]
    ]

    any [is-c is-js] then [
        e/emit [title boot-version stem by {
            /**********************************************************************
            **
            **  REBOL [R3] Language Interpreter and Run-time Environment
            **  Copyright 2012 REBOL Technologies
            **  Copyright 2012-2018 Ren-C Open Source Contributors
            **  REBOL is a trademark of REBOL Technologies
            **  Licensed under the Apache License, Version 2.0
            **
            ************************************************************************
            **
            **  Title: $<Mold Title>
            **  Build: A$<Boot-Version/3>
            **  File: $<Mold Stem>
            **  Author: $<Mold By>
            **  License: {
            **      Licensed under the Apache License, Version 2.0.
            **      See: http://www.apache.org/licenses/LICENSE-2.0
            **  }
        }]
        if temporary [
            e/emit {
                **  Note: {AUTO-GENERATED FILE - Do not modify.}
            }
        ]
        e/emit {
            **
            ***********************************************************************/
        }
        e/emit newline
    ]
    else [
        e/emit {REBOL }  ; no COMPOSE/DEEP in bootstrap shim, yet
        e/emit mold spread compose [
            System: "REBOL [R3] Language Interpreter and Run-time Environment"
            Title: (title)
            File: (stem)
            Rights: {
                Copyright 2012 REBOL Technologies
                Copyright 2012-2018 Ren-C Open Source Contributors
                REBOL is a trademark of REBOL Technologies
            }
            License: {
                Licensed under the Apache License, Version 2.0.
                See: http://www.apache.org/licenses/LICENSE-2.0
            }
            (if temporary [
                spread [Note: {AUTO-GENERATED FILE - Do not modify.}]
            ])
        ]
        e/emit newline
    ]
    return e
]
