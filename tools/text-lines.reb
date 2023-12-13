REBOL [
    Title: "Text Lines"
    Version: 1.0.0
    Type: module
    Name: Text-Lines
    Rights: {
        Copyright 2015 Brett Handley
    }
    License: {
        Licensed under the Apache License, Version 2.0
        See: http://www.apache.org/licenses/LICENSE-2.0
    }
    Author: "Brett Handley"
    Purpose: {Functions operating on lines of text.}
]

import <bootstrap-shim.r>

decode-lines: func [
    {Decode text encoded using a line prefix e.g. comments (modifies).}
    text [text!]
    line-prefix [text! block!] {Usually "**" or "//". Matched using parse.}
    indent [text! block!] {Usually "  ". Matched using parse.}
] [
    let pattern: compose [(line-prefix)]
    if not empty? indent [append pattern compose [opt (indent)]]

    let [pos rest]
    let line-rule: [
        pos:  ; <here>
        pattern
        rest:  ; <here>
        (rest: remove/part pos rest)
        :rest  ; seek
        thru newline
    ]
    parse2 text [opt some line-rule] else [
        fail [
            {Expected line} (try text-line-of text pos)
            {to begin with} (mold line-prefix)
            {and end with newline.}
        ]
    ]
    if pos: back tail-of text [remove pos]
    return text
]

encode-lines: func [
    {Encode text using a line prefix (e.g. comments)}

    text [text!]
    line-prefix [text!] {Usually "**" or "//"}
    indent [text!] {Usually "  "}
] [
    ; Note: Preserves newline formatting of the block.

    ; Encode newlines.
    let bol: join line-prefix indent
    let pos
    parse2 text [
        opt some [
            thru newline, pos:  ; <here>
            [
                newline (pos: insert pos line-prefix)
              | (pos: insert pos bol)
            ]
            :pos  ; seek
        ]
    ]

    ; Indent head if original text did not start with a newline.
    pos: insert text line-prefix
    if not equal? newline :pos/1 [insert pos indent]

    ; Clear indent from tail if present.
    if indent = pos: skip tail-of text 0 - length of indent [clear pos]
    append text newline

    return text
]

for-each-line: func [
    {Iterate over text lines}

    return: [~]
    'record "Word set to metadata for each line"
        [word!]
    text "Text with lines"
        [text!]
    body "Block to evaluate each time"
        [block!]
] [
    while [not tail? text] [
        let eol: any [find text newline, tail of text]

        set record compose [
            position (text) length (subtract index of eol index of text)
        ]
        text: next eol

        do body
    ]
]

lines-exceeding: func [  ; !!! Doesn't appear used, except in tests (?)
    {Return the line numbers of lines exceeding line-length.}

    return: "Returns null if no lines (is this better than returning []?)"
        [<opt> block!]
    line-length [integer!]
    text [text!]
] [
    let line-list: null
    let line: null
    let [eol bol]

    count-line-rule: [
        (
            line: 1 + any [line, 0]
            if line-length < subtract index-of eol index of bol [
                append line-list: any [line-list, copy []] line
            ]
        )
    ]

    parse2 text [
        opt some [
            bol:  ; <here>
            to newline
            eol:  ; <here>
            skip
            count-line-rule
        ]
        bol:  ; <here>
        skip, to end, eol:  ; <here>
        count-line-rule
    ]

    return line-list
]

text-line-of: func [
    {Returns line number of position within text}

    return: "Line 0 does not exist, no counting is performed for empty text"
        [<opt> integer!]
    position "Position (newline is considered the last character of a line)"
        [text! binary!]
] [
    let text: head of position
    let idx: index of position
    let line: 0

    let advance-rule: [skip (line: line + 1)]

    parse2 text [
        opt some [
            to newline cursor:  ; <here>

            ; IF deprecated in Ren-C, but :(...) with logic not available
            ; in the bootstrap build.
            ;
            if (lesser? index of cursor idx)

            advance-rule
        ]
        advance-rule
    ]

    if zero? line [return null]
    return line
]

text-location-of: func [
    {Returns line and column of position within text.}
    position [text! binary!]
] [
    ; Here newline is considered last character of a line.
    ; No counting performed for empty text.
    ; Line 0 does not exist.

    let text: head of position
    let idx: index of position
    let line: 0
    let eol

    let advance-rule: [
        eol:  ; <here>
        skip (line: line + 1)
    ]
    let cursor
    parse2 text [
        opt some [
            to newline cursor:  ; <here>

            ; !!! IF is deprecated in PARSE, but this code is expected to work
            ; in bootstrap.
            ;
            if (lesser? index of cursor idx)

            advance-rule
        ]
        advance-rule
    ]

    if zero? line [line: null] else [
        line: reduce [line 1 + subtract index? position index? eol]
    ]

    return line
]

export [
    decode-lines
    encode-lines
    for-each-line
    lines-exceeding
    text-line-of
    text-location-of
]
