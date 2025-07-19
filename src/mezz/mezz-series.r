REBOL [
    System: "REBOL [R3] Language Interpreter and Run-time Environment"
    Title: "REBOL 3 Mezzanine: Series Helpers"
    Rights: {
        Copyright 2012 REBOL Technologies
        REBOL is a trademark of REBOL Technologies
    }
    License: {
        Licensed under the Apache License, Version 2.0
        See: http://www.apache.org/licenses/LICENSE-2.0
    }
]


; !!! Although this follows the -OF naming convention, it doesn't fit the
; pattern of a reflector as it takes two arguments.  Moreover, it is a bit
; sketchy...it doesn't check to see that the two series are the same, and
; if all it's doing is plain subtraction it seems like a poor primitive to
; be stuck with giving a name and suggested greater semantics to.  Review.
;
offset-of: lambda [
    "Returns the offset between two series positions."
    series1 [any-series?]
    series2 [any-series?]
][
    (index of series2) - (index of series1)
]


last?: single?: lambda [
    "Returns TRUE if the series length is 1."
    series [any-series? port! map! tuple! bitset! object! any-word?]
][
    1 = length of series
]


extend: func [
    "Extend an object, map, or block type with word and value pair."
    return: [any-value?]
    obj [object! map!] {object to extend (modified)}
    word [any-word?]
    val [any-value?]
][
    append obj word
    return poke obj word :val
]


array: func [
    {Makes and initializes a block of a given size}

    return: "Generated block or null if blank input"
        [block!]
    size "Size or block of sizes for each dimension"
        [<maybe> integer! block!]
    /initial "Initial value (will be called each time if action)"
        [element? action?]
    <local> rest block
][
    initial: default ['~]  ; if not specified, array will be all meta trash
    if block? size [
        rest: next size else [
            ;
            ; Might be reasonable to say `array/initial [] <x>` is `<x>` ?
            ;
            fail "Empty ARRAY dimensions (file issue if you want a meaning)"
        ]
        if not integer? size: size.1 [
            fail 'size ["Expect INTEGER! size in BLOCK!, not" kind of size]
        ]
        if tail? rest [rest: null]  ; want `array [2]` => `[~ ~]`, no recurse
    ]
    else [rest: null]

    block: make block! size
    case [
        block? rest [
            repeat size [append block (array/initial rest :initial)]
        ]
        action? :initial [
            repeat size [append block initial]  ; Called every time
        ]
        any-series? initial [
            repeat size [append block (copy/deep initial)]
        ]
    ] else [
        append/dup block initial size
    ]
    return block
]


replace: func [
    {Replaces a search value with the replace value within the target series}

    return: [any-series?]
    target "Series to replace within (modified)"
        [any-series?]
    pattern "Value to be replaced (converted if necessary)"
        [~void~ element? splice? action?]
    replacement "Value to replace with (called each time if action)"
        [~void~ element? splice? action?]

    ; !!! Note these refinements alias ALL, CASE natives!
    /all "Replace all occurrences"
    /case "Case-sensitive replacement"

    <local> value' pos tail  ; !!! Aliases TAIL native (should use TAIL OF)
][
    if void? :pattern [return target]

    let all_REPLACE: all
    all: :lib.all
    let case_REPLACE: case
    case: :lib.case

    pos: target

    while [[pos /tail]: apply :find [
        pos
        :pattern
        /case case_REPLACE
    ]][
        if action? :replacement [
            ;
            ; If arity-0 action, pos and tail discarded
            ; If arity-1 action, pos will be argument to replacement
            ; If arity-2 action, pos and tail will be passed
            ;
            ; They are passed as const so that the replacing function answers
            ; merely by providing the replacement.
            ;
            value': ^ apply/relax :replacement [const pos, const tail]
        ] else [
            value': ^ replacement  ; inert value, might be null
        ]

        pos: change/part pos (unmeta value') tail

        if not all_REPLACE [break]
    ]

    return target
]


;
; reword "$1 is $2." [1 "This" 2 "that"] => "This is that."
;
reword: func [
    {Make a string or binary based on a template and substitution values}

    return: [any-string? binary!]
    source "Template series with escape sequences"
        [any-string? binary!]
    values "Keyword literals and value expressions"
        [map! object! block!]
    /case "Characters are case-sensitive"
    /escape "Escape char(s) or [prefix suffix] delimiters (default is $)"
        [char? any-string? word! binary! block!]

    <static>

    ; Note: this list should be the same as above with delimiters, with
    ; BLOCK! excluded.
    ;
    delimiter-types (
        [char?! | &any-string? | word! | binary!]
    )
    keyword-types (
        [char?! | &any-string? | integer! | word! | binary!]
    )
][
    let case_REWORD: case
    case: runs :lib.case

    let out: make (kind of source) length of source

    let prefix: void
    let suffix: void
    case [
        null? escape [prefix: "$"]  ; refinement not used, so use default

        any [
            escape = ""
            escape = []
        ][
        ]

        block? escape [
            parse3 escape [
                prefix: delimiter-types
                [<end> | suffix: delimiter-types]
            ] except [
                fail ["Invalid /ESCAPE delimiter block" escape]
            ]
        ]
    ] else [
        prefix: escape
    ]

    ; To be used in a parse rule, words must be turned into strings, though
    ; it would be nice if they didn't have to be, e.g.
    ;
    ;     parse "abc" ['abc] => true
    ;
    ; Integers have to be converted also.
    ;
    if match [integer! word!] prefix [prefix: to-text prefix]
    if match [integer! word!] suffix [suffix: to-text suffix]

    ; MAKE MAP! will create a map with no duplicates from the input if it
    ; is a BLOCK! (though differing cases of the same key will be preserved).
    ; This might be better with stricter checking, in case later keys
    ; overwrite earlier ones and obscure the invalidity of the earlier keys
    ; (or perhaps MAKE MAP! itself should disallow duplicates)
    ;
    if block? values [
        values: make map! values
    ]

    ; We match strings generated from the keywords, but need to know what
    ; generated the strings to look them up in the map.  Hence we build a rule
    ; that will look something like:
    ;
    ; [
    ;     "keyword1" suffix (keyword-match: 'keyword1)
    ;     | "keyword2" suffix (keyword-match: 'keyword2)
    ;     | fail
    ; ]
    ;
    ; Note that the enclosing rule has to account for `prefix`, but `suffix`
    ; has to be part of this rule.  If it weren't, imagine if prefix is "$<"
    ; and suffix is ">" and you try to match "$<10>":
    ;
    ;    prefix [
    ;         "1" (keyword-match: '1)  ; ...this will take priority and match
    ;         | "10" (keyword-match: '10)
    ;    ] suffix  ; ...but then it's at "0>" and not ">", so it fails
    ;
    let keyword-match: null  ; variable that gets set by rule
    let any-keyword-suffix-rule: inside [] collect [
        for-each [keyword value] values [
            if not ok? parse :[keyword] keyword-types [
                fail ["Invalid keyword type:" keyword]
            ]

            keep spread compose/deep <*> [
                (<*> if match [integer! word!] keyword [
                    to-text keyword  ; `parse "a1" ['a '1]` illegal for now
                ] else [
                    keyword
                ])

                (<*> suffix)  ; vaporizes if void no-op rule

                (keyword-match: '(<*> keyword))
            ]

            keep/line '|
        ]
        keep [false]  ; add failure if no match, instead of removing last |
    ]

    let rule: [
        let a: <here>  ; Begin marking text to copy verbatim to output
        opt some [
            to prefix  ; seek to prefix (may be void, this could be a no-op)
            let b: <here>  ; End marking text to copy verbatim to output
            prefix  ; consume prefix (if void, may not be at start of match)
            [
                [
                    any-keyword-suffix-rule (
                        append/part out a offset? a b  ; output before prefix

                        let v: apply :select [
                            values keyword-match
                            /case case_REWORD
                        ]
                        append out switch/type v [
                            frame! [
                                apply/relax v [:keyword-match]  ; arity-0 okay
                            ]
                            block! [eval v]
                        ] else [
                            v
                        ]
                    )
                    a: <here>  ; Restart mark of text to copy verbatim to output
                ]
                    |
                one  ; if wasn't at match, keep the ANY rule scanning ahead
            ]
        ]
        to <end>  ; Seek to end, just so rule succeeds
        (append out a)  ; finalize output - transfer any remainder verbatim
    ]

    apply :parse3 [source rule /case case_REWORD]  ; should succeed
    return out
]


move: func [
    {Move a value or span of values in a series}

    return: [~]  ; !!! Define return value?
    source "Source series (modified)"
        [any-series?]
    offset "Offset to move by, or index to move to"
        [integer!]
    /part "Move part of a series by length"
        [integer!]
    /skip "Treat the series as records of fixed size"
        [integer!]
    /to "Move to an index relative to the head of the series"
][
    part: default [1]
    if skip [
        if 1 > skip [cause-error 'script 'out-of-range skip]
        offset: either to [offset - 1 * skip + 1] [offset * skip]
        part: part * skip
    ]
    part: take/part source part
    insert either to [at head of source offset] [
        lib.skip source offset
    ] either any-array? source [spread part] [part]
]


extract: func [
    {Extracts a value from a series at regular intervals}

    series [any-series?]
    width "Size of each entry (the skip), negative for backwards step"
        [integer!]
    /index "Extract from offset position"
        [integer!]
][
    if zero? width [return make (kind of series) 0]  ; avoid an infinite loop

    let len: to integer! either positive? width [  ; Length to preallocate
        divide (length of series) width  ; Forward loop, use length
    ][
        divide (index of series) negate width  ; Backward loop, use position
    ]

    index: default [1]
    let out: make (kind of series) len
    iterate-skip series width [
        append out maybe (pick series index)
    ]
    return out
]


alter: func [
    {Append value if not found, else remove it; returns true if added}

    return: [logic?]
    series [any-series? port! bitset!] {(modified)}
    value
    /case "Case-sensitive comparison"
][
    case_ALTER: case
    case: runs :lib.case

    if bitset? series [
        if find series value [
            remove/part series value
            return false
        ]
        append series value
        return true
    ]
    if remove apply :find [series value, /case case_ALTER] [
        append series value
        return true
    ]
    return false
]


collect*: func [
    {Evaluate body, and return block of values collected via keep function}

    return: "Result block, or null if no KEEPs (prevent nulls with KEEP [])"
        [~null~ block!]
    body "Block to evaluate"
        [<maybe> block!]
][
    let out: null
    let keeper: specialize (  ; SPECIALIZE to hide series argument
        enclose :append lambda [  ; Derive from APPEND for /LINE /DUP
            f [frame!]
            <with> out
        ][
            decay either void? f.value [  ; DECAY, we want pure null
                null  ; void in, null out (should it pass through the void?)
            ][
                f.series: out: default [make block! 16]  ; no null return now
                f.value  ; ELIDE leaves as result
                elide eval f  ; would invalidate f.value (hence ELIDE)
            ]
        ]
    )[
        series: <replaced>
    ]

    ; use LAMBDA for binding work of connecting KEEP with the keeper function
    ; (Doesn't have or enforce RETURN)
    ;
    run lambda [keep] body :keeper

    return out  ; might be null if no KEEPs that kept anything yet
]


; Classic version of COLLECT which returns an empty block if nothing is
; collected, as opposed to the NULL that COLLECT* returns.
;
collect: redescribe [
    {Evaluate body, and return block of values collected via KEEP function.
    Returns empty block if nothing KEEPed.}
] chain [
    :collect*,
    specialize :else [branch: [copy []]]
]

format: func [
    "Format a string according to the format dialect."
    rules {A block in the format dialect. E.g. [10 -10 #"-" 4]}
    values
    /pad [char? integer!] {char or char code, but 0 -> #"0"}
][
    pad: default [space]
    case [
        pad = 0 [pad: #"0"]
        integer? pad [pad: to-char pad]
    ]

    rules: blockify :rules
    values: blockify :values

    ; Compute size of output (for better mem usage):
    let val: 0
    for-each rule rules [
        if word? rule [rule: get rule]

        val: me + switch/type rule [
            integer! [abs rule]
            text! [length of rule]
            char?! [1]
        ] else [0]
    ]

    let out: make text! val
    insert/dup out pad val

    ; Process each rule:
    for-each rule rules [
        if word? rule [rule: get rule]

        switch/type rule [
            integer! [
                pad: rule
                val: form first values
                values: my next
                if (abs rule) < length of val [
                    clear at val 1 + abs rule
                ]
                if negative? rule [
                    pad: rule + length of val
                    if negative? pad [out: skip out negate pad]
                    pad: length of val
                ]
                change out :val
                out: skip out pad ; spacing (remainder)
            ]
            text! [out: change out rule]
            char?! [out: change out rule]
        ]
    ]

    ; Provided enough rules? If not, append rest:
    if not tail? values [append out spread values]
    return head of out
]


printf: func [
    "Formatted print."
    return: [~]
    fmt "Format"
    val "Value or block of values"
][
    print format :fmt :val
]


split: func [
    {Split series in pieces: fixed/variable size, fixed number, or delimited}

    return: [~null~ block!]
    series "The series to split"
        [<maybe> any-series?]
    dlm "Split size, delimiter(s) (if all integer block), or block rule(s)"
        [
            ~void~  ; just return input
            block!  ; parse rule
            the-block!  ; list of integers for piece lengths
            integer!  ; length of pieces (or number of pieces if /INTO)
            bitset!  ; set of characters to split by
            char? text!  ; text to split by
            quoted!  ; literally look for value
            splice?  ; split on a splice's literal contents
            quasiform!  ; alternate way to pass in splice or void
        ]
    /into "If dlm is integer, split in n pieces (vs. pieces of length n)"
][
    if (void? dlm) or ('~void~ = dlm) [
        return reduce [series]
    ]

    if quasi? dlm [
        if not splice? unmeta dlm [
            fail "SPLIT only allows QUASIFORM! of SPLICE?! or VOID"
        ]
        dlm: unmeta dlm
    ]

    if splice? dlm [
        fail "SPLIT on SPLICE?! would need UPARSE, currently based on PARSE3"
    ]

    if the-block? dlm [
        return map-each len dlm [
            if not integer? len [
                fail ["THE-BLOCK! in SPLIT must be all integers:" mold len]
            ]
            if len <= 0 [
                series: skip series negate len
                continue  ; don't add to output
            ]
            copy/part series series: skip series len
        ]
    ]

    let size  ; set for INTEGER! case
    let result: collect [parse3 series case [
        integer? dlm [
            size: dlm  ; alias for readability in integer case
            if size < 1 [fail "Bad SPLIT size given:" size]

            if into [
                let count: size - 1
                let piece-size: to integer! round/down (length of series) / size
                if zero? piece-size [piece-size: 1]

                [
                    repeat (count) [
                        series: across opt [repeat (piece-size) one] (
                            keep series
                        )
                    ]
                    series: across to <end> (keep series)
                ]
            ] else [
                [opt some [
                    series: across [one, repeat (size - 1) opt one] (
                        keep series
                    )
                ]]
            ]
        ]
        block? dlm [
            let mk1
            let mk2
            [
                opt some [not <end> [
                    mk1: <here>
                    opt some [mk2: <here>, [dlm | <end>] break | one]
                    (keep copy/part mk1 mk2)
                ]]
                <end>
            ]
        ]
        match [bitset! text! char?] dlm [  ; PARSE behavior (merge w/above?)
            let mk1
            [
                some [not <end> [
                    mk1: across [to dlm | to <end>]
                    (keep mk1)
                    opt thru dlm
                ]]
            ]
        ]
    ] else [
        assert [quoted? dlm]
        let mk1
        compose/deep <*> [
            some [not <end> [
                mk1: across [to (<*> dlm) | to <end>]
                (keep mk1)
                opt thru (<*> dlm)
            ]]
        ]
    ]]

    ; Special processing, to handle cases where the spec'd more items in
    ; /into than the series contains (so we want to append empty items),
    ; or where the dlm was a char/string/charset and it was the last char
    ; (so we want to append an empty field that the above rule misses).
    ;
    let fill-val: does [copy either any-array? series [[]] [""]]
    let add-fill-val: does [append result fill-val]
    if integer? dlm [
        if into [
            ; If the result is too short, i.e., less items than 'size, add
            ; empty items to fill it to 'size.  Loop here instead of using
            ; INSERT/DUP, because that wouldn't copy the value inserted.
            ;
            if size > length of result [
                repeat (size - length of result) [add-fill-val]
            ]
        ]
    ] else [
        ; If the last thing in the series is a delimiter, there is an
        ; implied empty field after it, which we add here.
        ;
        switch/type dlm [
            bitset! [did select dlm maybe last series]
            char?! [dlm = last series]
            text! [
                (find series dlm) and (empty? [_ @]: find-last series dlm)
            ]
            quoted! [
                (find series unquote dlm) and (
                    empty? [_ @]: find-last series unquote dlm
                )
            ]
            block! [false]
        ] then fill -> [
            if fill [add-fill-val]
        ]
    ]

    return result
]


find-all: func [
    "Find all occurrences of a value within a series (allows modification)."

    return: [~]
    'series [word!]
        "Variable for block, string, or other series"
    value
    body [block!]
        "Evaluated for each occurrence"
][
    verify [any-series? orig: get series]
    while [any [
        , set series find get series :value
        , (set series orig, false)  ; reset series and break loop
    ]][
        eval body
        series: next series
    ]
]
