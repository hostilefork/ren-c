REBOL [
    System: "REBOL [R3] Language Interpreter and Run-time Environment"
    Title: "REBOL 3 Mezzanine: Debug"
    Rights: {
        Copyright 2012 REBOL Technologies
        REBOL is a trademark of REBOL Technologies
    }
    License: {
        Licensed under the Apache License, Version 2.0
        See: http://www.apache.org/licenses/LICENSE-2.0
    }
]

verify: function [
    {Verify all the conditions in the passed-in block are conditionally true}

    return: [nihil?]
    conditions "Conditions to check"
        [block!]
    /handler "Optional code to run if the assertion fails, receives condition"
        [<unrun> block! frame!]
    <local> pos result'
][
    while [
        ; If asserting on a multi-return, we want to assert based only on the
        ; first item.  But some evaluations (like of a COMMA!) return nihil...
        ; an empty parameter pack with no values at all.  The leading slash
        ; means we consider the result to be optional.  If we instead wrote:
        ;
        ;    result': ^ evaluate/next conditions 'pos
        ;
        ; ...then if the eval step produced multiple returns (like for instance
        ; FIND does) we'd have a meta-PACK! and have to write code to decay
        ; it ourselves.
        ;
        ; Using a meta-value here is a way of reacting to isotopes, but we
        ; could also put that responsibility onto the access points.  Review.
        ;
        [^/result']: evaluate/next conditions 'pos
        pos
    ][
        any [
            null? result'  ; evaluated to ~[]~ isotope, no first item to unpack
            '~true~ = result'  ; truthy
            non quasi! result' then [result' <> null']

            if :handler [  ; may or may-not take two arguments
                reaction: ^ if block? :handler [
                    do handler
                ] else [
                    if (comment [1 = arity of :handler], true) [  ; TBD
                        run handler (copy/part conditions pos)
                    ] else [
                        run handler (copy/part conditions pos) unmeta result'
                    ]
                ]

                ; If the handler doesn't itself fail--and does not return
                ; ~ignore~, then we go ahead and fail.  This lets you
                ; write simple handlers that just print you a message...like
                ; some context for the assert.
                ;
                reaction = '~ignore~
            ]
        ] else [
            fail 'conditions make error! [
                type: 'Script
                id: 'assertion-failure
                arg1: compose [
                    (spread copy/part conditions pos) ** (case [
                        null' = result' ['~null~]
                        '~false~ = result' ['~false~]  ; isotopic false
                        isoword? result' [result']  ; non-~true~ isotope
                    ])
                ]
            ]
        ]

        conditions: pos   ; move expression position and continue
    ]
    return nihil
]


; Set up ASSERT as having a user-mode implementation matching VERIFY.
; This helps show people a pattern for implementing their own assert.
;
; !!! If a debug mode were offered, you'd want to be able to put back ASSERT
; in such a way as to cost basically nothing.
;
; !!! Note there is a layering problem, in that if people make a habit of
; hijacking ASSERT, and it's used in lower layer implementations, it could
; recurse.  e.g. if file I/O writing used ASSERT, and you added a logging
; feature via HIJACK that wrote to a file.  Implications of being able to
; override a system-wide assert in this way should be examined, and perhaps
; copies of the function made at layer boundaries.
;
native-assert: runs copy unrun :assert
hijack :assert :verify


delta-time: function [
    {Returns the time it takes to evaluate the block}
    block [block!]
][
    timer: unrun :lib.now/precise  ; Note: NOW comes from an Extension
    results: reduce reduce [  ; resolve word lookups first, run fetched items
        timer
        (unrun :elide) (unrun :do) block
        timer
    ]
    return difference results.2 results.1
]

delta-profile: func [
    {Delta-profile of running a specific block.}
    block [block!]
    <local> start end
][
    start: values of stats/profile
    do block
    end: values of stats/profile
    for-each num start [
        change end end/1 - num
        end: next end
    ]
    start: make system.standard.stats []
    set start head of end
    start
]

speed?: function [
    "Returns approximate speed benchmarks [eval cpu memory file-io]."
    /no-io "Skip the I/O test"
    /times "Show time for each test"
][
    result: copy []
    for-each block [
        [
            repeat 100'000 [
                ; measure more than just loop func
                ; typical load: 1 set, 2 data, 1 op, 4 trivial funcs
                x: 1 * index of back next "x"
                x: 1 * index of back next "x"
                x: 1 * index of back next "x"
                x: 1 * index of back next "x"
            ]
            calc: [100'000 / secs / 100] ; arbitrary calc
        ][
            tmp: make binary! 500'000
            insert/dup tmp "abcdefghij" 50000
            repeat 10 [
                random tmp
                gunzip gzip tmp
            ]
            calc: [(length of tmp) * 10 / secs / 1900]
        ][
            count-up n 40 [
                change/dup tmp to-char n 500'000
            ]
            calc: [(length of tmp) * 40 / secs / 1024 / 1024]
        ][
            if not no-io [
                write file: %tmp-junk.txt "" ; force security request before timer
                tmp: make text! 32000 * 5
                insert/dup tmp "test^/" 32000
                repeat 100 [
                    write file tmp
                    read file
                ]
                delete file
                calc: [(length of tmp) * 100 * 2 / secs / 1024 / 1024]
            ]
        ]
    ][
        secs: now/precise
        calc: 0
        recycle
        do block
        secs: to decimal! difference now/precise secs
        append result to integer! do calc
        if times [append result secs]
    ]
    return result
]

net-log: lambda [txt /C /S][txt]

net-trace: function [
    "Switch between using a no-op or a print operation for net-tracing"

    return: [~]
    val [logic?]
][
    either val [
        hijack :net-log func [txt /C /S][
            print [
                (if c ["C:"]) (if s ["S:"])
                    either block? txt [spaced txt] [txt]
            ]
            txt
        ]
        print "Net-trace is now on"
    ][
        hijack :net-log func [txt /C /S][txt]
    ]
]
