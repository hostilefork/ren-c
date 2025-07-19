REBOL [
    System: "REBOL [R3] Language Interpreter and Run-time Environment"
    Title: "REBOL 3 Mezzanine: To-Type Helpers"
    Rights: {
        Copyright 2012 REBOL Technologies
        REBOL is a trademark of REBOL Technologies
    }
    License: {
        Licensed under the Apache License, Version 2.0
        See: http://www.apache.org/licenses/LICENSE-2.0
    }
]

; !!! Carl wrote "Are we sure we really want all these?" as a comment here.
; Discussion of eliminating the TO-XXX functions in favor of TO XXX! resolved
; to say that many people prefer them...and that they also may serve a point
; by showing a list of legal conversion types in the help.  They also could
; have refinements giving slightly different abilities than the default
; unrefined TO XXX! behavior would give.

; These must be listed explicitly in order for the words to be collected
; as legal "globals" for the mezzanine context (otherwise SET would fail)

; Note that TO-TEXT is currently its own native (with an /INVISIBLE refinement)
; and thus should not be overwritten here.  This may not be permanent.

to-integer: to-decimal: to-percent: to-money: to-pair:
to-tuple: to-time: to-date: to-binary: to-file: to-email: to-url: to-tag:
to-bitset: to-image: to-vector: to-block: to-group:
to-path: to-map: to-datatype:
to-word: to-issue:
to-function: to-object: to-module: to-error: to-port:
to-event:
    ~

; Auto-build the functions for the above TO-* words.
;
; !!! Should there be TO-SET-WORD, TO-GET-TUPLE, etc.?  They are no longer
; basic types.
;
use [word] [
    for-each 'type system.catalog.datatypes [
        word: make word! head of remove back tail of unspaced ["to-" type]
        word: has lib word else [continue]
        if set? word [continue]  ; don't overwrite existing definition
        set word redescribe compose [
            (spaced ["Converts to" form type "value."])
        ](
            specialize to/ [type: get type]
        )
    ]
]


; !!! Refinements are actually PATH! now, but TO PATH! of a WORD! assumes you
; want a 2-element path with a blank at the head that looks like a refinement
;
to-refinement: to-path/

reify-logic: lambda [logic [logic?]] [reify logic]
