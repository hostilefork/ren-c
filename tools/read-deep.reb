REBOL [
    Title: "Read-deep"
    Rights: {
        Copyright 2018 Brett Handley
    }
    Type: module
    Name: Read-Deep
    License: {
        Licensed under the Apache License, Version 2.0
        See: http://www.apache.org/licenses/LICENSE-2.0
    }
    Author: "Brett Handley"
    Purpose: "Recursive READ strategies."
]

if trap [:import/into] [  ; See %import-shim.r
    do load append copy system/script/path %import-shim.r
]

import <common.r>
import <bootstrap-shim.r>

; read-deep-seq aims to be as simple as possible. I.e. relative paths
; can be derived after the fact.  It uses a state to next state approach
; which means client code can use it iteratively which is useful to avoid
; reading the full tree up front, or for sort/merge type routines.
; The root (seed) path is included as the first result.
; Output can be made relative by stripping the root (seed) path from
; each returned file.
;

read-deep-seq: func [
    {Iterative read deep.}
    queue [block!]
][
    let item: take queue

    if equal? #"/" last item [
        insert queue spread map-each x read %% (repo-dir)/(item) [join item x]
    ]

    return item
]

; read-deep provide convenience over read-deep-seq.
;

export read-deep: func [
    {Return files and folders using recursive read strategy.}

    root [file! url! block!]
    /full "Include root path, retains full paths vs. returning relative paths"
    /strategy "TAKEs next item from queue, building the queue as necessary"
        [action!]
][
    let taker: runs any [strategy, :read-deep-seq]

    let result: copy []

    let queue: blockify root

    while [not tail? queue] [
        append result maybe taker queue  ; possibly null
    ]

    if not full [
        remove result ; No need for root in result.
        let len: length of root
        cfor i 1 length of result 1 [
            ; Strip off root path from locked paths.
            poke result i copy skip result/:i len
        ]
    ]

    return result
]
