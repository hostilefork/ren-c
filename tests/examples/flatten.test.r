; %flatten.test.r
;
; Currently there's no flatten "in the box".  MAP-EACH with splices is
; deemed to be enough if you want to write it.
;
; It may be revisited, but it likely won't be a core native.

[(
    flatten: lambda [block [block!] :deep] [
        map-each item block [
            if deep and (block? item) [
                item: flatten:deep item
            ]
            if block? item [spread item] else [item]
        ]
    ]
    ok
)
(
    [a b c d e f] = flatten [[a] [b] c d [e f]]
)
(
    [a b [c d] c d e f] = flatten [[a] [b [c d]] c d [e f]]
)
(
    [a b c d c d e f] = flatten:deep [[a] [b [c d]] c d [e f]]
)
]

