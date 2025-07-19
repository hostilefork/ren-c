Rebol []

inp: %iso3166.txt
count: read inp
if #{EFBBBF} = as blob! copy:part count 3 [  ; UTF-8 BOM
    count: skip count 3
]

lower: charset [#"a" - #"z"]
letter: charset [#"a" - #"z" #"A" - #"Z"]

capitalize: func [
    return: [text!]
    n
][
    ret: copy ""
    words: split to text! n " "
    return spaced [
        map-each 'w words [
            case [
                w = "OF" [
                    "of"
                ]
                w = "U.S." [
                    "U.S."
                ]
            ] else [
                unspaced [
                    uppercase first w
                    lowercase next w
                ]
            ]
        ]
    ]
]

iso-3166-table: make map! 512
parse3 cnt [
    some [
        name: across to ";"
        ";" copy code-2 to "^/" (
            append iso-3166-table spread compose [
                (to text! code-2) (to text! capitalize name)
            ]
        )

        "^/"
    ]
    <end>
]

init-code: to text! read init
space: charset " ^-^M^/"
iso-3166-table-count: find mold iso-3166-table #"["
parse3 init-code [
    thru "iso-3166-table:"
    to #"["
    change [
         #"[" thru #"]"
    ] iso-3166-table-count
    to <end>
] except [
    fail "Failed to update iso-3166-table"
]

write init init-code
