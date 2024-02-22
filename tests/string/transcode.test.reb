; TRANSCODE is an operation which exposes the scanner, and is the basis of LOAD
; It lets you take in UTF-8 text and gives back a BLOCK! of data
;
; The interface is historically controversial for its complexity:
; https://github.com/rebol/rebol-issues/issues/1916
;
; Ren-C attempts to make it easier.  Plain TRANSCODE with no options will simply
; load a string or binary of UTF-8 in its entirety as the sole return result.
; The multiple-return-awareness kicks it into a more progressive mode, so that
; it returns partial results and can actually give a position of an error.

; Default is to scan a whole block's worth of values
([1 [2] <3>] = transcode "1 [2] <3>")

; When asking for a block's worth of values, an empty string gives empty block
(all [
    result: transcode ""
    [] = result
    not new-line? result
])
(all [
    result: transcode "^/    ^/    ^/"
    [] = result
    new-line? result
])

; If TRANSCODE returns null, there is no POS so you have to make it optional
; (unless you don't expect it to return null)
(
    all [
        1 = [value /pos]: transcode/one "1 [2] <3>"
        value = 1
        pos = " [2] <3>"

        [2] = [value /pos]: transcode/one pos
        value = [2]
        pos = " <3>"

        <3> = [value /pos]: transcode/one pos
        value = <3>
        pos = ""

        null = [value /pos]: transcode/one pos
        value = null
        pos = null
    ]
)

(
    [value /pos]: transcode/one "[^M^/ a] b c" except e -> [
        e.id = 'illegal-cr
    ]
)

(
    str: "Cat😺: [😺 😺] (😺)"

    all [
        'Cat😺: = [value pos]: transcode/one str
        set-word? value
        value = 'Cat😺:
        pos = " [😺 😺] (😺)"

        [[😺 😺] (😺)] = value: transcode pos  ; no position, read to end
        block? value
        value = [[😺 😺] (😺)]  ; no position out always gets block
    ]
)

; Do the same thing, but with UTF-8 binary...
(
    bin: as binary! "Cat😺: [😺 😺] (😺)"
    bin =  #{436174F09F98BA3A205BF09F98BA20F09F98BA5D2028F09F98BA29}

    all [
        'Cat😺: = [value pos]: transcode/one bin
        set-word? value
        value = 'Cat😺:
        pos = #{205BF09F98BA20F09F98BA5D2028F09F98BA29}
        (as text! pos) = " [😺 😺] (😺)"

        [[😺 😺] (😺)] = value: transcode pos  ; no position, read to end
        block? value
        value = [[😺 😺] (😺)]  ; no position out always gets block
    ]
)

[
    ([abc def] = [_ _]: transcode "abc def")
    ('abc = [_ _]: transcode/one "abc def")
    (raised? [_ _]: transcode/one "3o4")
    ('scan-invalid = pick trap [[_ _]: transcode/one "3o4"] 'id)
]

(
    [v p]: transcode/one to binary! "7-Feb-2021/23:00"
    [7-Feb-2021/23:00 #{}] = reduce [v p]
)

[
    ('scan-invalid = pick trap [transcode "2022:"] 'id)
    ('scan-invalid = pick trap [transcode ":2022"] 'id)
    ('scan-invalid = pick trap [transcode "^^2022"] 'id)  ; escaped
    ('scan-invalid = pick trap [transcode "@2022"] 'id)
]
