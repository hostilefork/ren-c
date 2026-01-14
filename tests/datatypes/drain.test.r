; %drain.test.r
;
; "bedrock" state which discards whatever you assign to it.

(drain? (drain))

[
    (
        dr: drain
        obj: make object! [dr: drain]

        verify-drains: does [assert [
            drain? $dr
            drain? $obj.dr
            ^dr except (e -> [e.id = 'cant-get-drain])
            ^obj.dr except (e -> [e.id = 'cant-get-drain])
        ] ok]

        ~okay~
    )

    (verify-drains)  ; after initial fetch

    ; !!! Should an unstable non-^META pick of an error panic as if that
    ; error had happened?  In this case it's what we want: ~cant-get-drain~
    ; but knowing that a non-^META pick happened is pretty important.
    ;
    ~unstable-non-meta~ !! (ignore dr)  ; test panics
    ~unstable-non-meta~ !! (ignore obj.dr)

    (1020 = dr: 1020)  ; SET-WORD passes thru data (standard behavior)
    (1020 = obj.dr: 1020)

    (verify-drains)  ; should not have overwritten (still drains)

    ~zero-divide~ !! (failure? dr: 1 / 0)  ; normal assign should not work
    ~zero-divide~ !! (failure? obj.dr: 1 / 0)

    (verify-drains)  ; also should not have overwritten

    (failure? ^dr: 1 / 0)  ; meta-assign succeeds, but discards
    (failure? ^obj.dr: 1 / 0)

    (verify-drains)

    (dr: alias $okay)  ; bedrock assignment overrides ATM (review)
    (obj.dr: alias $okay)

    (dr)  ; overwritten with okay now
    (obj.dr)
]
