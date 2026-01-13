; system/system.r
[#76
    (date? system.build)
]

; Constants in %base-constants have to be protected from modification, as
; they are depended on internally
[
    ~series-frozen~ !! (append lib.empty-block [a b c])
    ~protected-word~ !! (lib.empty-block: "abc")
]
