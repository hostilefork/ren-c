; functions/control/try.r
;
; Note: This file is testing the error trapping functions.
;
; Annotations like #rescue are used here to act as a reminder not to remove
; the rescues from the test (thinking you could just use the usual error-id
; testing mechanism of test).  e.g. I don't want someone to change:
;
;    #rescue (
;       e: rescue [1 / 0]
;       e.id = 'zero-divide
;    )
;
; Into the abbreviated testing like we'd usually use:
;
;    ~zero-divide~ !! (1 / 0)
;

#rescue (
    e: rescue [1 / 0]
    e.id = 'zero-divide
)
#rescue (
    e: rescue [e: 1 / 0]
    e.id = 'zero-divide
)
#rescue (
    success: 'true
    error? rescue [
        1 / 0
        success: 'false
    ]
    true? success
)
#rescue (
    success: 'true
    f1: does [
        1 / 0
        success: 'false
    ]
    error? sys.util/recover [f1]
    true? success
)
[#822
    #rescue (
        rescue [make error! ""] then [<branch-not-run>] else [okay]
    )
]
#rescue (
    sys.util/recover [panic make error! ""] then [okay]
)
#rescue (
    rescue [1 / 0] then (error?/)
)
#rescue (
    rescue [1 / 0] then (e -> [error? e])
)
#rescue (
    rescue [] then (func [e] [return <handler-not-run>]) else [okay]
)
[#1514
    #rescue (
        error? sys.util/recover [rescue [1 / 0] then (:add)]
    )
]

[#1506
    #rescue (
        10 = reeval unrun func [return: [integer!]] [rescue [return 10] 20]
    )
]

; ENRESCUE (similar to RESCUE but single result, LIFT'ed if not an error)

#enrescue (
    (lift ^void) = enrescue []
)
#enrescue (
    (lift null) = enrescue [null]
)
#enrescue (
    (the '3) = enrescue [1 + 2]
)
#enrescue (
    (the '[b c]) = enrescue [skip [a b c] 1]
)
#enrescue (
    'zero-divide = (enrescue [1 / 0]).id
)
#enrescue (
    f: make frame! lambda [] [fail 'test]
    all {
        error? e: enrescue f
        e.id = 'test
    }
)
#enrescue (
    f: make frame! lambda [] [1000 + 20]
    all {
        quoted? q: enrescue f
        1020 = unquote q
    }
)


; Multiple return values
#rescue (
    null? rescue [10 + 20]
)
#rescue (
    e: rescue [fail 'something]  ; trap before assign attempt
    all [
        error? e
        e.id = 'something
    ]
)
#rescue (
    a: <a>
    b: <b>
    e: rescue [[a b]: fail 'something]  ; trap after assign attempt
    all [
        error? e
        e.id = 'something
        a = <a>
        b = <b>
    ]
)
