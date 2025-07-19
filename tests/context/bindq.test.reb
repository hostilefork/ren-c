; functions/context/bindq.r
(
    o: make object! [a: null]
    same? o binding of has o 'a
)

(
    obj: make object! [x: 1020]
    for-each item bind [
        x 'x ''x '''x ''''x
        @x '@x ''@x '''@x ''''@x
        :x ':x '':x ''':x '''':x
        x: 'x: ''x: '''x: ''''x:
        ^x '^x ''^x '''^x ''''^x

        ; !!! Should BINDING OF work on PATH! and TUPLE!?
        ;
        ; /x '/x ''/x '''/x ''''/x
        ; .x '.x ''.x '''.x ''''.x
        ; x/ 'x/ ''x/ '''x/ ''''x/
        ; x. 'x. ''x. '''x. ''''x.
    ] obj [
        if obj <> binding of item [
            fail ["Binding of" ^item "is not to expected object"]
        ]
    ]
    ok
)
