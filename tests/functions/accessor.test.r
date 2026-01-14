; %accessor.test.r
;
; Functions for getting and setting.

[(
    x: 1000
    y: getter [x + 20]
    all [
        y = 1020
        ^y = 1020
    ]
)(
    store: ~
    x: accessor func [^value [<end> any-value?]] [
        if ghost? ^value [
            return ^store
        ]
        ^store: ^value
        return ~
    ]
    all [
        x: 100
        store = 100
        x = 100
        ^x = 100
        ^x: pack [10 20]
        (lift ^store) = '~['10 '20]~
        (lift ^x) = '~[10 '20]~
    ]
)]
