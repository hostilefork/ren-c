; %invisible.test.r


; "Opportunistic Invisibility" means that functions can treat invisibility as
; a return type, decided on after they've already started running.
[
    (elide-if-odd: vanishable func [return: [void! integer!] x] [
        if even? x [return x]
        return ~,~
    ] ok)

    (2 = (<test> elide-if-odd 2))
    (<test> = (<test> elide-if-odd 1))

    (elide-if-even: func [return: [void! integer!] y] [
        return elide-if-odd y + 1
    ] ok)

    (<test> = (<test> elide-if-even 2))
    (2 = (<test> elide-if-even 1))
]


[
    (
        no-annotation: func [x] [return ~,~]
        heavy-void? (<test> no-annotation 10)
    )
    ~bad-return-type~ !! (
        int-spec: func [return: [integer!] x] [return ~,~]
        int-spec 10
    )
    (
        annotated: vanishable func [return: [void! integer!] x] [
            return ~,~
        ]
        <test> = (<test> annotated 10)
    )
]

(
    num-runs: 0

    add-period: func [x [text!]] [
        num-runs: me + 1
        return append x "."
    ]

    all [
        "Hello World." = add-period "Hello World"
        num-runs = 1
        null = add-period veto  ; shouldn't run ADD-PERIOD body
        num-runs = 1
    ]
)

(void? ~,~)
(void? ^void)
(heavy-void? eval [^void])
(any-void? eval $(^void))
(heavy-void? (eval [^void]))
(heavy-void? eval [~,~])
(heavy-void? eval [, ~,~,])
(heavy-void? eval [1 + 2, ^void])
