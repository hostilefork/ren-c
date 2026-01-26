; %literal.test.r

[
    (did detector: lambda [^x [<hole> any-stable? pack! void!]] [
        get:dual meta $x
    ])

    ((the '10) = detector 10)
    ((lift null) = detector null)
    ('~(~null~)~ = detector if ok [null])

    ('~,~ = detector (comment "hi"))
    (parameter? detector)

    (did left-detector: infix detector/)

    ((the '1) = (1 left-detector))
    (parameter? left-detector)
    (parameter? (left-detector))
]

(
    x: 'false
    (unlift lift ^void) then [x: 'true]
    true? x
)
