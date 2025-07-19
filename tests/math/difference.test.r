; functions/math/difference.r
(24:00 = difference 1-Jan-2007/0:00 31-Dec-2006/0:00)
(0:00 = difference 1-Jan-2007/0:00 1-Jan-2007/0:00)
; block
([1 2] = difference [1 3] [2 3])
([] = difference [1 2] [1 2])
; bitset
((charset "a") = difference charset "a" charset "")
[#1822 ; DIFFERENCE on date!s problem
    (12:00 = difference 13-1-2011/12:00 13-1-2011/0:00)
]

([1 3] = difference [1 2] [2 3])
([[1 2] [3 4]] = difference [[1 2] [2 3]] [[2 3] [3 4]])
([path/1 path/3] = difference [path/1 path/2] [path/2 path/3])

[#1822
    ~invalid-compare~ !! (12:00 = difference 13-1-2011/12:00 13-1-2011)
    (12:00 = difference 13-1-2011/12:00 13-1-2011/0:0)
]
