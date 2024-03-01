; datatypes/block.r
(block? [1])
(not block? 1)
(block! = kind of [1])

; minimum
(block? [])

; alternative literal representation
([] == make block! 0)
([] == make block! "")
("[]" == mold [])

(
    data: [a 10 b 20]
    all [
        10 = data.a
        10 = select data 'a
        20 = data.b
        20 = select data 'b
        null = get $data.c
        null? select data 'c
    ]
)

; #848
(
    [1] = copy/part tail [1] -2147483648
)
~out-of-range~ !! (
    copy/part tail [1] -2147483649
)
~out-of-range~ !! (
    copy/part tail of [1] -9223372036854775808
)
~out-of-range~ !! (
    copy/part [] 9223372036854775807
)

; Making a block from an action will iterate the action until it gives null

(
    make-one-thru-five: func [
        return: [~null~ integer!]
        <static> count (0)
    ][
        if count = 5 [count: 0 return null]
        return count: count + 1
    ]
    all [
        [1 2 3 4 5] = make block! :make-one-thru-five
        [1 2 3 4 5] = make block! :make-one-thru-five
    ]
)


; ARRAY is poorly named (noun-ish), generates blocks
; http://www.rebol.com/docs/words/warray.html

([~ ~ ~ ~ ~] = array 5)
([0 0 0 0 0] = array/initial 5 0)
([[~ ~ ~] [~ ~ ~]] = array [2 3])
([[0 0 0] [0 0 0]] = array/initial [2 3] 0)
(
    counter: func [<static> n (0)] [return n: n + 1]
    [1 2 3 4 5] = array/initial 5 :counter
)

; !!! Right now, picking out of range (positive or negative) is null, while
; picking by word is also null.  This is inconsistent with objects.  Review.
[
    (did block: [ae #{BD}])

    (block.-304 = null)
    (block.1020 = null)
    (block.ae = #{BD})
    (block.AE = #{BD})
    (block.xy = null)
]
