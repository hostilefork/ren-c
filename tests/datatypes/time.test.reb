; datatypes/time.r
(time? 0:00)
(not time? 1)
(time! = type of 0:00)
(0:0:10 = make time! 10)
(0:0:10 = to time! 10)
(error? trap [to time! "a"])
("0:00" = mold 0:00)

; small value
(
    t: ~
    any [
        error? trap [t: -596522:0:0 - 1:00]
        t = transcode:one mold t
    ]
)

; big value
(
    t: ~
    any [
        error? trap [t: 596522:0:0 + 1:00]
        t = transcode:one mold t
    ]
)

; strange value
(error? trap [load "--596523:-14:-07.772224"])

; minimal time
(time? -596523:14:07.999999999)

; maximal negative time
(negative? -0:0:0.000000001)

; minimal positive time
(positive? 0:0:0.000000001)

; maximal time
(time? 596523:14:07.999999999)

[#96 (
    time: 1:23:45.6
    1:23:45.7 = (time + 0.1)
)]
[#96 (
    time: 1:23:45.6
    0:41:52.8 = (time * 0.5)
)]


[#1156 (
    0:01:00 / 0:00:07 = 8.571428571428571
)]
[#1156 (
    8 * 0:00:07 = 0:00:56
)]

; Primitive exposed as interim workaround for writing NOW/TIME/PRECISE, since
; MAKE TIME! didn't define a nanosecond format (and may need broader thinking)
;
(0:10:20.0304 = make-time-sn 620 30400000)

; Test datatypes of return values
;
; !!! There's no particularly good reason for these to be refinements of NOW,
; as it's just reproducing the interface of the DATE! datatype.
[
    (integer? now:year)
    (integer? now:month)
    (integer? now:day)
    (time? now:time)
    (time? now:zone)
    (date? now:date)
    (integer? now:weekday)
    (integer? now:yearday)
    (date? now:precise)
    (date? now:utc)
    (date? now:local)
]

; Mutating times should write back to the container, which is where the
; immediate bits for the values live.
;
(block: [x 10:00 y], block.2.hour: 20, block = [x 20:00 y])
(obj: make object! [t: 10:00], obj.t.hour: 20, obj.t = 20:00)
(m: to map! [t 10:00], m.t.hour: 20, m.t = 20:00)
