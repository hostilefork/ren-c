; functions/series/next.r
(
    blk: [1]
    same? tail of blk next blk
)
(
    blk: tail of [1]
    null? next blk
)
