-- Size hidden by match.
-- ==
-- input { 2i64 } output { 2i64 }

let main (n: i64) =
  let arr = match n
            case m -> iota m
  in length arr
