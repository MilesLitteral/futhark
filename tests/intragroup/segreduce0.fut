-- Simple intra-group reduction.
-- ==
-- random input { [1][1][256]i32 } auto output
-- random input { [10][10][256]i32 } auto output
-- structure distributed { SegMap/SegRed 1 }

let main =
  map (\xs -> #[incremental_flattening(only_intra)] map i32.sum xs)
