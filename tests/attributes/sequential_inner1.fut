-- ==
-- random input { [10][10][10]i32 } auto output
-- structure distributed { /SegMap 1 /SegMap/DoLoop 1 }

let main = map (\xss -> #[sequential_inner] map i32.sum xss)
