%1 = gpu.all_reduce add %0 {} : (f32) -> (f32)
%2 = gpu.all_reduce %0 {
^bb(%lhs : f32, %rhs : f32):
  %sum = arith.addf %lhs, %rhs : f32
  "gpu.yield"(%sum) : (f32) -> ()
} : (f32) -> (f32)