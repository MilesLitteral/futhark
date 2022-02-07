func @foo(%arg0: i32, %arg1: i64) -> (i32, i64) {
  return %arg0, %arg1 : i32, i64
}
func @bar() {
  %0 = arith.constant 42 : i32
  %1 = arith.constant 17 : i64
  %2:2 = call @foo(%0, %1) : (i32, i64) -> (i32, i64)
  "use_i32"(%2#0) : (i32) -> ()
  "use_i64"(%2#1) : (i64) -> ()
}

// is transformed into

llvm.func @foo(%arg0: i32, %arg1: i64) -> !llvm.struct<(i32, i64)> {
  // insert the vales into a structure
  %0 = llvm.mlir.undef : !llvm.struct<(i32, i64)>
  %1 = llvm.insertvalue %arg0, %0[0] : !llvm.struct<(i32, i64)>
  %2 = llvm.insertvalue %arg1, %1[1] : !llvm.struct<(i32, i64)>

  // return the structure value
  llvm.return %2 : !llvm.struct<(i32, i64)>
}
llvm.func @bar() {
  %0 = llvm.mlir.constant(42 : i32) : i32
  %1 = llvm.mlir.constant(17) : i64

  // call and extract the values from the structure
  %2 = llvm.call @bar(%0, %1)
     : (i32, i32) -> !llvm.struct<(i32, i64)>
  %3 = llvm.extractvalue %2[0] : !llvm.struct<(i32, i64)>
  %4 = llvm.extractvalue %2[1] : !llvm.struct<(i32, i64)>

  // use as before
  "use_i32"(%3) : (i32) -> ()
  "use_i64"(%4) : (i64) -> ()
}
