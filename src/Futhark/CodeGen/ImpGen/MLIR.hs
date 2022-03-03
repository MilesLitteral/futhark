-- | Code generation for ImpCode with Metal kernels.
module Futhark.CodeGen.ImpGen.MLIR
  ( compileProg,
    Warnings,
  )
where

import Data.Bifunctor (second)
import qualified Futhark.CodeGen.ImpCode.MLIR as MLIR
import Futhark.CodeGen.ImpGen.GPU
import Futhark.CodeGen.ImpGen.GPU.ToMLIR
import Futhark.IR.GPUMem
import Futhark.MonadFreshNames

-- | Compile the program to ImpCode with Metal kernels.
compileProg :: MonadFreshNames m => Prog GPUMem -> m (Warnings, MLIR.Program)
compileProg prog = second kernelsToMetal <$> compileProgMetal prog
