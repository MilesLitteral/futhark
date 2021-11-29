-- | Code generation for ImpCode with OpenCL kernels.
module Futhark.CodeGen.ImpGen.Metal
  ( compileProg,
    Warnings,
  )
where

import Data.Bifunctor (second)
import qualified Futhark.CodeGen.ImpCode.Metal as Metal 
import Futhark.CodeGen.ImpGen.GPU
import Futhark.CodeGen.ImpGen.GPU.ToMetal 
import Futhark.IR.GPUMem
import Futhark.MonadFreshNames

-- | Compile the program to ImpCode with Metal kernels.
compileProg :: MonadFreshNames m => Prog GPUMem -> m (Warnings, Metal.Program)
compileProg prog = second kernelsToMetal <$> compileProgMetal prog
