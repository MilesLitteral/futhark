{-# LANGUAGE FlexibleContexts #-}

-- | @futhark metal@
module Futhark.CLI.MLIR (main) where

import Futhark.Actions (compileMLIRAction)
import Futhark.Compiler.CLI
import Futhark.Passes (gpuPipeline)

-- | Run @futhark metal@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile MLIR"
  "Generate MLIR Code from optimised Futhark program."
  gpuPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileMLIRAction fcfg mode outpath) prog
