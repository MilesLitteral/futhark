{-# LANGUAGE FlexibleContexts #-}

-- | @futhark metal@
module Futhark.CLI.Metal (main) where

import Futhark.Actions (compileMetalAction)
import Futhark.Compiler.CLI
import Futhark.Passes (gpuPipeline)

-- | Run @futhark metal@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile Metal"
  "Generate Metal/MSL code from optimised Futhark program."
  gpuPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileMetalAction fcfg mode outpath) prog
