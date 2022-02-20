{-# LANGUAGE FlexibleContexts #-}

-- | @futhark pymetal@
module Futhark.CLI.PyMetal (main) where

import Futhark.Actions (compilePyOpenCLAction)
import Futhark.Compiler.CLI
import Futhark.Passes

-- | Run @futhark pymetal@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile PyMetal"
  "Generate Python + Metal code from optimised Futhark program."
  gpuPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compilePyOpenCLAction fcfg mode outpath) prog
