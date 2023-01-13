module Futhark.CodeGen.Backends.MLIR.Boilerplate (generateBoilerplate) where

import MLIR.AST
import MLIR.AST.Serialize
import Test.Hspec

generateBoilerplate :: Spec -> IO ()
generateBoilerplate spec = hspec spec