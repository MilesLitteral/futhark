{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Various boilerplate definitions for the Metal backend.
module Futhark.CodeGen.Backends.Metal.Boilerplate
  ( generateBoilerplate )
where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.ImpCode.Metal
import Futhark.CodeGen.RTS.C (metalH, freeListH)
import Futhark.Util (chunk, zEncodeString)
import qualified Language.C.Quote.OpenCL as C
import qualified Language.C.Syntax as C

errorMsgNumArgs :: ErrorMsg a -> Int
errorMsgNumArgs = length . errorMsgArgTypes

-- | Called after most code has been generated to generate the bulk of
-- the boilerplate.
generateBoilerplate ::
  T.Text ->
  T.Text ->
  M.Map KernelName KernelSafety ->
  M.Map Name SizeClass ->
  [FailureMsg] ->
  GC.CompilerM Metal () ()
generateBoilerplate metal_program metal_prelude kernels sizes failures = do
  mapM_
    GC.earlyDecl
    [C.cunit|
      $esc:(T.unpack freeListH)
      $esc:(T.unpack metalH)
      const unsigned int arrayLength = 10; //1 << 24;
      const unsigned int bufferSize = arrayLength * sizeof(float);

      $esc:(T.unpack metal_program)
  |]
