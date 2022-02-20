{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Various boilerplate definitions for the Metal backend.
module Futhark.CodeGen.Backends.CMetal.Boilerplate
  ( generateBoilerplate,
    profilingEnclosure,
    module Futhark.CodeGen.Backends.CMetal.Boilerplate,
  )
where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Futhark.CodeGen.Backends.CMetal.Boilerplate
  ( copyDevToDev,
    copyDevToHost,
    copyHostToDev,
    copyScalarFromDev,
    copyScalarToDev,
    costCentreReport,
    failureSwitch,
    kernelRuns,
    kernelRuntime,
  )
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.ImpCode.Metal
import Futhark.CodeGen.RTS.C (metalH, freeListH)
import Futhark.Util (chunk, zEncodeString)
import qualified Language.C.Quote.Metal as C
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

  GC.profileReport [C.citem|assert(device);|] -- Maybe?
  mapM_ GC.profileReport $ M.keys kernels
  where
    fragments =
      [ [C.cinit|$string:s|]
        | s <- chunk 2000 $ T.unpack $ metal_prelude <> metal_program
      ]
