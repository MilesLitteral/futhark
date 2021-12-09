{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Various boilerplate definitions for the Metal(MSL) backend.
module Futhark.CodeGen.Backends.Metal.Boilerplate
  ( generateBoilerplate,
    profilingEnclosure,
    module Futhark.CodeGen.Backends.Metal.Boilerplate,
  )
where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
--import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.Util (chunk, zEncodeString)
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.RTS.C (metalH, freeListH, metalC)
--import qualified Language.C.Quote.OpenCL as C
import qualified Language.C.Quote.ObjC as ObjC
import qualified Language.C.Syntax as C

errorMsgNumArgs :: ErrorMsg a -> Int
errorMsgNumArgs = length . errorMsgArgTypes

-- | Called after most code has been generated to generate the bulk of
-- the boilerplate.
generateBoilerplate ::
  T.Text ->
  T.Text ->
  [Name] ->
  M.Map KernelName KernelSafety ->
  M.Map Name SizeClass ->
  [FailureMsg] ->
  GC.CompilerM Metal () ()
generateBoilerplate metal_program metal_prelude kernels sizes failures = do
  mapM_
    GC.earlyDecl
    [C.cunit|
      $esc:("#include <metal_stdlib>")
      $esc:(T.unpack freeListH)
      $esc:(T.unpack metalH)

      using namespace metal;
      // This is a Metal Shading Language (MSL) function equivalent 
      // to the add_arrays() C function, used to perform the 
      //  calculation on a GPU.
      
      //modify this so that any function can be generically piped into this location
      //$esc:(T.unpack metalC)?

      kernel void add_arrays(device const float* inA,
                            device const float* inB,
                            device float* result,
                            uint index [[thread_position_in_grid]])
      {
          // the for-loop is replaced with a collection of threads, each of which
          // calls this function.
          result[index] = inA[index] + inB[index];
      }
|]




