{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}

-- | Code generation for server executables.
module Futhark.CodeGen.Backends.GenericC.Server
  ( serverDefs,
  )
where

import Data.Bifunctor (first, second)
import qualified Data.Map as M
import qualified Data.Text as T
import Futhark.CodeGen.Backends.GenericC.Manifest
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.SimpleRep
import Futhark.CodeGen.RTS.ObjC (serverH, tuningH, valuesH)
import Futhark.Util (zEncodeString)
import Futhark.Util.Pretty (prettyText)
--import qualified Language.C.Quote.OpenCL as C
import qualified Language.ObjC.Syntax as ObjC


entryBoilerplate :: Manifest -> ([ObjC.Definition], [ObjC.Initializer])
entryBoilerplate manifest =
  first concat $
    unzip $
      map (oneEntryBoilerplate manifest) $
        M.toList $ manifestEntryPoints manifest

mkBoilerplate ::
  Manifest ->
  ([ObjC.Definition], [ObjC.Initializer], [ObjC.Initializer])
mkBoilerplate manifest =
  let (type_inits, type_defs) = entryTypeBoilerplate manifest
      (entry_defs, entry_inits) = entryBoilerplate manifest
      scalar_type_inits = map scalarTypeInit scalar_types
   in (type_defs ++ entry_defs, scalar_type_inits ++ type_inits, entry_inits)
  where
    scalarTypeInit tname = [ObjC.cinit|&$id:(typeStructName tname)|]
    scalar_types =
      [ "i8",
        "i16",
        "i32",
        "i64",
        "u8",
        "u16",
        "u32",
        "u64",
        "f16",
        "f32",
        "f64",
        "bool"
      ]