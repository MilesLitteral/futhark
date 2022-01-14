{-# LANGUAGE OverloadedStrings #-}

-- | ObjC manifest data structure and serialisation to JSON.
--
-- A manifest contains machine-readable information about the API of
-- the compiled Futhark program.  Specifically which entry points are
-- available, which types are exposed, and what their C names are.
module Futhark.CodeGen.Backends.GenericObjC.Manifest
  ( Manifest (..),
    Input (..),
    Output (..),
    EntryPoint (..),
    Type (..),
    ArrayOps (..),
    OpaqueOps (..),
    manifestToJSON,
  )
where

import Data.Aeson (ToJSON (..), object)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (bimap)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
