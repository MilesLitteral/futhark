-- | Imperative code with an Metal component.
--
-- Apart from ordinary imperative code, this also carries around an
-- Metal program as a string, as well as a list of kernels defined by
-- the Metal program.
--
-- The imperative code has been augmented with a 'LaunchKernel'
-- operation that allows one to execute an Metal kernel.
module Futhark.CodeGen.ImpCode.Metal
  ( Program (..),
    KernelName,
    KernelArg (..),
    CLCode,
    Metal (..),
    KernelSafety (..),
    numFailureParams,
    KernelTarget (..),
    FailureMsg (..),
    module Futhark.CodeGen.ImpCode,
    module Futhark.IR.GPU.Sizes,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import Futhark.CodeGen.ImpCode
import Futhark.IR.GPU.Sizes
import Futhark.Util.Pretty

-- | An program calling Metal kernels.
data Program = Program
  { metalProgram :: T.Text,
    -- | Must be prepended to the program.
    metalPrelude :: T.Text,
    metalKernelNames :: M.Map KernelName KernelSafety,
    -- | So we can detect whether the device is capable.
    metalUsedTypes :: [PrimType],
    -- | Runtime-configurable constants.
    metalSizes :: M.Map Name SizeClass,
    -- | Assertion failure error messages.
    metalFailures :: [FailureMsg],
    hostDefinitions :: Definitions Metal
  }

-- | Something that can go wrong in a kernel.  Part of the machinery
-- for reporting error messages from within kernels.
data FailureMsg = FailureMsg
  { failureError :: ErrorMsg Exp,
    failureBacktrace :: String
  }

-- | A piece of code calling Metal.
type CLCode = Code Metal

-- | The name of a kernel.
type KernelName = Name

-- | An argument to be passed to a kernel.
data KernelArg
  = -- | Pass the value of this scalar expression as argument.
    ValueKArg Exp PrimType
  | -- | Pass this pointer as argument.
    MemKArg VName
  | -- | Create this much local memory per workgroup.
    SharedMemoryKArg (Count Bytes Exp)
  deriving (Show)

-- | Whether a kernel can potentially fail (because it contains bounds
-- checks and such).
data MayFail = MayFail | CannotFail
  deriving (Show)

-- | Information about bounds checks and how sensitive it is to
-- errors.  Ordered by least demanding to most.
data KernelSafety
  = -- | Does not need to know if we are in a failing state, and also
    -- cannot fail.
    SafetyNone
  | -- | Needs to be told if there's a global failure, and that's it,
    -- and cannot fail.
    SafetyCheap
  | -- | Needs all parameters, may fail itself.
    SafetyFull
  deriving (Eq, Ord, Show)

-- | How many leading failure arguments we must pass when launching a
-- kernel with these safety characteristics.
numFailureParams :: KernelSafety -> Int
numFailureParams SafetyNone = 0
numFailureParams SafetyCheap = 1
numFailureParams SafetyFull = 3

-- | Host-level Metal operation.
data Metal
  = LaunchKernel KernelSafety KernelName [KernelArg] [Exp] [Exp]
  | GetSize VName Name
  | CmpSizeLe VName Name Exp
  | GetSizeMax VName SizeClass
  deriving (Show)

-- | The target platform when compiling imperative code to a 'Program'
data KernelTarget
  = TargetMetal
  deriving (Eq)

instance Pretty Metal where
  ppr = text . show
