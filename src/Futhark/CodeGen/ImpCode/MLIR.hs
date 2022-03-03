-- | Imperative code with an MLIR component.
--
-- Apart from ordinary imperative code, this also carries around an
-- MLIR program as a string, as well as a list of kernels defined by
-- the MLIR program.
--
-- The imperative code has been augmented with a 'LaunchKernel'
-- operation that allows one to execute an MLIR kernel.
module Futhark.CodeGen.ImpCode.MLIR
  ( Program (..),
    KernelName,
    KernelArg (..),
    CLCode,
    MLIR (..),
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

-- | An program calling MLIR kernels.
data Program = Program
  { mlirProgram :: T.Text,
    -- | Must be prepended to the program.
    mlirPrelude :: T.Text,
    mlirKernelNames :: M.Map KernelName KernelSafety,
    -- | So we can detect whether the device is capable.
    mlirUsedTypes :: [PrimType],
    -- | Runtime-configurable constants.
    mlirSizes :: M.Map Name SizeClass,
    -- | Assertion failure error messages.
    mlirFailures :: [FailureMsg],
    hostDefinitions :: Definitions MLIR
  }

-- | Something that can go wrong in a kernel.  Part of the machinery
-- for reporting error messages from within kernels.
data FailureMsg = FailureMsg
  { failureError :: ErrorMsg Exp,
    failureBacktrace :: String
  }

-- | A piece of code calling MLIR.
type CLCode = Code MLIR

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

-- | Host-level MLIR operation.
data MLIR
  = LaunchKernel KernelSafety KernelName [KernelArg] [Exp] [Exp]
  | GetSize VName Name
  | CmpSizeLe VName Name Exp
  | GetSizeMax VName SizeClass
  deriving (Show)

-- | The target platform when compiling imperative code to a 'Program'
data KernelTarget
  = TargetMLIR
  deriving (Eq)

instance Pretty MLIR where
  ppr = text . show
