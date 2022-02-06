-- | Imperative code with an Metal component.
--
-- Apart from ordinary imperative code, this also carries around a
-- Metal Program as a string, as well as a list of kernels defined by
-- the Metal Library.
--
-- The imperative code has been augmented with a 'LaunchKernel'
-- operation that allows one to execute a Metal kernel.
module Futhark.CodeGen.ImpCode.Metal
  ( Program (..),
    Function,
    FunctionT (Function),
    Code,
    KernelName,
    KernelArg (..),
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
import Futhark.CodeGen.ImpCode hiding (Code, Function)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.IR.GPU.Sizes
import Futhark.Util.Pretty


{-
    This is what CodeGen must create

    -- By Default, Metallib is made at compilation time however this is an alternative test to run const chars as Metal Scripts
    -- Possibly more Important for Futhark
    void generateMetalLib(const char *src, mtlpp::Device device){
    -- Example *src:
    -- const char shadersSrc[] = 
    --         "#include <metal_stdlib>";
    --         "using namespace metal;";
    --         "kernel void sqr(";
    --             "const device float *vIn [[ buffer(0) ]],";
    --             "device float *vOut [[ buffer(1) ]],";
    --             "uint id[[ thread_position_in_grid ]])";
    --         "{";
    --             "vOut[id] = vIn[id] * vIn[id];";       
    --         "}";

        ns::Error* error = NULL; //nullptr
        
        mtlpp::Library library  = device.NewLibrary(src, mtlpp::CompileOptions(), error);
        assert(library);
        mtlpp::Function sqrFunc = library.NewFunction("sqr");
        assert(sqrFunc);

        mtlpp::ComputePipelineState computePipelineState = device.NewComputePipelineState(sqrFunc, error);
        assert(computePipelineState);

        mtlpp::CommandQueue commandQueue = device.NewCommandQueue();
        assert(commandQueue);
    }
-}

-- | An program calling Metal kernels.
data Program = Program
  { metalProgram :: T.Text,
    -- | Must be prepended to the program.
    metalPrelude :: T.Text,
    metalKernelNames :: M.Map KernelName KernelSafety,
    -- | So we can detect whether the device is capable.
    metalUsedTypes :: [PrimType],
    -- | Runtime-Immutable constants.
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

-- | A function calling Metal kernels. 
type Function = Imp.Function Metal

-- | A piece of code calling OpenCL.
type Code = Imp.Code Metal

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
  = LaunchKernel KernelSafety KernelName [KernelArg] [Exp] [Exp] --(AKA THIS IS WHERE sendComputeCommand() IS CALLED)
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
