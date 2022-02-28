

-- | Code generation for ImpCode with Metal kernels.
module Futhark.CodeGen.ImpGen.Metal
  ( compileProg,
    Warnings,
  )
where

import Data.Bifunctor (second)
import qualified Futhark.CodeGen.ImpCode.Metal as Metal
import Futhark.CodeGen.ImpGen.GPU
import Futhark.CodeGen.ImpGen.GPU.ToMetal
import Futhark.IR.GPUMem
import Futhark.MonadFreshNames
import Language.Futhark.Warnings (Warnings)

-- | Compile the program to ImpCode with Metal kernels.
compileProg :: MonadFreshNames m => Prog GPUMem -> m (Warnings, Metal.Program)
compileProg prog = second kernelsToMetal <$> compileProgMetal prog

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