{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Various boilerplate definitions for the Metal(MSL) backend.
module Futhark.CodeGen.Backends.Metal.Boilerplate
  ( generateBoilerplate,
    profilingEnclosure,
    module Futhark.CodeGen.Backends.COpenCL.Boilerplate,
  )
where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Futhark.CodeGen.Backends.COpenCL.Boilerplate
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
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.RTS.C (metalH, freeListH)
import Futhark.Util (chunk, zEncodeString)
import qualified Language.C.Quote.OpenCL as C
import qualified Language.C.Syntax as C

errorMsgNumArgs :: ErrorMsg a -> Int
errorMsgNumArgs = length . errorMsgArgTypes

-- | Block items to put before and after a thing to be profiled.
profilingEnclosure :: Name -> ([C.BlockItem], [C.BlockItem])
profilingEnclosure name =
  ( [C.citems|
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
  )

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
generateBoilerplate metal_program metal_prelude cost_centres kernels sizes failures = do
  mapM_
    GC.earlyDecl
    [C.cunit|
      $esc:("#include <Foundation/Foundation.h>")
      $esc:("#include <Metal/Metal.h>")
      $esc:("#include "MetalProgram.h" ") //Futhark Generated Code of Code to Execute on GPU
      $esc:(T.unpack freeListH)
      $esc:(T.unpack metalH)


// This is the C version of the function that the sample
// implements in Metal Shading Language.
void add_arrays(const float* inA,
                const float* inB,
                float* result,
                int length)
{
    for (int index = 0; index < length ; index++)
    {
        result[index] = inA[index] + inB[index];
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        id<MTLDevice> device = MTLCreateSystemDefaultDevice();

        // Create the custom object used to encapsulate the Metal code.
        // Initializes objects to communicate with the GPU.
        MetalAdder* adder = [[MetalAdder alloc] initWithDevice:device];
        
        // Create buffers to hold data
        [adder prepareData];
        
        // Send a command to the GPU to perform the calculation.
        [adder sendComputeCommand];

        NSLog(@"Execution finished");
    }
    return 0;
}
      |]

  generateSizeFuns sizes
  cfg <- generateConfigFuns sizes
  generateContextFuns cfg cost_centres kernels sizes failures

  GC.profileReport [C.citem|METAL_SUCCEED_FATAL(metal_tally_profiling_records(&ctx->metal));|]
  mapM_ GC.profileReport $ costCentreReport $ cost_centres ++ M.keys kernels
  where
    fragments =
      [ [C.cinit|$string:s|]
        | s <- chunk 2000 $ T.unpack $ metal_prelude <> metal_program
      ]

generateSizeFuns :: M.Map Name SizeClass -> GC.CompilerM OpenCL () ()
generateSizeFuns sizes = do
  let size_name_inits = map (\k -> [C.cinit|$string:(pretty k)|]) $ M.keys sizes
      size_var_inits = map (\k -> [C.cinit|$string:(zEncodeString (pretty k))|]) $ M.keys sizes
      size_class_inits = map (\c -> [C.cinit|$string:(pretty c)|]) $ M.elems sizes

  GC.earlyDecl [C.cedecl|static const char *tuning_param_names[] = { $inits:size_name_inits };|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_vars[] = { $inits:size_var_inits };|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_classes[] = { $inits:size_class_inits };|]

generateConfigFuns :: M.Map Name SizeClass -> GC.CompilerM OpenCL () String
generateConfigFuns sizes = do
  let size_decls = map (\k -> [C.csdecl|typename int64_t *$id:k;|]) $ M.keys sizes
      num_sizes = M.size sizes
  GC.earlyDecl [C.cedecl|struct tuning_params { $sdecls:size_decls };|]
  cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:s;|],
      [C.cedecl|struct $id:s { struct metal_config cu_cfg;
                              int profiling;
                              typename int64_t tuning_params[$int:num_sizes];
                              int num_nvrtc_opts;
                              const char **nvrtc_opts;
                            };|]
    )

  GC.publicDef_ "context_config_free" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                         free(cfg->nvrtc_opts);
                         free(cfg);
                       }|]
    )

  GC.publicDef_ "context_config_add_nvrtc_option" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *opt);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *opt) {
                         cfg->nvrtc_opts[cfg->num_nvrtc_opts] = opt;
                         cfg->num_nvrtc_opts++;
                         cfg->nvrtc_opts = (const char**) realloc(cfg->nvrtc_opts, (cfg->num_nvrtc_opts+1) * sizeof(const char*));
                         cfg->nvrtc_opts[cfg->num_nvrtc_opts] = NULL;
                       }|]
    )

generateContextFuns ::
  String ->
  [Name] ->
  M.Map KernelName KernelSafety ->
  M.Map Name SizeClass ->
  [FailureMsg] ->
  GC.CompilerM OpenCL () ()
generateContextFuns cfg cost_centres kernels sizes failures = do
  final_inits <- GC.contextFinalInits
  (fields, init_fields, free_fields) <- GC.contextContents
  let forCostCentre name =
        [ ( [C.csdecl|typename int64_t $id:(kernelRuntime name);|],
            [C.cstm|ctx->$id:(kernelRuntime name) = 0;|]
          ),
          ( [C.csdecl|int $id:(kernelRuns name);|],
            [C.cstm|ctx->$id:(kernelRuns name) = 0;|]
          )
        ]

      forKernel name =
        ( [C.csdecl|typename CUfunction $id:name;|],
          [C.cstm|METAL_SUCCEED_FATAL(cuModuleGetFunction(
                                     &ctx->$id:name,
                                     ctx->netal.module,
                                     $string:(pretty (C.toIdent name mempty))));|]
        ) :
        forCostCentre name

      (kernel_fields, init_kernel_fields) =
        unzip $
          concatMap forKernel (M.keys kernels)
            ++ concatMap forCostCentre cost_centres

  ctx <- GC.publicDef "context" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:s;|],
      [C.cedecl|struct $id:s {
                         int detail_memory;
                         int debugging;
                         int profiling;
                         int profiling_paused;
                         int logging;
                         typename lock_t lock;
                         char *error;
                         typename FILE *log;
                         $sdecls:fields
                         $sdecls:kernel_fields
                         typename CUdeviceptr global_failure;
                         typename CUdeviceptr global_failure_args;
                         struct metal_context metal;
                         struct tuning_params tuning_params;
                         // True if a potentially failing kernel has been enqueued.
                         typename int32_t failure_is_an_option;

                         int total_runs;
                         long int total_runtime;
                       };|]
    )

  let set_tuning_params =
        zipWith
          (\i k -> [C.cstm|ctx->tuning_params.$id:k = &cfg->tuning_params[$int:i];|])
          [(0 :: Int) ..]
          $ M.keys sizes
      max_failure_args =
        foldl max 0 $ map (errorMsgNumArgs . failureError) failures

  GC.publicDef_ "context_new" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg);|],
      [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg) {
                 struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                 if (ctx == NULL) {
                   return NULL;
                 }
                 ctx->debugging = ctx->detail_memory = cfg->cu_cfg.debugging;
                 ctx->profiling = cfg->profiling;
                 ctx->profiling_paused = 0;
                 ctx->logging = cfg->cu_cfg.logging;
                 ctx->error = NULL;
                 ctx->log = stderr;
                 ctx->metal.profiling_records_capacity = 200;
                 ctx->metal.profiling_records_used = 0;
                 ctx->metal.profiling_records =
                   malloc(ctx->metal.profiling_records_capacity *
                          sizeof(struct profiling_record));

                 ctx->metal.cfg = cfg->msl_cfg; --cu_cfg
                 create_lock(&ctx->lock);

                 ctx->failure_is_an_option = 0;
                 ctx->total_runs = 0;
                 ctx->total_runtime = 0;
                 $stms:init_fields

                 ctx->error = metal_setup(&ctx->metal, metal_program, cfg->nvrtc_opts);

                 if (ctx->error != NULL) {
                   return NULL;
                 }

                 typename int32_t no_error = -1;
                 METAL_SUCCEED_FATAL(cuMemAlloc(&ctx->global_failure, sizeof(no_error)));
                 METAL_SUCCEED_FATAL(cuMemcpyHtoD(ctx->global_failure, &no_error, sizeof(no_error)));
                 // The +1 is to avoid zero-byte allocations.
                 METAL_SUCCEED_FATAL(cuMemAlloc(&ctx->global_failure_args, sizeof(int64_t)*($int:max_failure_args+1)));

                 $stms:init_kernel_fields

                 $stms:final_inits
                 $stms:set_tuning_params

                 init_constants(ctx);
                 // Clear the free list of any deallocations that occurred while initialising constants.
                 METAL_SUCCEED_FATAL(metal_free_all(&ctx->metal));

                 futhark_context_sync(ctx);

                 return ctx;
               }|]
    )

  GC.publicDef_ "context_free" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                                 $stms:free_fields
                                 free_constants(ctx);
                                 metal_cleanup(&ctx->metal);
                                 free_lock(&ctx->lock);
                                 free(ctx);
                               }|]
    )

  {- potentially not necessary or has to be rethought because of how
  Objective-C handles memory
  GC.onClear
    [C.citem|if (ctx->error == NULL) {
               METAL_SUCCEED_NONFATAL(metal_free_all(&ctx->metal));
             }|]-}
