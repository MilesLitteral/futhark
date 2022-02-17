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

-- | Block items to put before and after a thing to be profiled.
profilingEnclosure :: Name -> ([C.BlockItem], [C.BlockItem])
profilingEnclosure name =
  ( [C.citems|
      typename metalEvent_t *pevents = NULL;
      if (ctx->profiling && !ctx->profiling_paused) {
        pevents = metal_get_events(&ctx->metal,
                                  &ctx->$id:(kernelRuns name),
                                  &ctx->$id:(kernelRuntime name));
        assert(metalEventRecord(pevents[0], 0));
      }
      |],
    [C.citems|
      if (pevents != NULL) {
        assert(metalEventRecord(pevents[1], 0));
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
      $esc:("#include <mtlpp.hpp>")
      $esc:(T.unpack freeListH)
      $esc:(T.unpack metalH)
      const char *metal_program[] = {$inits:fragments, NULL};
      ns::Error* error = NULL;
      |]

  generateSizeFuns sizes
  cfg <- generateConfigFuns sizes
  generateContextFuns cfg cost_centres kernels sizes failures

  GC.profileReport [C.citem|assert(metal_tally_profiling_records(&ctx->metal));|]
  mapM_ GC.profileReport $ costCentreReport $ cost_centres ++ M.keys kernels
  where
    fragments =
      [ [C.cinit|$string:s|]
        | s <- chunk 2000 $ T.unpack $ metal_prelude <> metal_program
      ]

generateSizeFuns :: M.Map Name SizeClass -> GC.CompilerM Metal () ()
generateSizeFuns sizes = do
  let size_name_inits = map (\k -> [C.cinit|$string:(pretty k)|]) $ M.keys sizes
      size_var_inits = map (\k -> [C.cinit|$string:(zEncodeString (pretty k))|]) $ M.keys sizes
      size_class_inits = map (\c -> [C.cinit|$string:(pretty c)|]) $ M.elems sizes

  GC.earlyDecl [C.cedecl|static const char *tuning_param_names[] = { $inits:size_name_inits };|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_vars[] = { $inits:size_var_inits };|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_classes[] = { $inits:size_class_inits };|]

generateConfigFuns :: M.Map Name SizeClass -> GC.CompilerM Metal () String
generateConfigFuns sizes = do
  let size_decls = map (\k -> [C.csdecl|typename int64_t *$id:k;|]) $ M.keys sizes
      num_sizes = M.size sizes
  GC.earlyDecl [C.cedecl|struct tuning_params { $sdecls:size_decls };|]
  let size_value_inits = zipWith sizeInit [0 .. M.size sizes -1] (M.elems sizes)
      sizeInit i size = [C.cstm|cfg->tuning_params[$int:i] = $int:val;|]
        where
          val = fromMaybe 0 $ sizeDefault size
  GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:cfg* $id:s(void);|],
      [C.cedecl|struct $id:cfg* $id:s(void) {
                            mtlpp::Device device = mtlpp::Device::CreateSystemDefaultDevice();
                         return device;
                       }|]
    )

  GC.publicDef_ "context_config_set_device" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(mtlpp::Device cfg,   const char *s);|],
      [C.cedecl|void $id:s(mtlpp::Device cfg,   const char *s) {
                         device = mtlpp::Device::CreateSystemDefaultDevice();
                       }|]
    )
  return cfg

generateContextFuns ::
  String ->
  [Name] ->
  M.Map KernelName KernelSafety ->
  M.Map Name SizeClass ->
  [FailureMsg] ->
  GC.CompilerM Metal () ()
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
        ( [C.csdecl|typename mtlpp::Function $id:name;|],
          [C.cstm|assert(cuModuleGetFunction(
                                     &ctx->$id:name,
                                     ctx->metal.module,
                                     $string:(pretty (C.toIdent name mempty))));|]
        ) :
        forCostCentre name

      (kernel_fields, init_kernel_fields) =
        unzip $
          concatMap forKernel (M.keys kernels)
            ++ concatMap forCostCentre cost_centres

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
                 assert(!cfg->in_use);
                 struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                 if (ctx == NULL) {
                   return NULL;
                 }
                 ctx->cfg = cfg;
                 ctx->cfg->in_use = 1;
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

                 ctx->metal.cfg = cfg->cu_cfg;
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
                 assert(cuMemAlloc(&ctx->global_failure, sizeof(no_error)));
                 assert(cuMemcpyHtoD(ctx->global_failure, &no_error, sizeof(no_error)));
                 // The +1 is to avoid zero-byte allocations.
                 assert(cuMemAlloc(&ctx->global_failure_args, sizeof(int64_t)*($int:max_failure_args+1)));

                 $stms:init_kernel_fields

                 $stms:final_inits
                 $stms:set_tuning_params

                 init_constants(ctx);
                 // Clear the free list of any deallocations that occurred while initialising constants.
                 assert(metal_free_all(&ctx->metal));

                 futhark_context_sync(ctx);

                 return ctx;
               }|]
    )

  GC.publicDef_ "context_sync" GC.MiscDecl $ \s ->
    ( [C.cedecl|int $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                 assert(cuCtxPushCurrent(ctx->metal.cu_ctx));
                 assert(cuCtxSynchronize());
                 if (ctx->failure_is_an_option) {
                   // Check for any delayed error.
                   typename int32_t failure_idx;
                   assert(
                     cuMemcpyDtoH(&failure_idx,
                                  ctx->global_failure,
                                  sizeof(int32_t)));
                   ctx->failure_is_an_option = 0;

                   if (failure_idx >= 0) {
                     // We have to clear global_failure so that the next entry point
                     // is not considered a failure from the start.
                     typename int32_t no_failure = -1;
                     assert(
                       cuMemcpyHtoD(ctx->global_failure,
                                    &no_failure,
                                    sizeof(int32_t)));

                     typename int64_t args[$int:max_failure_args+1];
                     assert(
                       cuMemcpyDtoH(&args,
                                    ctx->global_failure_args,
                                    sizeof(args)));

                     $stm:(failureSwitch failures)

                     return FUTHARK_PROGRAM_ERROR;
                   }
                 }
                 CUDA_SUCCEED_OR_RETURN(cuCtxPopCurrent(&ctx->metal.cu_ctx));
                 return 0;
               }|]
    )
