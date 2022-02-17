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
                mtlpp::Device device = mtlpp::Device::CreateSystemDefaultDevice();
                return device;
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
