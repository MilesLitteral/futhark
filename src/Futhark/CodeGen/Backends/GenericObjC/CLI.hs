{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}

-- | Code generation for standalone executables.
module Futhark.CodeGen.Backends.GenericObjC.CLI
  ( cliDefs,
  )
where

import Data.List (unzip5)
import qualified Data.Map as M
import qualified Data.Text as T
import Futhark.CodeGen.Backends.GenericObjC.Manifest
import Futhark.CodeGen.Backends.GenericObjC.Options
{-import Futhark.CodeGen.Backends.SimpleRep
  ( cproduct,
    primAPIType,
    primStorageType,
    scalarToPrim,
  )
-}

import Futhark.CodeGen.RTS.ObjC (tuningH, valuesH)
import Futhark.Util.Pretty (pretty, prettyText)
--import qualified Language.C.Quote.OpenCL as C
import qualified Language.ObjC.Syntax as ObjC

genericOptions :: [Option]
genericOptions =
  [ Option
      { optionLongName = "write-runtime-to",
        optionShortName = Just 't',
        optionArgument = RequiredArgument "FILE",
        optionDescription = "Print the time taken to execute the program to the indicated file, an integral number of microseconds.",
        optionAction = set_runtime_file
      },
    Option
      { optionLongName = "runs",
        optionShortName = Just 'r',
        optionArgument = RequiredArgument "INT",
        optionDescription = "Perform NUM runs of the program.",
        optionAction = set_num_runs
      },
    Option
      { optionLongName = "debugging",
        optionShortName = Just 'D',
        optionArgument = NoArgument,
        optionDescription = "Perform possibly expensive internal correctness checks and verbose logging.",
        optionAction = [ObjC.cstm|futhark_context_config_set_debugging(cfg, 1);|]
      },
    Option
      { optionLongName = "log",
        optionShortName = Just 'L',
        optionArgument = NoArgument,
        optionDescription = "Print various low-overhead logging information to stderr while running.",
        optionAction = [ObjC.cstm|futhark_context_config_set_logging(cfg, 1);|]
      },
    Option
      { optionLongName = "entry-point",
        optionShortName = Just 'e',
        optionArgument = RequiredArgument "NAME",
        optionDescription = "The entry point to run. Defaults to main.",
        optionAction = [ObjC.cstm|if (entry_point != NULL) entry_point = optarg;|]
      },
    Option
      { optionLongName = "binary-output",
        optionShortName = Just 'b',
        optionArgument = NoArgument,
        optionDescription = "Print the program result in the binary output format.",
        optionAction = [ObjC.cstm|binary_output = 1;|]
      },
    Option
      { optionLongName = "no-print-result",
        optionShortName = Just 'n',
        optionArgument = NoArgument,
        optionDescription = "Do not print the program result.",
        optionAction = [ObjC.cstm|print_result = 0;|]
      },
    Option
      { optionLongName = "help",
        optionShortName = Just 'h',
        optionArgument = NoArgument,
        optionDescription = "Print help information and exit.",
        optionAction =
          [ObjC.cstm|{
                   printf("Usage: %s [OPTION]...\nOptions:\n\n%s\nFor more information, consult the Futhark User's Guide or the man pages.\n",
                          fut_progname, option_descriptions);
                   exit(0);
                  }|]
      },
    Option
      { optionLongName = "print-params",
        optionShortName = Nothing,
        optionArgument = NoArgument,
        optionDescription = "Print all tuning parameters that can be set with --param or --tuning.",
        optionAction =
          [ObjC.cstm|{
                int n = futhark_get_tuning_param_count();
                for (int i = 0; i < n; i++) {
                  printf("%s (%s)\n", futhark_get_tuning_param_name(i),
                                      futhark_get_tuning_param_class(i));
                }
                exit(0);
              }|]
      },
    Option
      { optionLongName = "param",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "ASSIGNMENT",
        optionDescription = "Set a tuning parameter to the given value.",
        optionAction =
          [ObjC.cstm|{
                char *name = optarg;
                char *equals = strstr(optarg, "=");
                char *value_str = equals != NULL ? equals+1 : optarg;
                int value = atoi(value_str);
                if (equals != NULL) {
                  *equals = 0;
                  if (futhark_context_config_set_tuning_param(cfg, name, (size_t)value) != 0) {
                    futhark_panic(1, "Unknown size: %s\n", name);
                  }
                } else {
                  futhark_panic(1, "Invalid argument for size option: %s\n", optarg);
                }}|]
      },
    Option
      { optionLongName = "tuning",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "FILE",
        optionDescription = "Read size=value assignments from the given file.",
        optionAction =
          [ObjC.cstm|{
                char *ret = load_tuning_file(optarg, cfg, (int(*)(void*, const char*, size_t))
                                                          futhark_context_config_set_tuning_param);
                if (ret != NULL) {
                  futhark_panic(1, "When loading tuning from '%s': %s\n", optarg, ret);
                }}|]
      }
  ]
  where
    set_runtime_file =
      [C.cstm|{
          runtime_file = fopen(optarg, "w");
          if (runtime_file == NULL) {
            futhark_panic(1, "Cannot open %s: %s\n", optarg, strerror(errno));
          }
        }|]
    set_num_runs =
      [C.cstm|{
          num_runs = atoi(optarg);
          perform_warmup = 1;
          if (num_runs <= 0) {
            futhark_panic(1, "Need a positive number of runs, not %s\n", optarg);
          }
        }|]

readInput :: Manifest -> Int -> T.Text -> ([ObjC.BlockItem], ObjC.Stm, ObjC.Stm, ObjC.Stm, ObjC.Exp)
readInput manifest i tname =
  case M.lookup tname $ manifestTypes manifest of
    Nothing ->
      let (_, t) = scalarToPrim tname
          dest = "read_value_" ++ show i
          info = T.unpack tname <> "_info"
       in ( [ObjC.citems|
             $ty:(primStorageType t) $id:dest;
             if (read_scalar(stdin, &$id:info, &$id:dest) != 0) {
             futhark_panic(1, "Error when reading input #%d of type %s (errno: %s).\n",
                           $int:i,
                           $string:(T.unpack tname),
                           strerror(errno));
                           };|],
            [ObjC.cstm|;|],
            [ObjC.cstm|;|],
            [ObjC.cstm|;|],
            [ObjC.cexp|$id:dest|]
          )
    Just (TypeOpaque desc _) ->
      ( [ObjC.citems|futhark_panic(1, "Cannot read input #%d of type %s\n", $int:i, $string:(T.unpack desc));|],
        [ObjC.cstm|;|],
        [ObjC.cstm|;|],
        [ObjC.cstm|;|],
        [ObjC.cexp|NULL|]
      )
    Just (TypeArray t et rank ops) ->
      let dest = "read_value_" ++ show i
          shape = "read_shape_" ++ show i
          arr = "read_arr_" ++ show i

          ty = [ObjC.cty|typename $id:t|]
          dims_exps = [[ObjC.cexp|$id:shape[$int:j]|] | j <- [0 .. rank -1]]
          t' = uncurry primAPIType $ scalarToPrim et

          new_array = arrayNew ops
          free_array = arrayFree ops
          info = T.unpack et <> "_info"

          items =
            [ObjC.citems|
               $ty:ty $id:dest;
               typename int64_t $id:shape[$int:rank];
               $ty:t' *$id:arr = NULL;
               errno = 0;
               if (read_array(stdin,
                              &$id:info,
                              (void**) &$id:arr,
                              $id:shape,
                              $int:rank)
                   != 0) {
                 futhark_panic(1, "Cannot read input #%d of type %s%s (errno: %s).\n",
                               $int:i,
                               $string:(T.unpack tname),
                               $id:info.type_name,
                               strerror(errno));
               }|]
       in ( items,
            [ObjC.cstm|assert(($id:dest = $id:new_array(ctx, $id:arr, $args:dims_exps)) != NULL);|],
            [ObjC.cstm|assert($id:free_array(ctx, $id:dest) == 0);|],
            [ObjC.cstm|free($id:arr);|],
            [ObjC.cexp|$id:dest|]
          )

readInputs :: Manifest -> [T.Text] -> [([ObjC.BlockItem], ObjC.Stm, C.Stm, ObjC.Stm, ObjC.Exp)]
readInputs manifest = zipWith (readInput manifest) [0 ..]

prepareOutputs :: Manifest -> [T.Text] -> [(ObjC.BlockItem, ObjC.Exp, ObjC.Stm)]
prepareOutputs manifest = zipWith prepareResult [(0 :: Int) ..]
  where
    prepareResult i tname = do
      let result = "result_" ++ show i

      case M.lookup tname $ manifestTypes manifest of
        Nothing ->
          let (s, pt) = scalarToPrim tname
              ty = primAPIType s pt
           in ( [ObjC.citem|$ty:ty $id:result;|],
                [ObjC.cexp|$id:result|],
                [ObjC.cstm|;|]
              )
        Just (TypeArray t _ _ ops) ->
          ( [ObjC.citem|typename $id:t $id:result;|],
            [ObjC.cexp|$id:result|],
            [ObjC.cstm|assert($id:(arrayFree ops)(ctx, $id:result) == 0);|]
          )
        Just (TypeOpaque t ops) ->
          ( [ObjC.citem|typename $id:t $id:result;|],
            [ObjC.cexp|$id:result|],
            [ObjC.cstm|assert($id:(opaqueFree ops)(ctx, $id:result) == 0);|]
          )

-- | Return a statement printing the given external value.
printStm :: Manifest -> T.Text -> C.Exp -> C.Stm
printStm manifest tname e =
  case M.lookup tname $ manifestTypes manifest of
    Nothing ->
      let info = tname <> "_info"
       in [C.cstm|write_scalar(stdout, binary_output, &$id:info, &$exp:e);|]
    Just (TypeOpaque desc _) ->
      [C.cstm|printf("#<opaque %s>", $string:(T.unpack desc));|]
    Just (TypeArray _ et rank ops) ->
      let et' = uncurry primAPIType $ scalarToPrim et
          values_array = arrayValues ops
          shape_array = arrayShape ops
          num_elems =
            cproduct [[C.cexp|$id:shape_array(ctx, $exp:e)[$int:i]|] | i <- [0 .. rank -1]]
          info = et <> "_info"
       in [C.cstm|{
                 $ty:et' *arr = calloc($exp:num_elems, $id:info.size);
                 assert(arr != NULL);
                 assert($id:values_array(ctx, $exp:e, arr) == 0);
                 write_array(stdout, binary_output, &$id:info, arr,
                             $id:shape_array(ctx, $exp:e), $int:rank);
                 free(arr);
                 }|]

printResult :: Manifest -> [(T.Text, C.Exp)] -> [C.Stm]
printResult manifest = concatMap f
  where
    f (v, e) = [printStm manifest v e, [C.cstm|printf("\n");|]]

cliEntryPoint ::
  Manifest -> T.Text -> EntryPoint -> (C.Definition, C.Initializer)
cliEntryPoint manifest entry_point_name (EntryPoint cfun outputs inputs) =
  let (input_items, pack_input, free_input, free_parsed, input_args) =
        unzip5 $ readInputs manifest $ map inputType inputs

      (output_decls, output_vals, free_outputs) =
        unzip3 $ prepareOutputs manifest $ map outputType outputs

      printstms =
        printResult manifest $ zip (map outputType outputs) output_vals

      ctx_ty = [C.cty|struct futhark_context|]
      sync_ctx = "futhark_context_sync" :: T.Text
      error_ctx = "futhark_context_get_error" :: T.Text

      cli_entry_point_function_name = "futrts_cli_entry_" ++ T.unpack entry_point_name

      pause_profiling = "futhark_context_pause_profiling" :: T.Text
      unpause_profiling = "futhark_context_unpause_profiling" :: T.Text

      addrOf e = [C.cexp|&$exp:e|]

      run_it =
        [C.citems|
                int r;
                // Run the program once.
                $stms:pack_input
                if ($id:sync_ctx(ctx) != 0) {
                  futhark_panic(1, "%s", $id:error_ctx(ctx));
                };
                // Only profile last run.
                if (profile_run) {
                  $id:unpause_profiling(ctx);
                }
                t_start = get_wall_time();
                r = $id:cfun(ctx,
                             $args:(map addrOf output_vals),
                             $args:input_args);
                if (r != 0) {
                  futhark_panic(1, "%s", $id:error_ctx(ctx));
                }
                if ($id:sync_ctx(ctx) != 0) {
                  futhark_panic(1, "%s", $id:error_ctx(ctx));
                };
                if (profile_run) {
                  $id:pause_profiling(ctx);
                }
                t_end = get_wall_time();
                long int elapsed_usec = t_end - t_start;
                if (time_runs && runtime_file != NULL) {
                  fprintf(runtime_file, "%lld\n", (long long) elapsed_usec);
                  fflush(runtime_file);
                }
                $stms:free_input
              |]
   in ( [C.cedecl|
   static void $id:cli_entry_point_function_name($ty:ctx_ty *ctx) {
     typename int64_t t_start, t_end;
     int time_runs = 0, profile_run = 0;

     // We do not want to profile all the initialisation.
     $id:pause_profiling(ctx);

     // Declare and read input.
     set_binary_mode(stdin);
     $items:(mconcat input_items)

     if (end_of_input(stdin) != 0) {
       futhark_panic(1, "Expected EOF on stdin after reading input for \"%s\".\n", $string:(pretty entry_point_name));
     }

     $items:output_decls

     // Warmup run
     if (perform_warmup) {
       $items:run_it
       $stms:free_outputs
     }
     time_runs = 1;
     // Proper run.
     for (int run = 0; run < num_runs; run++) {
       // Only profile last run.
       profile_run = run == num_runs -1;
       $items:run_it
       if (run < num_runs-1) {
         $stms:free_outputs
       }
     }

     // Free the parsed input.
     $stms:free_parsed

     if (print_result) {
       // Print the final result.
       if (binary_output) {
         set_binary_mode(stdout);
       }
       $stms:printstms
     }

     $stms:free_outputs
   }|],
        [C.cinit|{ .name = $string:(T.unpack entry_point_name),
                       .fun = $id:cli_entry_point_function_name }|]
      )

{-# NOINLINE cliDefs #-}

-- | Generate Futhark standalone executable code.
cliDefs :: [Option] -> Manifest -> T.Text
cliDefs options manifest =
  let option_parser =
        generateOptionParser "parse_options" $ genericOptions ++ options
      (cli_entry_point_decls, entry_point_inits) =
        unzip $
          map (uncurry (cliEntryPoint manifest)) $
            M.toList $ manifestEntryPoints manifest
   in prettyText
        [ObjC.cunit|
    $esc:("#include <Foundation/Foundation.h>")
    $esc:("#include <Metal/Metal.h>")
    $esc:("#include "MetalProgram.h")
    $esc:(T.unpack valuesH)

    //$esc:(T.unpack MetalC) //write Metal C Functions here

    int main(int argc, const char * argv[]) {
        @autoreleasepool {
        
        id<MTLDevice> device = MTLCreateSystemDefaultDevice(); //Modify to allow contextual gpu calls

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


|]

createCommandBuffer :: M.Map Name SizeClass -> GC.CompilerM.Metal
createCommandQueue  copyCommandQueue = do
  let newCommandQueue = map (\c -> [ObjC.cinit|$string: (_mDevice newCommandQueue)|]) $ M.keys sizes
      gridSize = map (\c -> [ObjC.cinit|$string:(MTLSizeMake (arrayLength, 1, 1)|]) $ M.Keys sizes;
      --MTLSize
