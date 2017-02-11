{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Multiversion segmented reduction.
module Futhark.Pass.ExtractKernels.Segmented
       ( regularSegmentedRedomapAsScan
       , regularSegmentedRedomap
       , regularSegmentedScan
       )
       where

import Control.Monad
import qualified Data.HashMap.Lazy as HM
import Data.Monoid

import Prelude

import Futhark.Transform.Rename
import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Representation.Kernels
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass.ExtractKernels.BlockedKernel

data SegmentedVersion = OneGroupOneSegment
                      | ManyGroupsOneSegment
                      deriving (Eq, Ord, Show)

regularSegmentedRedomap :: (HasScope Kernels m, MonadBinder m, Lore m ~ Kernels) =>
                           SubExp            -- segment_size
                        -> SubExp            -- num_segments
                        -> [SubExp]          -- nest_sizes = the sizes of the maps on "top" of this redomap
                        -> Pattern Kernels   -- flat_pat ... pat where each type is array with dim [w]
                        -> Pattern Kernels   -- pat
                        -> Certificates      -- cs
                        -> SubExp            -- w = total_num_elements
                        -> Commutativity     -- comm
                        -> Lambda InKernel   -- reduce_lam
                        -> Lambda InKernel   -- fold_lam = this lambda performs both the map-part and
                                             -- reduce-part of a redomap (described in redomap paper)
                        -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this redomap
                        -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
                        -> [SubExp]          -- nes
                        -> [VName]           -- arrs_flat
                        -> m ()
regularSegmentedRedomap segment_size num_segments _nest_sizes flat_pat
                        pat cs w comm reduce_lam fold_lam ispace inps nes arrs_flat = do
  unless (null $ patternContextElements pat) $ fail "regularSegmentedRedomap result pattern contains context elements, and Rasmus did not think this would ever happen."

  -- the result of the "map" part of a redomap has to be stored somewhere within
  -- the chunking loop of a kernel. The current way to do this is to make some
  -- scratch space initially, and each thread will get a part of this by
  -- splitting it. Finally it is returned as a result of the kernel (to not
  -- break functional semantics).
  map_out_arrs <- forM (drop num_redres $ patternIdents pat) $ \(Ident name t) -> do
    tmp <- letExp (baseString name <> "_out_in") $
           BasicOp $ Scratch (elemType t) (arrayDims t)
    -- This reshape will not always work.
    -- For example if the "map" part takes an input a 1D array and produces a 2D
    -- array, this is clearly wrong. See ex3.fut
    letExp (baseString name ++ "_out_in") $
            BasicOp $ Reshape cs [DimNew w] tmp

  -- Check that we're only dealing with arrays with dimension [w]
  forM_ arrs_flat $ \arr -> do
    tp <- lookupType arr
    case tp of
      -- FIXME: this won't work if the reduction operator works on lists... but
      -- they seem to be handled in some other way (which makes sense). Talk
      -- with troels if I should worry about this.
      Array _primtp (Shape [flatsize]) _uniqness ->
        when (flatsize /= w) $
          fail$ "regularSegmentedRedomap: flat array, with incorrect size encountered " ++ pretty arr
      _ -> fail $ "regularSegmentedRedomap: non-flat array encountered " ++ pretty arr

  -- The pattern passed to chunkLambda must have exactly *one* array dimension,
  -- to get the correct size of [chunk_size]type.
  --
  -- FIXME: not sure if this will work when result of map is multidimensional,
  -- or if reduction operator uses lists... must check
  chunk_pat <- fmap (Pattern []) $ forM (patternValueElements pat) $ \pat_e ->
    case patElemType pat_e of
      (Array ty (Shape (dim0:_)) u) -> do
          vn' <- newName $ patElemName pat_e
          return $ PatElem vn' BindVar $ Array ty (Shape [dim0]) u
      _ -> fail $ "segmentedRedomap: result pattern is not array " ++ pretty pat_e

  chunk_fold_lam <- chunkLambda chunk_pat nes fold_lam

  -- kernliseLambda intializes the value of the merge pattern for the reduction
  -- to the neutral element.
  kern_chunk_fold_lam <- kerneliseLambda nes chunk_fold_lam

  let chunk_red_pat = Pattern [] $ take num_redres $ patternValueElements chunk_pat
  kern_chunk_reduce_lam <- kerneliseLambda nes =<< chunkLambda chunk_red_pat nes reduce_lam

  -- the lambda for a GroupReduce needs these two extra parameters
  my_index <- newVName "my_index"
  other_offset <- newVName "other_offset"
  let my_index_param = Param my_index (Prim int32)
  let other_offset_param = Param other_offset (Prim int32)
  let reduce_lam' = reduce_lam { lambdaParams = my_index_param :
                                                other_offset_param :
                                                lambdaParams reduce_lam
                               }
  flag_reduce_lam <- addFlagToLambda nes reduce_lam
  let flag_reduce_lam' = flag_reduce_lam { lambdaParams = my_index_param :
                                                          other_offset_param :
                                                          lambdaParams flag_reduce_lam
                                         }


  -- FIXME: do we need to copy arrays here? :S
  -- see 'blockedReductionStream' in BlockedKernel.hs

  let all_arrs = arrs_flat ++ map_out_arrs
  (ogps_ses, ogps_stms) <- runBinder $ oneGroupPerSeg all_arrs reduce_lam' kern_chunk_fold_lam
  (mgps_ses, mgps_stms) <- runBinder $ manyGroupPerSeg all_arrs reduce_lam' kern_chunk_fold_lam kern_chunk_reduce_lam

  (ogms_ses, ogms_stms) <- runBinder $ oneGroupManySeg map_out_arrs flag_reduce_lam'

  -- TODO: should unify this calculation somewhere, with the one taking place in
  -- 'groupPerSegmentKernel'
  num_groups_hint <- letSubExp "num_groups_hint" $ Op NumGroups
  num_groups_per_segment <- letSubExp "num_groups_per_segment" =<<
    eDivRoundingUp Int32 (eSubExp num_groups_hint) (eSubExp num_segments)

  group_size <- letSubExp "group_size" $ Op GroupSize
  num_segments_per_group <- letSubExp "num_segments_per_group" $
    BasicOp $ BinOp (UDiv Int32) group_size segment_size

  -- Instead of making this @if@ here, we could run "multiple groups per
  -- segment" first, and if the number of groups per segment was 1, we could
  -- simply return the result immediately without running the second kernel. I
  -- chose not to do this, as the oneGroupPerSegment kernel will contain
  -- slightly fewer instructions -- and hopefully be a bit faster (at the cost
  -- of large code size). TODO: test how much we win by doing this.


  let e_no_small_seg = eIf (eCmpOp (CmpEq $ IntType Int32) (eSubExp num_groups_per_segment)
                                                           (eSubExp one))
                        (mkBodyM ogps_stms ogps_ses)
                        (mkBodyM mgps_stms mgps_ses)

  e <- eIf (eCmpOp (CmpSlt Int32) (eSubExp one) (eSubExp num_segments_per_group))
         (mkBodyM ogms_stms ogms_ses)
         (eBody [e_no_small_seg])

  redres_pes <- forM (take num_redres (patternValueElements pat)) $ \pe -> do
    vn' <- newName $ patElemName pe
    return $ PatElem vn' BindVar $ patElemType pe `setArrayDims` [num_segments]
  let mapres_pes = drop num_redres $ patternValueElements flat_pat
  let unreshaped_pat = Pattern [] $ redres_pes ++ mapres_pes

  addStm $ Let unreshaped_pat () e

  forM_ (zip (patternValueElements unreshaped_pat)
             (patternValueElements pat)) $ \(kpe, pe) ->
    addStm $ Let (Pattern [] [pe]) () $
             BasicOp $ Reshape cs [DimNew se | se <- arrayDims $ patElemAttr pe]
                               (patElemName kpe)

  where
    one = constant (1 :: Int32)

    -- number of reduction results (tuple size for reduction operator)
    num_redres = length nes

    ----------------------------------------------------------------------------
    -- The functions below generate all the needed code for the two different
    -- version of segmented-redomap (one group per segment, and many groups per
    -- segment).
    --
    -- We rename statements before adding them because the same lambdas
    -- (reduce/fold) are used multiple times, and we do not want to bind the
    -- same VName twice (as this is a type error)
    ----------------------------------------------------------------------------
    oneGroupPerSeg all_arrs reduce_lam' kern_chunk_fold_lam = do
      mapres_pes <- forM (drop num_redres $ patternValueElements flat_pat) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' BindVar $ patElemType pe

      (kernel, _, _) <- groupPerSegmentKernel segment_size num_segments cs
        all_arrs comm reduce_lam' kern_chunk_fold_lam
        nes w OneGroupOneSegment
        ispace inps

      kernel_redres_pes <- forM (take num_redres (patternValueElements pat)) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' BindVar $ patElemType pe `setArrayDims` [num_segments]

      let kernel_pat = Pattern [] $ kernel_redres_pes ++ mapres_pes

      addStm =<< renameStm (Let kernel_pat () $ Op kernel)
      return $ map (Var . patElemName) $ patternValueElements kernel_pat

    ----------------------------------------------------------------------------
    manyGroupPerSeg all_arrs reduce_lam' kern_chunk_fold_lam kern_chunk_reduce_lam = do
      mapres_pes <- forM (drop num_redres $ patternValueElements flat_pat) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' BindVar $ patElemType pe

      (firstkernel, num_groups_used, num_groups_per_segment) <- groupPerSegmentKernel segment_size num_segments cs
        all_arrs comm reduce_lam' kern_chunk_fold_lam
        nes w ManyGroupsOneSegment
        ispace inps

      firstkernel_redres_pes <- forM (take num_redres (patternValueElements pat)) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' BindVar $ patElemType pe `setArrayDims` [num_groups_used]

      let first_pat = Pattern [] $ firstkernel_redres_pes ++ mapres_pes
      addStm =<< renameStm (Let first_pat () $ Op firstkernel)

      (secondkernel, _, _) <- groupPerSegmentKernel num_groups_per_segment num_segments cs
        (map patElemName firstkernel_redres_pes) comm reduce_lam' kern_chunk_reduce_lam
        nes num_groups_used OneGroupOneSegment
        ispace inps

      second_redres_pes <- forM (take num_redres (patternValueElements pat)) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' BindVar $ patElemType pe `setArrayDims` [num_segments]

      let second_pat = Pattern [] second_redres_pes
      addStm =<< renameStm (Let second_pat () $ Op secondkernel)

      let result_pes = second_redres_pes ++ mapres_pes
      return $ map (Var . patElemName) result_pes

    ----------------------------------------------------------------------------
    oneGroupManySeg map_out_arrs flag_reduce_lam' = do
      red_scratch_arrs <- forM (take num_redres $ patternIdents pat) $ \(Ident name t) -> do
        tmp <- letExp (baseString name <> "_redres_scratch") $
               BasicOp $ Scratch (elemType t) (arrayDims t)
        letExp (baseString name ++ "_redres_scratch") $
                BasicOp $ Reshape cs [DimNew num_segments] tmp

      let scratch_arrays = red_scratch_arrs ++ map_out_arrs

      e <- oneGroupManySegmentKernel segment_size num_segments cs
                          arrs_flat scratch_arrays
                          comm flag_reduce_lam' fold_lam
                          nes w ispace inps
      letTupExp' "kernel_result" $ Op e

groupPerSegmentKernel :: (MonadBinder m, Lore m ~ Kernels) =>
          SubExp            -- segment_size
       -> SubExp            -- num_segments
       -> Certificates      -- cs
       -> [VName]           -- all_arrs: flat arrays (also the "map_out" ones)
       -> Commutativity     -- comm
       -> Lambda InKernel   -- reduce_lam
       -> Lambda InKernel   -- kern_chunk_fold_lam
       -> [SubExp]          -- nes
       -> SubExp            -- w = total_num_elements
       -> SegmentedVersion  -- segver
       -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this redomap
       -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
       -> m (Kernel InKernel, SubExp, SubExp)
groupPerSegmentKernel segment_size num_segments cs all_arrs comm
                      reduce_lam' kern_chunk_fold_lam
                      nes w segver ispace inps = do
  let num_redres = length nes -- number of reduction results (tuple size for
                              -- reduction operator)
  kernel_input_stms <- forM inps $ \kin -> do
    let pe = PatElem (kernelInputName kin) BindVar (kernelInputType kin)
    let arr = kernelInputArray kin
    arrtp <- lookupType arr
    let slice = fullSlice arrtp [DimFix se | se <- kernelInputIndices kin]
    return $ Let (Pattern [] [pe]) () $ BasicOp $ Index cs arr slice

  group_size <- letSubExp "group_size" $ Op GroupSize
  num_groups_hint <- letSubExp "num_groups_hint" $ Op NumGroups

  num_groups_per_segment <-
    letSubExp "num_groups_per_segment" =<<
    case segver of
      OneGroupOneSegment -> eSubExp one
      ManyGroupsOneSegment -> eDivRoundingUp Int32 (eSubExp num_groups_hint)
                                                   (eSubExp num_segments)
  num_groups <- letSubExp "num_groups" $
    case segver of
      OneGroupOneSegment -> BasicOp $ SubExp num_segments
      ManyGroupsOneSegment -> BasicOp $ BinOp (Mul Int32) num_segments num_groups_per_segment

  num_threads <- letSubExp "num_threads" $
    BasicOp $ BinOp (Mul Int32) num_groups group_size

  elements_per_thread <-
    letSubExp "elements_per_thread" =<<
    eDivRoundingUp Int32 (eSubExp segment_size)
                         (eBinOp (Mul Int32) (eSubExp group_size)
                                             (eSubExp num_groups_per_segment))

  threads_within_segment <- letSubExp "threads_within_segment" $
    BasicOp $ BinOp (Mul Int32) group_size num_groups_per_segment

  gtid_vn <- newVName "gtid"

  -- the array passed here is the structure for how to layout the kernel space
  space <- newKernelSpace (num_groups, group_size, num_threads) $
    FlatGroupSpace $ ispace ++ [(gtid_vn, num_groups_per_segment)]

  ((segment_index, index_within_segment), calc_segindex_stms) <- runBinder $ do
    segment_index <- letSubExp "segment_index" $
      BasicOp $ BinOp (SDiv Int32) (Var $ spaceGroupId space) num_groups_per_segment

    -- localId + (group_size * (groupId % num_groups_per_segment))
    index_within_segment <- letSubExp "index_within_segment" =<<
      eBinOp (Add Int32)
          (eSubExp $ Var $ spaceLocalId space)
          (eBinOp (Mul Int32)
             (eSubExp group_size)
             (eBinOp (SMod Int32) (eSubExp $ Var $ spaceGroupId space) (eSubExp num_groups_per_segment))
          )
    return (segment_index, index_within_segment)

  let ordering = case comm of Commutative -> SplitStrided threads_within_segment
                              Noncommutative -> SplitContiguous

  let (_, chunksize, [], arr_params) =
        partitionChunkedKernelFoldParameters 0 $ lambdaParams kern_chunk_fold_lam
  let chunksize_se = Var $ paramName chunksize

  patelems_res_of_split <- forM arr_params $ \arr_param -> do
    let chunk_t = paramType arr_param `setOuterSize` Var (paramName chunksize)
    return $ PatElem (paramName arr_param) BindVar chunk_t

  let chunksize_stm =
        Let (Pattern [] [PatElem (paramName chunksize) BindVar $ paramType chunksize])
            () $
            Op $ SplitSpace ordering segment_size index_within_segment elements_per_thread

  let stride = case ordering of SplitStrided s -> s
                                SplitContiguous -> one

  (offset, offset_stms) <- runBinder $
    makeOffsetExp ordering index_within_segment elements_per_thread segment_index

  index_stms <- forM (zip all_arrs patelems_res_of_split) $ \(arr, pe) -> do
    tp <- lookupType arr
    let slice = fullSlice tp [DimSlice offset chunksize_se stride]
    return $ Let (Pattern [] [pe]) () $ BasicOp $ Index cs arr slice

  let red_ts = take num_redres $ lambdaReturnType kern_chunk_fold_lam
  let map_ts = map rowType $ drop num_redres $ lambdaReturnType kern_chunk_fold_lam
  let kernel_return_types = red_ts ++ map_ts

  red_pes <- forM red_ts $ \red_t -> do
    pe_name <- newVName "chunk_fold_red"
    return $ PatElem pe_name BindVar red_t
  map_pes <- forM map_ts $ \map_t -> do
    pe_name <- newVName "chunk_fold_map"
    return $ PatElem pe_name BindVar $ map_t `arrayOfRow` chunksize_se

  -- we add the lets here, as we practially don't know if the resulting subexp
  -- is a Constant or a Var, so better be safe (?)
  let fold_chunk_stms = bodyStms (lambdaBody kern_chunk_fold_lam) ++
        [ Let (Pattern [] [pe]) () $ BasicOp $ SubExp se
          | (pe,se) <- zip (red_pes ++ map_pes) (bodyResult $ lambdaBody kern_chunk_fold_lam) ]

  -- Combine the reduction results from each thread. This will put results in
  -- local memory, so a GroupReduce can be performed on them
  combine_red_pes <- forM red_ts $ \red_t -> do
    pe_name <- newVName "chunk_fold_red"
    return $ PatElem pe_name BindVar $ red_t `arrayOfRow` group_size
  let combine_stms = [ Let (Pattern [] [pe']) () $ Op $
                       Combine [(spaceLocalId space, group_size)] [patElemType pe]
                       (constant True) $
                       Body () [] [Var $ patElemName pe]
                     | (pe', pe) <- zip combine_red_pes red_pes ]

  final_red_pes <- forM (lambdaReturnType reduce_lam') $ \t -> do
    pe_name <- newVName "final_result"
    return $ PatElem pe_name BindVar t
  let group_reduce_stm = Let (Pattern [] final_red_pes) () $ Op $
                         GroupReduce group_size reduce_lam' $
                         zip nes $ map patElemName combine_red_pes

  red_returns <- forM final_red_pes $ \pe ->
    return $ ThreadsReturn (OneThreadPerGroup (constant (0::Int32))) $
                           Var $ patElemName pe
  map_returns <- forM map_pes $ \pe ->
    return $ ConcatReturns ordering w elements_per_thread
                           (Just offset) $
                           patElemName pe
  let kernel_returns = red_returns ++ map_returns

  let kerneldebughints = KernelDebugHints kernelname
                         [ ("num_segment", num_segments)
                         , ("segment_size", segment_size)
                         ]

  let kernel = Kernel kerneldebughints cs space kernel_return_types $
                  KernelBody () (kernel_input_stms ++ calc_segindex_stms ++
                                 [chunksize_stm] ++ offset_stms ++ index_stms ++
                                 fold_chunk_stms ++ combine_stms ++ [group_reduce_stm])
                  kernel_returns

  return (kernel, num_groups, num_groups_per_segment)

  where
    one = constant (1 :: Int32)

    kernelname = case segver of
      OneGroupOneSegment -> "segmented_redomap__one_group_one_segment"
      ManyGroupsOneSegment -> "segmented_redomap__many_groups_one_segment"

    makeOffsetExp SplitContiguous index_within_segment elements_per_thread segment_index = do
      e <- eBinOp (Add Int32)
             (eBinOp (Mul Int32) (eSubExp elements_per_thread) (eSubExp index_within_segment))
             (eBinOp (Mul Int32) (eSubExp segment_size) (eSubExp segment_index))
      letSubExp "offset" e
    makeOffsetExp (SplitStrided _) index_within_segment _elements_per_thread segment_index = do
      e <- eBinOp (Add Int32) (eSubExp index_within_segment)
             (eBinOp (Mul Int32) (eSubExp segment_size) (eSubExp segment_index))
      letSubExp "offset" e

oneGroupManySegmentKernel :: (MonadBinder m, Lore m ~ Kernels) =>
          SubExp            -- segment_size
       -> SubExp            -- num_segments
       -> Certificates      -- cs
       -> [VName]           -- redin_arrs: flat arrays (containing input to fold_lam)
       -> [VName]           -- scratch_arrs: Preallocated space that we can write into
       -> Commutativity     -- comm
       -> Lambda InKernel   -- flag_reduce_lam'
       -> Lambda InKernel   -- fold_lam
       -> [SubExp]          -- nes
       -> SubExp            -- w = total_num_elements
       -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this redomap
       -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
       -> m (Kernel InKernel)
oneGroupManySegmentKernel segment_size num_segments cs redin_arrs scratch_arrs
                      _comm flag_reduce_lam' fold_lam_unrenamed
                      nes w ispace inps = do
  let num_redres = length nes -- number of reduction results (tuple size for
                              -- reduction operator)

  fold_lam <- renameLambda fold_lam_unrenamed

  group_size <- letSubExp "group_size" $ Op GroupSize

  num_segments_per_group <- letSubExp "num_segments_per_group" $
    BasicOp $ BinOp (UDiv Int32) group_size segment_size

  num_groups <- letSubExp "num_groups" =<<
    eDivRoundingUp Int32 (eSubExp num_segments) (eSubExp num_segments_per_group)

  num_threads <- letSubExp "num_threads" $
    BasicOp $ BinOp (Mul Int32) num_groups group_size

  active_threads_per_group <- letSubExp "active_threads_per_group" $
    BasicOp $ BinOp (Mul Int32) segment_size num_segments_per_group

  let remainder_last_group = eBinOp (UMod Int32) (eSubExp num_segments) (eSubExp num_segments_per_group)

  segments_in_last_group <- letSubExp "seg_in_last_group" =<<
    eIf (eCmpOp (CmpEq $ IntType Int32) remainder_last_group
                                        (eSubExp zero))
        (eBody [eSubExp num_segments_per_group])
        (eBody [remainder_last_group])

  active_threads_in_last_group <- letSubExp "active_threads_last_group" $
    BasicOp $ BinOp (Mul Int32) segment_size segments_in_last_group

  total_wanted_threads <- letSubExp "totalwantedthreads" =<<
    eBinOp (Add Int32)
        (eBinOp (Mul Int32)
            (eBinOp (Sub Int32) (eSubExp num_groups) (eSubExp one))
            (eSubExp group_size))
        (eSubExp active_threads_in_last_group)

  gtid_vn <- newVName "gtid"

  -- FIXME: Can't handle map-invariant variables right now, eg. ex4.fut

  -- the array passed here is the structure for how to layout the kernel space
  space <- newKernelSpace (num_groups, group_size, num_threads) $
    FlatThreadSpace []

  ------------------------------------------------------------------------------
  -- What follows is the statements used in the kernel
  ------------------------------------------------------------------------------

  let lid = Var $ spaceLocalId space

  ((segment_index, index_within_segment), calc_segindex_stms) <- runBinder $ do
    segment_index <- letSubExp "segment_index" =<<
      eBinOp (Add Int32)
        (eBinOp (UDiv Int32) (eSubExp $ Var $ spaceLocalId space) (eSubExp segment_size))
        (eBinOp (Mul Int32) (eSubExp $ Var $ spaceGroupId space) (eSubExp num_segments_per_group))

    index_within_segment <- letSubExp "index_within_segment" =<<
      eBinOp (UMod Int32) (eSubExp $ Var $ spaceLocalId space) (eSubExp segment_size)

    return (segment_index, index_within_segment)

  (offset, offset_stms) <- runBinder $ makeOffsetExp index_within_segment segment_index


  let (red_ts, map_ts) = splitAt num_redres $ lambdaReturnType fold_lam
  let kernel_return_types = red_ts ++ map_ts

  let wasted_thread = do
        unless (all primType kernel_return_types) $ fail "TODO: segreodmap, when using GroupScan, cannot handle non-PrimType return types for fold_lam"
        let dummy_vals = map (Constant . blankPrimValue . elemType) kernel_return_types
        return (negone : negone : dummy_vals)

  let normal_thread = do
        let red_ts' = Prim Bool : red_ts

        red_pes <- forM red_ts $ \red_t -> do
          pe_name <- newVName "fold_red"
          return $ PatElem pe_name BindVar red_t
        map_pes <- forM map_ts $ \map_t -> do
          pe_name <- newVName "fold_map"
          return $ PatElem pe_name BindVar map_t

        -- kernel_input_stms <- forM inps $ \kin -> do
        --   let pe = PatElem (kernelInputName kin) BindVar (kernelInputType kin)
        --   let arr = kernelInputArray kin
        --   arrtp <- lookupType arr
        --   let slice = fullSlice arrtp [DimFix se | se <- kernelInputIndices kin]
        --   return $ Let (Pattern [] [pe]) () $ BasicOp $ Index cs arr slice

        -- Index input array to get arguments to fold_lam
        let arr_params = drop num_redres $ lambdaParams fold_lam
        let nonred_lamparam_pes = map
              (\p -> PatElem (paramName p) BindVar (paramType p)) arr_params
        forM_ (zip redin_arrs nonred_lamparam_pes) $ \(arr, pe) -> do
          tp <- lookupType arr
          let slice = fullSlice tp [DimFix offset]
          addStm $ Let (Pattern [] [pe]) () $ BasicOp $ Index cs arr slice

        -- Bind neutral element (serves as the reduction arguments to fold_lam)
        forM_ (zip nes (take num_redres $ lambdaParams fold_lam)) $ \(ne,param) -> do
          let pe = PatElem (paramName param) BindVar (paramType param)
          addStm $ Let (Pattern [] [pe]) () $ BasicOp $ SubExp ne

        mapM_ addStm $ bodyStms $ lambdaBody fold_lam

        -- we add the lets here, as we practially don't know if the resulting subexp
        -- is a Constant or a Var, so better be safe (?)
        mapM_ addStm [ Let (Pattern [] [pe]) () $ BasicOp $ SubExp se
                    | (pe,se) <- zip (red_pes ++ map_pes) (bodyResult $ lambdaBody fold_lam) ]

        isfirstinsegment <- letExp "isfirstinsegment" =<<
          eCmpOp (CmpEq $ IntType Int32)
            (eBinOp (UMod Int32) (eSubExp lid) (eSubExp segment_size))
            (eSubExp zero)

        -- We will perform a segmented-scan, so all the prime variables here
        -- include the flag, which is the first argument to flag_reduce_lam
        let red_pes' = PatElem isfirstinsegment BindVar (Prim Bool) : red_pes

        -- Combine the reduction results from each thread. This will put results in
        -- local memory, so a GroupReduce/GroupScan can be performed on them
        combine_red_pes' <- forM red_ts' $ \red_t -> do
          pe_name <- newVName "chunk_fold_red"
          return $ PatElem pe_name BindVar $ red_t `arrayOfRow` group_size
        mapM_ addStm [ Let (Pattern [] [pe']) () $ Op $
                             Combine [(spaceLocalId space, group_size)] [patElemType pe]
                             (constant True) $
                             Body () [] [Var $ patElemName pe]
                           | (pe', pe) <- zip combine_red_pes' red_pes' ]

        scan_red_pes' <- forM red_ts' $ \red_t -> do
          pe_name <- newVName "scanned"
          return $ PatElem pe_name BindVar $ red_t `arrayOfRow` group_size
        let scan_red_pes = drop 1 scan_red_pes'
        addStm $ Let (Pattern [] scan_red_pes') () $ Op $
          GroupScan group_size flag_reduce_lam' $
          zip (false:nes) (map patElemName combine_red_pes')

        islastinsegment <- letExp "islastinseg" =<< eCmpOp (CmpEq $ IntType Int32)
            (eBinOp (UMod Int32) (eSubExp lid) (eSubExp segment_size))
            (eBinOp (Sub Int32) (eSubExp segment_size) (eSubExp one))

        redoffset <- letSubExp "redoffset" =<<
            eIf (eSubExp $ Var islastinsegment)
              (eBody [eSubExp segment_index])
              (mkBodyM [] [negone])
        let mapoffset = offset

        redret_elems <- fmap (map Var) $ letTupExp "red_return_elem" =<<
          eIf (eSubExp $ Var islastinsegment)
            (eBody [return $ BasicOp $ Index [] (patElemName pe) (fullSlice (patElemType pe) [DimFix lid])
                   | pe <- scan_red_pes])
            (mkBodyM [] nes)

        let mapret_elems = map (Var . patElemName) map_pes

        return (redoffset : mapoffset : redret_elems++mapret_elems)


  let picknchoose = do
        is_last_group <- letSubExp "islastgroup" =<<
            eCmpOp (CmpEq $ IntType Int32)
                (eSubExp $ Var $ spaceGroupId space)
                (eBinOp (Sub Int32) (eSubExp num_groups) (eSubExp one))

        active_threads_this_group <- letSubExp "active_thread_this_group" =<<
            eIf (eSubExp is_last_group)
               (eBody [eSubExp active_threads_in_last_group])
               (eBody [eSubExp active_threads_per_group])

        isactive <- letSubExp "isactive" =<<
          eCmpOp (CmpSlt Int32) (eSubExp lid) (eSubExp active_threads_this_group)

        (normal_res, normal_stms) <- runBinder normal_thread
        (wasted_res, wasted_stms) <- runBinder wasted_thread

        -- we could just have used letTupExp, but this would not give as nice
        -- names in the generated code
        redoffset_pe <- (\vn -> PatElem vn BindVar i32) <$> newVName "redoffset"
        mapoffset_pe <- (\vn -> PatElem vn BindVar i32) <$> newVName "mapoffset"
        red_pes <- forM red_ts $ \red_t -> do
          pe_name <- newVName "red_res"
          return $ PatElem pe_name BindVar red_t
        map_pes <- forM map_ts $ \map_t -> do
          pe_name <- newVName "map_res"
          return $ PatElem pe_name BindVar map_t
        let all_res_pes = redoffset_pe:mapoffset_pe:red_pes++map_pes

        e <- eIf (eSubExp isactive)
            (mkBodyM normal_stms normal_res)
            (mkBodyM wasted_stms wasted_res)
        addStm $ Let (Pattern [] all_res_pes) () e

        return $ map (Var . patElemName) all_res_pes

  (redoffset:mapoffset:redmapres, stms) <- runBinder $
    localScope (scopeOf $ offset_stms ++ calc_segindex_stms) picknchoose
  let (finalredvals, finalmapvals) = splitAt num_redres redmapres

  -- To be able to only return elements from some threads, we exploit the fact
  -- that WriteReturn with offset=-1, won't do anything.
  red_returns <- forM (zip finalredvals $ take num_redres scratch_arrs) $ \(se, scarr) ->
    return $ WriteReturn num_segments scarr redoffset se
  map_returns <- forM (zip finalmapvals $ drop num_redres scratch_arrs) $ \(se, scarr) ->
    return $ WriteReturn w scarr mapoffset se
  let kernel_returns = red_returns ++ map_returns

  let kerneldebughints = KernelDebugHints kernelname
                         [ ("num_segment", num_segments)
                         , ("segment_size", segment_size)
                         , ("num_groups", num_groups)
                         , ("group_size", group_size)
                         , ("num_segments_per_group", num_segments_per_group)
                         , ("active_threads_per_group", active_threads_per_group)
                         , ("segments_in_last_group", segments_in_last_group)
                         , ("active_threads_in_last_group", active_threads_in_last_group)
                         ]

  let kernel = Kernel kerneldebughints cs space kernel_return_types $
                  KernelBody () (--FIXME: add these back: kernel_input_stms ++
                                 calc_segindex_stms ++
                                 offset_stms ++
                                 stms)
                  kernel_returns

  return kernel

  where
    i32 = Prim $ IntType Int32
    zero = constant (0 :: Int32)
    one = constant (1 :: Int32)
    negone = constant (-1 :: Int32)
    false = constant False

    kernelname = "segmented_redomap__one_group_many_segment"

    makeOffsetExp index_within_segment segment_index = do
      e <- eBinOp (Add Int32)
             (eSubExp index_within_segment)
             (eBinOp (Mul Int32) (eSubExp segment_size) (eSubExp segment_index))
      letSubExp "offset" e

regularSegmentedRedomapAsScan :: (HasScope Kernels m, MonadBinder m, Lore m ~ Kernels) =>
                                SubExp
                             -> SubExp
                             -> [SubExp]
                             -> Pattern Kernels
                             -> Pattern Kernels
                             -> Certificates
                             -> SubExp
                             -> Commutativity
                             -> Lambda InKernel
                             -> Lambda InKernel
                             -> [(VName, SubExp)]
                             -> [KernelInput]
                             -> [SubExp] -> [VName]
                             -> m ()
regularSegmentedRedomapAsScan segment_size num_segments nest_sizes flat_pat
                              pat cs w _comm lam fold_lam ispace inps nes arrs = do
  regularSegmentedScan segment_size flat_pat cs w lam fold_lam ispace inps nes arrs

  let (acc_arrs, map_arrs) = splitAt (length nes) $ patternValueIdents flat_pat
      (acc_pes, map_pes) = splitAt (length nes) $ patternValueElements pat
      acc_ts = lambdaReturnType lam
      acc_pat = Pattern [] acc_pes

  is <- replicateM (length nest_sizes) $ newVName "i"

  body <- runBodyBinder $ localScope (HM.fromList $ zip is $ repeat $ IndexInfo Int32) $ do
    let segment_id = flattenIndex
                     (map SE.intSubExpToScalExp nest_sizes)
                     (map (SE.intSubExpToScalExp . Var) is)
        offset = (segment_id + 1) * SE.intSubExpToScalExp segment_size - 1
    j <- letSubExp "j" =<< SE.fromScalExp offset
    vals <- forM acc_arrs $ \arr ->
      letSubExp "v" $ BasicOp $ Index [] (identName arr) $
      fullSlice (identType arr) [DimFix j]
    return $ resultBody vals

  (mapk_bnds, mapk) <-
    mapKernelFromBody [] num_segments (FlatThreadSpace $ zip is nest_sizes) [] acc_ts body
  mapM_ addStm mapk_bnds
  letBind_ acc_pat $ Op mapk

  forM_ (zip map_pes map_arrs) $ \(pe,arr) ->
    letBind_ (Pattern [] [pe]) $
    BasicOp $ Reshape [] (map DimNew $ arrayDims $ typeOf pe) $ identName arr

addFlagToLambda :: (MonadBinder m, Lore m ~ Kernels) =>
                   [SubExp] -> Lambda InKernel -> m (Lambda InKernel)
addFlagToLambda nes lam = do
  let num_accs = length nes
  x_flag <- newVName "x_flag"
  y_flag <- newVName "y_flag"
  let x_flag_param = Param x_flag $ Prim Bool
      y_flag_param = Param y_flag $ Prim Bool
      (x_params, y_params) = splitAt num_accs $ lambdaParams lam
      params = [x_flag_param] ++ x_params ++ [y_flag_param] ++ y_params

  body <- runBodyBinder $ localScope (scopeOfLParams params) $ do
    new_flag <- letSubExp "new_flag" $
                BasicOp $ BinOp LogOr (Var x_flag) (Var y_flag)
    lhs <- fmap (map Var) $ letTupExp "seg_lhs" $ If (Var y_flag)
      (resultBody nes)
      (resultBody $ map (Var . paramName) x_params)
      (staticShapes $ map paramType x_params)
    let rhs = map (Var . paramName) y_params

    lam' <- renameLambda lam -- avoid shadowing
    res <- eLambda lam' $ lhs ++ rhs

    return $ resultBody $ new_flag : res

  return Lambda { lambdaParams = params
                , lambdaBody = body
                , lambdaReturnType = Prim Bool : lambdaReturnType lam
                }

regularSegmentedScan :: (MonadBinder m, Lore m ~ Kernels) =>
                        SubExp
                     -> Pattern Kernels
                     -> Certificates
                     -> SubExp
                     -> Lambda InKernel
                     -> Lambda InKernel
                     -> [(VName, SubExp)] -> [KernelInput]
                     -> [SubExp] -> [VName]
                     -> m ()
regularSegmentedScan segment_size pat cs w lam fold_lam ispace inps nes arrs = do
  flags_i <- newVName "flags_i"

  unused_flag_array <- newVName "unused_flag_array"
  flags_body <-
    runBodyBinder $ localScope (HM.singleton flags_i $ IndexInfo Int32) $ do
      segment_index <- letSubExp "segment_index" $
                       BasicOp $ BinOp (SRem Int32) (Var flags_i) segment_size
      start_of_segment <- letSubExp "start_of_segment" $
                          BasicOp $ CmpOp (CmpEq int32) segment_index zero
      let flag = start_of_segment
      return $ resultBody [flag]
  (mapk_bnds, mapk) <- mapKernelFromBody [] w (FlatThreadSpace [(flags_i, w)]) [] [Prim Bool] flags_body
  mapM_ addStm mapk_bnds
  flags <- letExp "flags" $ Op mapk

  lam' <- addFlagToLambda nes lam
  fold_lam' <- addFlagToLambda nes fold_lam

  let pat' = pat { patternValueElements = PatElem unused_flag_array BindVar
                                          (arrayOf (Prim Bool) (Shape [w]) NoUniqueness) :
                                          patternValueElements pat
                 }
  blockedScan pat' cs w lam' fold_lam' segment_size ispace inps (false:nes) (flags:arrs)
  where zero = constant (0 :: Int32)
        false = constant False
