{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | An unstructured grab-bag of various tools and inspection
-- functions that didn't really fit anywhere else.
module Futhark.Tools
  ( module Futhark.Construct,
    redomapToMapAndReduce,
    dissectScrema,
    sequentialStreamWholeArray,
    partitionChunkedFoldParameters,

    -- * Primitive expressions
    module Futhark.Analysis.PrimExp.Convert,
  )
where

import Control.Monad.Identity
import Futhark.Analysis.PrimExp.Convert
import Futhark.Construct
import Futhark.IR
import Futhark.IR.SOACS.SOAC
import Futhark.Util

-- | Turns a binding of a @redomap@ into two seperate bindings, a
-- @map@ binding and a @reduce@ binding (returned in that order).
--
-- Reuses the original pattern for the @reduce@, and creates a new
-- pattern with new 'Ident's for the result of the @map@.
--
-- Only handles a pattern with an empty 'patternContextElements'.
redomapToMapAndReduce ::
  ( MonadFreshNames m,
    Bindable rep,
    ExpDec rep ~ (),
    Op rep ~ SOAC rep
  ) =>
  Pattern rep ->
  ( SubExp,
    Commutativity,
    LambdaT rep,
    LambdaT rep,
    [SubExp],
    [VName]
  ) ->
  m (Stm rep, Stm rep)
redomapToMapAndReduce
  (Pattern [] patelems)
  (w, comm, redlam, map_lam, accs, arrs) = do
    (map_pat, red_pat, red_args) <-
      splitScanOrRedomap patelems w map_lam accs
    let map_bnd = mkLet [] map_pat $ Op $ Screma w arrs (mapSOAC map_lam)
        (nes, red_arrs) = unzip red_args
    red_bnd <-
      Let red_pat (defAux ()) . Op
        <$> (Screma w red_arrs <$> reduceSOAC [Reduce comm redlam nes])
    return (map_bnd, red_bnd)
redomapToMapAndReduce _ _ =
  error "redomapToMapAndReduce does not handle a non-empty 'patternContextElements'"

splitScanOrRedomap ::
  (Typed dec, MonadFreshNames m) =>
  [PatElemT dec] ->
  SubExp ->
  LambdaT rep ->
  [SubExp] ->
  m ([Ident], PatternT dec, [(SubExp, VName)])
splitScanOrRedomap patelems w map_lam accs = do
  let (acc_patelems, arr_patelems) = splitAt (length accs) patelems
      (acc_ts, _arr_ts) = splitAt (length accs) $ lambdaReturnType map_lam
  map_accpat <- zipWithM accMapPatElem acc_patelems acc_ts
  map_arrpat <- mapM arrMapPatElem arr_patelems
  let map_pat = map_accpat ++ map_arrpat
      red_args = zip accs $ map identName map_accpat
  return (map_pat, Pattern [] acc_patelems, red_args)
  where
    accMapPatElem pe acc_t =
      newIdent (baseString (patElemName pe) ++ "_map_acc") $ acc_t `arrayOfRow` w
    arrMapPatElem = return . patElemIdent

-- | Turn a Screma into a Scanomap (possibly with mapout parts) and a
-- Redomap.  This is used to handle Scremas that are so complicated
-- that we cannot directly generate efficient parallel code for them.
-- In essense, what happens is the opposite of horisontal fusion.
dissectScrema ::
  ( MonadBinder m,
    Op (Rep m) ~ SOAC (Rep m),
    Bindable (Rep m)
  ) =>
  Pattern (Rep m) ->
  SubExp ->
  ScremaForm (Rep m) ->
  [VName] ->
  m ()
dissectScrema pat w (ScremaForm scans reds map_lam) arrs = do
  let num_reds = redResults reds
      num_scans = scanResults scans
      (scan_res, red_res, map_res) =
        splitAt3 num_scans num_reds $ patternNames pat

  to_red <- replicateM num_reds $ newVName "to_red"

  let scanomap = scanomapSOAC scans map_lam
  letBindNames (scan_res <> to_red <> map_res) $
    Op $ Screma w arrs scanomap

  reduce <- reduceSOAC reds
  letBindNames red_res $ Op $ Screma w to_red reduce

-- | Turn a stream SOAC into statements that apply the stream lambda
-- to the entire input.
sequentialStreamWholeArray ::
  (MonadBinder m, Bindable (Rep m)) =>
  Pattern (Rep m) ->
  SubExp ->
  [SubExp] ->
  LambdaT (Rep m) ->
  [VName] ->
  m ()
sequentialStreamWholeArray pat w nes lam arrs = do
  -- We just set the chunksize to w and inline the lambda body.  There
  -- is no difference between parallel and sequential streams here.
  let (chunk_size_param, fold_params, arr_params) =
        partitionChunkedFoldParameters (length nes) $ lambdaParams lam

  -- The chunk size is the full size of the array.
  letBindNames [paramName chunk_size_param] $ BasicOp $ SubExp w

  -- The accumulator parameters are initialised to the neutral element.
  forM_ (zip fold_params nes) $ \(p, ne) ->
    letBindNames [paramName p] $ BasicOp $ SubExp ne

  -- Finally, the array parameters are set to the arrays (but reshaped
  -- to make the types work out; this will be simplified rapidly).
  forM_ (zip arr_params arrs) $ \(p, arr) ->
    letBindNames [paramName p] $
      BasicOp $
        Reshape (map DimCoercion $ arrayDims $ paramType p) arr

  -- Then we just inline the lambda body.
  mapM_ addStm $ bodyStms $ lambdaBody lam

  -- The number of results in the body matches exactly the size (and
  -- order) of 'pat', so we bind them up here, again with a reshape to
  -- make the types work out.
  forM_ (zip (patternElements pat) $ bodyResult $ lambdaBody lam) $ \(pe, se) ->
    case (arrayDims $ patElemType pe, se) of
      (dims, Var v)
        | not $ null dims ->
          letBindNames [patElemName pe] $ BasicOp $ Reshape (map DimCoercion dims) v
      _ -> letBindNames [patElemName pe] $ BasicOp $ SubExp se

-- | Split the parameters of a stream reduction lambda into the chunk
-- size parameter, the accumulator parameters, and the input chunk
-- parameters.  The integer argument is how many accumulators are
-- used.
partitionChunkedFoldParameters ::
  Int ->
  [Param dec] ->
  (Param dec, [Param dec], [Param dec])
partitionChunkedFoldParameters _ [] =
  error "partitionChunkedFoldParameters: lambda takes no parameters"
partitionChunkedFoldParameters num_accs (chunk_param : params) =
  let (acc_params, arr_params) = splitAt num_accs params
   in (chunk_param, acc_params, arr_params)
