
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}

{-|

  Module      : AutoBench.Internal.Regression
  Description : Generating and fitting models for regression analysis.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module is responsible for generating and fitting regression models.
  See 'LinearType', 'LinearCandidate' and 'LinearFit' for more details.

  The system uses ridge regression because it has a closed-form solution so 
  is fast. Previously used LASSO implemented using the HVX library but 
  it was too computationally expensive, (see bottom of this file).

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - Too much white space;
   -
-}

module AutoBench.Internal.Regression
  (

  -- * Candidate generation 
    generateLinearCandidate    -- Generate a 'LinearCandidate' from a 'LinearType' by filling 
                               -- in the details of the model necessary to fit it to a given dataset.    
  -- * Regression analysis 
  , fitOLS                     -- Given a data set (a list of xy-'Coord's) and a 'LinearCandidate', 
                               -- fit the model to the data using the OLS method.
  , fitRidgeRegress            -- Given a data set (a list of xy-'Coord's) and a 'LinearCandidate', 
                               -- fit the model to the data using ridge regression.

  ) where 

import           Data.List            (minimumBy, nub)
import           Data.Ord             (comparing)
import qualified Data.Vector.Storable as V

import Numeric.LinearAlgebra
  ( Element
  , Matrix
  , Vector
  , (<>)
  , (<\>)
  , (#>)
  , asColumn
  , cmap
  , diag
  , dot
  , fromColumns
  , fromRows
  , konst
  , norm_2
  , norm_Inf
  , qr
  , rows
  , scale
  , subVector
  , sumElements
  , takeRows
  , toColumns
  , tr'
  , vjoin
  )

import qualified AutoBench.Internal.Expr as E
import AutoBench.Internal.Types 
  ( Coord
  , Exp
  , LinearCandidate(..) 
  , LinearType(..)
  , numPredictors 
  )

#ifdef MIN_VERSION_base
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif
#endif

-- * 'LinearCandidate' generation

-- | Generate a 'LinearCandidate' from a 'LinearType' by filling in the 
-- details of the model necessary to fit it to a given dataset. 
-- Makes use of the generation helper functions 'generateLinearCandidateExpr'
-- and 'generateLinearCandidateYHat'.
generateLinearCandidate :: LinearType -> LinearCandidate
generateLinearCandidate t@(Poly _) = 
  LinearCandidate 
   { 
     _lct   = t
   , _fxs   = id
   , _fex   = generateLinearCandidateExpr t
   , _fyhat = generateLinearCandidateYHat t
   }
generateLinearCandidate t@(Log b _) =
  LinearCandidate 
   { 
     _lct   = t
   , _fxs   = cmap $ logBase (fromIntegral b)
   , _fex   = generateLinearCandidateExpr t
   , _fyhat = generateLinearCandidateYHat t    
   }
generateLinearCandidate t@(PolyLog b _) =
  LinearCandidate 
   { 
     _lct   = t
   , _fxs   = cmap (\x -> x * logBase (fromIntegral b) x)
   , _fex   = generateLinearCandidateExpr t
   , _fyhat = generateLinearCandidateYHat t                                                
   }
generateLinearCandidate t@(Exp n) =
  LinearCandidate 
   { 
     _lct   = t
   , _fxs   = cmap (flip (**) $ fromIntegral n)
   , _fex   = generateLinearCandidateExpr t
   , _fyhat = generateLinearCandidateYHat t     
   }

-- * Linear fitting

-- | Given a data set (a list of xy-'Coord's) and a 'LinearCandidate',
-- fit the model to the data using the ordinary least squares (OLS) method. The 
-- result is the model's coefficients.
fitOLS :: [Coord] -> LinearCandidate -> Vector Double
fitOLS coords c = head $ toColumns $ (_preds <\> _resps)
  where (_preds, _resps) = genFitMatrices coords c
   
-- | Given a data set (a list of xy-'Coord's) and a 'LinearCandidate',
-- fit the model to the data using ridge regression. The result is the model's
-- coefficients.
fitRidgeRegress :: [Coord] -> LinearCandidate -> Vector Double
fitRidgeRegress coords c =
  -- Select the lambda that minimises mean squared error.
  fst $ minimumBy (comparing snd) $ zip fits errs
  where 
    n        = length coords
    (xs, ys) = unzip  coords 
    vXs      = V.fromList xs
    vYs      = V.fromList ys
    -- Fitting error is mean squared error.
    calcMse m vec = ((norm_2 $ V.zipWith (-) vYs vec) ** 2.0) / fromIntegral m
    -- Generate the matrices to solve.
    (_preds, _resps) = genFitMatrices coords c
    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------  
    -- Using ridge regression with maximum tuning parameter:
    -- https://stats.stackexchange.com/questions/289075/what-is-the-
    -- smallest-lambda-that-gives-a-0-component-in-lasso?noredirect=1&lq=1
    ---------------------------------------------------------------------------
    maxLam   = (1.0 / fromIntegral n) * norm_Inf (tr' _preds <> _resps)
    -- Minimise 'mse' to select best tuning parameter.
    lamStep  = maxLam / 10
    lamRange = [lamStep, 2 * lamStep .. maxLam]
    fits     = nub $ fmap (head 
                            . toColumns 
                            . ridgeRegress _preds _resps
                          ) lamRange
    -- ** Do not use vFxs here, use vXs ** 
    errs     = fmap (calcMse n . flip (_fyhat c) vXs) fits 
    ---------------------------------------------------------------------------  
    ---------------------------------------------------------------------------  
    -- Closed form solution of ridge regression.
    -- https://github.com/chris-taylor/aima-haskell/blob/master/src/AI/
    -- Learning/LinearRegression.hs
    ---------------------------------------------------------------------------
    ridgeRegress
      :: Matrix Double  -- X
      -> Matrix Double  -- Y
      -> Double         -- λ
      -> Matrix Double  -- (X'X + λI)^−1 X'Y.
    ridgeRegress x y lambda = 
      let m      = rows x
          (_, r) = qr x
          rr     = takeRows m r
          ww     = diag $ vjoin [0, konst 1 (m - 1)]
      in (tr' rr <> rr + (lambda `scale` ww)) <\> (tr' x <> y)

-- * Helpers

-- | Generate an expression ('Exp') for a 'LinearCandidate' that will allow
-- its model's equation to be pretty printed at a later time. Important note: 
-- the vectors are assumed to be of the correct form, /no/  checking is 
-- performed currently.
generateLinearCandidateExpr 
   :: LinearType      -- Model.
   -> Vector Double   -- Coefficients of the model.
   -> Exp             -- Model's equation as an expression for pretty printing.
--
-- Given coefficients: 
--
--   a_0, a_1, ..., a_n 
-- 
-- Generate:
--
--   a_0 + a_1 * x^1 + a_2 * x^2 + ... + a_n * x^n 
--
generateLinearCandidateExpr (Poly n) coeffs = 
  foldr1 E.Add . take (n + 1) $ zipWith ($) terms (V.toList coeffs)
  where terms = E.Num                                         -- a_0
                 : ( \a_k -> E.Mul (E.Num a_k) (E.Var "x")    -- a_1 * x
                   )   
                 : fmap ( \p a_k -> E.Mul (E.Num a_k)         -- a_k * x^k, k > 1
                                          (E.Pow (E.Var "x") 
                                                 (E.Num p)
                                          )
                        ) [2.0..]                    
--
-- Given coefficients: 
-- 
-- a_0, a_1, ..., a_n
--
-- Generate: 
-- 
-- a_0 + a_1 * log_b^1(x) + a_2 * log_b^2(x) + ... + a_n * log_b^n(x)
--
generateLinearCandidateExpr (Log b n) coeffs = 
  foldr1 E.Add $ take (n + 1) $ zipWith ($) terms (V.toList coeffs)
  where terms = E.Num                                                -- a_0
                 : ( \a_k -> E.Mul (E.Num a_k)                       -- a_1 * log_b(x)
                                   (E.Log (E.Num $ fromIntegral b) 
                                          (E.Var "x")
                                   )
                   )           
                 : fmap ( \p a_k -> E.Mul (E.Num a_k)                -- a_k * log_b^k(x), k > 1
                                          (E.Pow (E.Log (E.Num $ fromIntegral b) 
                                                        (E.Var "x")
                                                 )
                                                 (E.Num p)
                                          )
                        ) [2.0..]
--
-- Given coefficients: 
-- 
-- a_0, a_1, ..., a_n
--
-- Generate:
-- 
-- a_0 + a_1 * x^1 * log_b^1(x) + a_2 * x^2 * log_b^2(x) + ... + a_n * x^n * log_b^n(x) 
--
generateLinearCandidateExpr (PolyLog b n) coeffs = 
  foldr1 E.Add $ take (n + 1) $ zipWith ($) terms (V.toList coeffs)
  where terms = E.Num                                                -- a_0
                 : ( \a_k -> E.Mul (E.Num a_k)                       -- a_1 * x * log_b(x)
                                   (E.Mul (E.Var "x") 
                                          (E.Log (E.Num $ fromIntegral b) 
                                                 (E.Var "x")
                                          )
                                   )
                   )
                 : fmap ( \p a_k -> E.Mul (E.Num a_k)                -- a_k * x^k * log_b^k(x), k > 1
                                          (E.Mul (E.Pow (E.Var "x") 
                                                        (E.Num p)
                                                 )
                                                 (E.Pow (E.Log (E.Num $ fromIntegral b) 
                                                               (E.Var "x")
                                                        )
                                                        (E.Num p)
                                                 )
                                          )
                        ) [2.0..]
--
-- Given coefficients: 
-- 
-- a_0, a_1
--
-- Generate:
-- 
-- a_0 + a_1 * n^x 
--
generateLinearCandidateExpr (Exp n) coeffs = 
  foldr1 E.Add $ zipWith ($) terms (V.toList coeffs)
  where terms = [ E.Num  -- a_0
                , \a_1 -> E.Mul (E.Num a_1)   -- a_1 *
                                (E.Pow (E.Num $ fromIntegral n) -- n^x
                                        (E.Var "x")
                                ) 
                ]

-- | For a given set of x-coordinates, generate the y-coordinates that are 
-- predicted by the model of a 'LinearCandidate'. Important note: the vectors 
-- are assumed to be of the correct form, /no/ checking is performed currently.
generateLinearCandidateYHat 
  :: LinearType
  -> Vector Double   -- Coefficients of the model.
  -> Vector Double   -- x-coordinates.
  -> Vector Double   -- Predicted y-coordinates.
--
-- Given coefficients and x-coords:
--
--   a_0, a_1, ..., a_n     x_0, x_1, ..., x_m
-- 
-- For each x_i, generate: 
--
--   a_0 + a_1 * x_i^1 + a_2 * x_i^2 + ... + a_n * x_i^n 
--
generateLinearCandidateYHat (Poly n) coeffs xs =
  (fromColumns $ V.fromList (replicate m 1.0) : xs') #> coeffs
  where 
    m   = V.length xs
    xs' = zipWith vecPow [1.0..] (replicate n xs)
--
-- Given coefficients and x-coords: 
-- 
-- a_0, a_1, ..., a_n     x_0, x_1, ..., x_m
-- 
-- For each x_i, generate: 
-- 
-- a_0 + a_1 * log_b^1(x_i) + a_2 * log_b^2(x_i) + ... + a_n * log_b^n(x_i)
--
generateLinearCandidateYHat (Log b n) coeffs xs =
  (fromColumns $ V.fromList (replicate m 1.0) : xs') #> coeffs
  where
    m   = V.length xs
    xs' = zipWith (vecLogPow $ fromIntegral b) [1.0..] (replicate n xs) 
--
-- Given coefficients and x-coords: 
-- 
-- a_0, a_1, .., a_n     x_0, x_1, ..., x_m
--
-- For each x_i, generate:
-- 
-- a_0 + a_1 * x_i^1 * log_b^1(x_i) + a_2 * x_i^2 * log_b^2(x_i) + ... + a_n * x_i^n * log_b^n(x_i) 
--
generateLinearCandidateYHat (PolyLog b n) coeffs xs =
  (fromColumns $ V.fromList (replicate m 1.0) : xs') #> coeffs
  where
    m   = V.length xs
    xs' = zipWith (vecPolyLogPow $ fromIntegral b) [1.0..] (replicate n xs)
--
-- Given coefficients and x-coords: 
-- 
-- a_0, a_1      x_0, x_1, ..., x_m
--
-- For each x_i, generate:
-- 
-- a_0 + a_1 * n^x_i 
--     
generateLinearCandidateYHat (Exp n) coeffs xs =
  (fromColumns $ V.fromList (replicate m 1.0) : xs') #> coeffs
  where
    m   = V.length xs
    xs' = zipWith (vecBasePow $ fromIntegral n) [1.0..] (replicate 1 xs) 

-- | Constructs predictor (@preds@) and responder (@resps@) matrices so that 
-- regression equations can be solved in their matrix form.
genFitMatrices :: [Coord] -> LinearCandidate -> (Matrix Double, Matrix Double)
genFitMatrices coords c = gen (numPredictors (_lct c) - 1) 
  where 

    gen :: Int -> (Matrix Double, Matrix Double)
    gen k = let
               _xPows    = xPows k
               _xSumPows = xSumPows _xPows
               _ySums    = ySums _xPows k
               _preds    = preds _xSumPows k
               _resps    = resps _ySums 
            in (_preds, asColumn _resps)

    ---------------------------------------------------------------------------
    --
    --
    (xs, ys) = unzip coords
    -- Transform the x-coords using the candidate's 'fxs' function. This allows 
    -- us to fit different models to the data in a generic way. For example, a 
    -- 'Log b n' candidate will first take 'LogBase b' of the x-coords. We can 
    -- then use (a variant of) linear least squares to fit a straight line 
    -- through the resulting data points. Although this is a straight line on 
    -- the /resulting data/, it corresponds to a logarithmic function on the
    -- /initial/ data. This technique is standard.  
    vFxs = (_fxs c) (V.fromList xs)
    vYs  = V.fromList ys
    -- Number of data points.
    n    = length coords
    -- As the x-'Coords' have been transformed by the candidate's 'fxs'
    -- function, this method works generically for any model and 'k' 
    -- coefficients.
    ---------------------------------------------------------------------------
    --
    -- Helper functions to construct the matrices to solve using linear 
    -- regression. Note: these functions are to be used in a particular way so 
    -- their names reflect their use.
    --
    -- x_i^j for j = 1, 2, .., 2 * k
    xPows :: Int -> [Vector Double]
    xPows k = zipWith vecPow [1.0..] (replicate (2 * k) vFxs)
    -- SUM x_i^j for j = 1, 2, .., 2 * k
    xSumPows :: [Vector Double] -> [Double]
    xSumPows  = fmap sumElements
    -- SUM y_i, SUM x_i * y_i, SUM x_i^2 * y_i, .., SUM x_i^j * y_i
    ySums :: [Vector Double] -> Int -> [Double]
    ySums vs k = zipWith dot (V.fromList (replicate n 1.0) : vs) (replicate (k + 1) vYs)
    --
    -- Convert xSumPows into a matrix by 'chunking' off rows from the initial vector.
    --
    -- Rows are length k + 1, the initial rows are:
    -- 
    --   n      , SUM x_i  , SUM x_i^2, .., SUM x_i^k
    --   
    --   SUM x_i, SUM x_i^2, SUM x_i^3, .., SUM x_i^(k + 1)
    --
    -- The last row is:
    --
    --   SUM x_i^j, SUM x_i^(j + 1), .., SUM x_i^(2 * k)
    -- 
    -- These values are the /predictors/.
    --
    preds :: [Double] -> Int -> Matrix Double
    preds ds k = vecToMatrix (V.fromList $ fromIntegral n : ds) (k + 1)
    -- 'ySums' are the /responders/.
    resps :: [Double] -> Vector Double
    resps  = V.fromList


-- | Raise each double in a vector of doubles to a given power.
vecPow :: Double -> Vector Double -> Vector Double
vecPow pow = cmap (** pow)

-- | For each double 'x' in a vector of doubles, apply the following 
-- calculation:
--
-- > \x -> base ** (x * pow)
--
-- for a given 'base' and 'pow'.
vecBasePow :: Double -> Double -> Vector Double -> Vector Double
vecBasePow base pow = cmap (\x -> base ** (x * pow))

-- | For each double 'x' in a vector of doubles, apply the following 
-- calculation:
--
-- > \x -> (x ** pow) * (logBase b x ** pow)
--
-- for a given 'b' and 'pow'.
vecPolyLogPow :: Double -> Double -> Vector Double -> Vector Double 
vecPolyLogPow b pow = cmap (\x -> (x ** pow) * ((logBase b x) ** pow))

-- | For each double 'x' in a vector of doubles, apply the following 
-- calculation:
--
-- > \x -> logBase b x ** pow
--
-- for a given 'b' and 'pow'.
vecLogPow :: Double -> Double -> Vector Double -> Vector Double 
vecLogPow b pow = cmap (\x -> (logBase b x) ** pow)

-- | Create a matrix by 'chunking' off rows of a given length from a vector.
-- Works similarly to 'tails'.
vecToMatrix :: (V.Storable a, Element a) => Vector a -> Int -> Matrix a
vecToMatrix v j = fromRows [ subVector i j v | i <- [0..j - 1] ]





















{- 

LASSO has been retired for the time being due to computational expense.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds        #-}

import           HVX.Internal.Constraints ((<=~))
import           HVX.Primitives           ((+~), (*~), neg, norm, quadform)
import           HVX.Internal.Solvers     (ellipsoidMinimize)
import qualified HVX.Internal.Primitives  as HVX


-- ** LASSO (Least Absolute Shrinkage and Selection Operator)


fitLASSO :: [Coord] -> LinearCandidate -> Vector Double
fitLASSO coords c =
  -- Select the lambda that minimises mean squared error.
  fst $ minimumBy (comparing snd) $ zip fits errs
  where 
    n        = length coords
    (xs, ys) = unzip  coords 
    vXs      = V.fromList xs
    vYs      = V.fromList ys
    -- Fitting error is mean squared error.
    calcMse m vec = ((norm_2 $ V.zipWith (-) vYs vec) ** 2.0) / fromIntegral m
    -- Generate the matrices to solve.
    (_preds, _resps) = genFitMatrices coords c
    ---------------------------------------------------------------------------
    --------------------------------------------------------------------------- 
    initCoeffs = _preds <\> _resps 
    maxT = V.sum $ cmap abs $ flatten initCoeffs
    -- Minimise 'mse' to select best tuning parameter.
    tStep  = maxT / 10
    tRange = [0 + tStep, 0 + (2 * tStep) .. maxT]
    fits   = filter (all (\m -> not (isNaN m || isInfinite m)) . V.toList) 
              . nub 
              $ fmap (head 
                       . toColumns 
                       . lassoRegress _preds _resps
                     ) tRange
    -- ** Do not use vFxs here, use vXs ** 
    errs   = fmap (calcMse n . flip (fyhat c) vXs) fits
    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------  
    -- LASSO regression (convex equation using the subgradient method in HVX).
    lassoRegress 
      :: Matrix Double      -- ^ n >< n matrix X.
      -> Matrix Double      -- ^ n >< 1 matrix Y.
      -> Double             -- ^ Constraint on sum of coefficients.
      -> Matrix Double      -- ^ Optimal variable values.
    lassoRegress x y t = snd . head $ res
      where 
        (res, _, _) = ellipsoidMinimize expr [constr] [("a", m)] 1e-20 (2 * maxT)
        m      = rows x 
        constr = norm 1 (HVX.EVar "a") <=~ (HVX.EConst $ konst t (1, 1))
        expr   = quadform (HVX.EConst $ ident m) $ ((HVX.EConst y) +~ neg (HVX.EConst x *~ HVX.EVar "a"))

-}