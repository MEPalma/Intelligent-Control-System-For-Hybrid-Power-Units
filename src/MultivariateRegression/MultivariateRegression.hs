{-
    Encapsulates data types and functions for the computation
    of multivariate polynomial regression.
-}
module MultivariateRegression.MultivariateRegression where
--
import qualified Data.Matrix as Matrix
import Debug.Trace

{-# ANN module "HLint: ignore Redundant bracket" #-}

{-
    Data type for the configuration of a multivariate
    polynomial regression evaluation.
-}
data MRConfig =
        MRConfig
            { mrPolynomialDegree :: Integer
            , mrWeights          :: [Double]
            , mrBias             :: Double
            } deriving (Show, Eq)


{-
    Takes a feature vector and evaluates each feature
    up to n polynomials; attaches a 1 constant, for
    intersection of the regressors, as first feature
    in the resulting feature vector.
-}
toPoly :: Integer -> [Double] -> [Double]
toPoly p fs = 1 : [x^d | x <- fs, d <- [1..p]]

{-
    Computes the optimal regressors weights by normal function.

    @[[Double]]: collection of feature vectors.
    @[Double]: estimates vector.
    @Integer: number of polynomials.
    @Double: amount of bias of the fit.

    @MRConfig: resulting multivariate polynomial regression configuration.
-}
fitPoly :: [[Double]] -> [Double] -> Integer -> Double -> MRConfig
fitPoly fvs t p bias =
    MRConfig p (Matrix.toList w) bias
    where
    nRows = length fvs
    nColumns = length $ toPoly p (head fvs)
    --
    a = Matrix.fromList nRows nColumns (concatMap (toPoly p) fvs)
    aT = Matrix.transpose a
    --
    Right invaTa = Matrix.inverse $ (Matrix.multStd aT a) + (Matrix.scaleMatrix bias (Matrix.identity nColumns)) -- (aT*a + kI)^-1
    --
    aTt = Matrix.multStd aT (Matrix.fromList nRows 1 t)
    --
    w = Matrix.multStd invaTa aTt

{-
    Takes a multivariate polynomial configuration and a feature vector,
    and computes the estimate.
-}
predictPoly :: MRConfig -> [Double] -> Double
predictPoly (MRConfig p w bias) x = sum $ zipWith (*) (toPoly p x) w
