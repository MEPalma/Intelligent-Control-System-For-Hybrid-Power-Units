module KNNRegression.KNNRegressionClass where

import Data.Hashable
import Data.HashMap.Strict


class (Hashable a, Show a)  => KNNRegressionClass a where

    initCollection :: HashMap Int a
    normalize :: a -> a
    getValue :: a -> Double
    updateOnMatch :: a -> a -> a

    toVector :: a -> [Double]