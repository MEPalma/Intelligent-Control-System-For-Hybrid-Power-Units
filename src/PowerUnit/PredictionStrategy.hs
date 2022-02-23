{-
    Data type including which prediction strategy an engine
    should be modelled with.
-}
module PowerUnit.PredictionStrategy where
{-
    @KNN: KNN-Regression
    @REG: Multivariate Polynomial Regression.
-}
data PredictionStrategy = KNN | REG deriving (Show, Eq)