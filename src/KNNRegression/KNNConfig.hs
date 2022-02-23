module KNNRegression.KNNConfig where

import KNNRegression.KNNRegressionClass


data KNNConfig =
        KNNConfig
            { knnConfigNeighbors :: Int -> Int
            , knnConfigAvgCoeff  :: Integer
            }
--
instance Show KNNConfig where
    show (KNNConfig f nnei) = "KNNConfig {knnConfigNeighbors=(Int -> Int), knnConfigAvgCoeff=" ++ show nnei ++ "}"
--
instance Eq KNNConfig where
    (==) c c' = False
    (/=) c c' = True