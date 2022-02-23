{-
    Encapsulates an instance of KNNRegressionClass
    required for the modelling of induction motors
    with KNN-Regression (harvesting mode).
-}
module PowerUnit.EPowerUnit.Regen.RTel where
--
import Commons.ISUnits
import KNNRegression.KNNRegressionClass
import KNNRegression.KNN
--
import Data.Hashable
import Data.HashMap.Strict as HashMap


data RTel =
        RTel
            { rTelTorque :: Double
            , rTelSpeed  :: Double
            , rTelSOC    :: Double
            , rTelEffAvg :: Double
            , rTelNOccur :: Integer
            } deriving (Show, Eq)
--
instance Hashable RTel where
    hash etel =
        let (RTel t r soc _ _) = normalize etel
            in hash (show t ++ show r ++ show soc)
    hashWithSalt salt etel = hashWithSalt salt (hash etel)
--
instance KNNRegressionClass RTel where
    initCollection = HashMap.empty :: HashMap Int RTel

    normalize = id

    getValue = rTelEffAvg

    updateOnMatch t1 t2 =
        let len    = fromIntegral $ rTelNOccur t1
            eff    = rTelEffAvg t1
            newVal = rTelEffAvg t2
            
            newEff = ((eff * len) + newVal) / (len + 1)
            newLen = rTelNOccur t1 + 1

        in t1 {rTelEffAvg = newEff, rTelNOccur = newLen}

    toVector tel = let (RTel t r soc _ _) = normalize tel in [t, r, soc]