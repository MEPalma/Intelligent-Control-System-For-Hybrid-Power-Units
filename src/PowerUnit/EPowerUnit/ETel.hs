{-
    Encapsulates an instance of KNNRegressionClass
    required for the modelling of induction motors
    with KNN-Regression (power mode).
-}
module PowerUnit.EPowerUnit.ETel where
--
import Commons.ISUnits
import KNNRegression.KNNRegressionClass
import KNNRegression.KNN
--
import Data.Hashable
import Data.HashMap.Strict as HashMap


data ETel =
        ETel
            { eTelTorque  :: Double
            , eTelSpeed   :: Double
            , eTelAmbient :: Double
            , eTelEffAvg  :: Double
            , eTelNOccur  :: Integer
            } deriving (Show, Eq)
--
instance Hashable ETel where
    hash etel =
        let (ETel t s a _ _) = normalize etel
         in hash (show t ++ show s ++ show a)
    hashWithSalt salt etel = hashWithSalt salt (hash etel)
--
instance KNNRegressionClass ETel where
    initCollection = HashMap.empty :: HashMap Int ETel

    normalize = id

    getValue = eTelEffAvg

    updateOnMatch t1 t2 = let len    = fromIntegral $ eTelNOccur t1
                              eff    = eTelEffAvg t1
                              newVal = eTelEffAvg t2
                              
                              newEff = ((eff * len) + newVal) / (len + 1)
                              newLen = eTelNOccur t1 + 1

                          in t1 {eTelEffAvg = newEff, eTelNOccur = newLen}

    toVector tel = let (ETel t r a _ _) = normalize tel in [t, r, a]