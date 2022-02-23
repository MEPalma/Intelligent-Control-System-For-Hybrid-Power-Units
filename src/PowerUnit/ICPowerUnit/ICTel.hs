{-
    Encapsulates an instance of KNNRegressionClass
    required for the modelling of spark ignition engines
    with KNN-Regression.
-}
module PowerUnit.ICPowerUnit.ICTel where
--
import Commons.ISUnits
import KNNRegression.KNNRegressionClass
import KNNRegression.KNN
--
import Data.Hashable
import Data.HashMap.Strict as HashMap


data ICTel =
    ICTel
    { icTelTorque  :: Double
    , icTelSpeed   :: Double
    , icTelAmbient :: Double
    , icTelEffAvg  :: Double
    , icTelNOccur  :: Integer
    } deriving (Show, Eq)
--
instance Hashable ICTel where
    hash ictel =
        let (ICTel t s a _ _) = normalize ictel
         in hash (show t ++ show s ++ show a)
    --
    hashWithSalt salt ictel = hashWithSalt salt (hash ictel)
--
instance KNNRegressionClass ICTel where
    initCollection = HashMap.empty :: HashMap Int ICTel

    normalize = id

    getValue = icTelEffAvg

    updateOnMatch t1 t2 = let len    = fromIntegral $ icTelNOccur t1
                              eff    = icTelEffAvg t1
                              newVal = icTelEffAvg t2
                              
                              newEff = ((eff * len) + newVal) / (len + 1)
                              newLen = icTelNOccur t1 + 1

                          in t1 {icTelEffAvg = newEff, icTelNOccur = newLen}

    toVector tel = let (ICTel t r a _ _) = normalize tel in [t, r, a]