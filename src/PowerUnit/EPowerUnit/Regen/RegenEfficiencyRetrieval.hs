{-
    Encapsulates functions for the modelling of
    induction motors regenerative mode through KNN-Regression.
-}
module PowerUnit.EPowerUnit.Regen.RegenEfficiencyRetrieval where
--
import Commons.ISUnits
--
import qualified KNNRegression.KNN as KNN
import KNNRegression.KNNRegressionClass
import PowerUnit.EPowerUnit.Regen.RTelInputNode
import PowerUnit.EPowerUnit.Regen.RTel
import KNNRegression.KNNConfig


{-
    Updates a KNN dataset given a set of telemetry data points.
-}
putAll :: [RTelInputNode] -> Maybe (KNN.Telemetry RTel) -> KNN.Telemetry RTel
putAll telins repo = KNN.addAll repo (map toRTel telins)

{-
    Predicts the efficiency of a query point.
-}
effOf :: KNNConfig -> RTelQuery -> KNN.Telemetry RTel -> Double
effOf c telin = KNN.valueOfClosest c (toQueryRTel telin)

{-
    Predicts the efficiency of a query point.
-}
effOf2 :: KNNConfig -> (Torque, Rev, Integer) -> KNN.Telemetry RTel -> Double
effOf2 c (_t, _s, soc) = effOf c (RTelQuery _t _s soc)