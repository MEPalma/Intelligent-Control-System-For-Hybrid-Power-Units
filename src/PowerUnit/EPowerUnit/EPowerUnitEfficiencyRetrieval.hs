{-
    Encapsulates functions for the modelling of
    induction motors through KNN-Regression.
-}
module PowerUnit.EPowerUnit.EPowerUnitEfficiencyRetrieval where
--
import Commons.ISUnits
--
import qualified KNNRegression.KNN as KNN
import KNNRegression.KNNRegressionClass
import KNNRegression.KNNConfig
import PowerUnit.EPowerUnit.ETelInputNode
import PowerUnit.EPowerUnit.ETel


{-
    Updates a KNN dataset given a set of telemetry data points.
-}
putAll :: [ETelInputNode] -> Maybe (KNN.Telemetry ETel) -> KNN.Telemetry ETel
putAll telins repo = KNN.addAll repo (map toETel telins)

{-
    Predicts the efficiency of a query point.
-}
effOf :: KNNConfig -> ETelQuery -> KNN.Telemetry ETel -> Double
effOf c telin = KNN.valueOfClosest c (toQueryETel telin)

{-
    Predicts the efficiency of a query point.
-}
effOf2 :: KNNConfig -> (Torque, Rev, Temp) -> KNN.Telemetry ETel -> Double
effOf2 c (_t, _s, _a) = effOf c (ETelQuery _t _s _a)