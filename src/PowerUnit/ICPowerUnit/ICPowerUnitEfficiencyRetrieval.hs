{-
    Encapsulates functions for the modelling of
    spark ignition engines KNN-Regression.
-}
module PowerUnit.ICPowerUnit.ICPowerUnitEfficiencyRetrieval where
--
import Commons.ISUnits
--
import qualified KNNRegression.KNN as KNN
import KNNRegression.KNNRegressionClass
import PowerUnit.ICPowerUnit.ICTelInputNode
import PowerUnit.ICPowerUnit.ICTel
import KNNRegression.KNNConfig


{-
    Updates a KNN dataset given a set of telemetry data points.
-}
putAll :: [ICTelInputNode] -> Maybe (KNN.Telemetry ICTel) -> KNN.Telemetry ICTel
putAll telins repo = KNN.addAll repo (map toICTel telins)

{-
    Predicts the efficiency of a query point.
-}
effOf :: KNNConfig -> ICTelQuery -> KNN.Telemetry ICTel -> Double
effOf c telin = KNN.valueOfClosest c (toQueryICTel telin)

{-
    Predicts the efficiency of a query point.
-}
effOf2 :: KNNConfig -> (Torque, Rev, Temp) -> KNN.Telemetry ICTel -> Double
effOf2 c (_t, _s, _a) = effOf c (ICTelQuery _t _s _a)
