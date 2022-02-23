{-
    Encapsulates data types and functions
    for the composition of a density controlled
    KNN-Regression dataset.
-}
module PowerUnit.ICPowerUnit.ICTelInputNode where
--
import Commons.ISUnits
import Commons.Utilities
import PowerUnit.ICPowerUnit.ICTel


-- Constants for density control
-- of the KNN dataset.
_TORQUE_STEP = 10 -- Nm
_REV_STEP = 100 -- RPM
_TEMP_STEP = 5 -- Celsius


{-
    Data type for a mon-normalized input node,
    for the supervised building of a dataset.
-}
data ICTelInputNode =
    ICTelInputNode
    { ictnTorque  :: Torque
    , ictnSpeed   :: Rev
    , ictnAmbient :: Temp
    , ictnEffAvg  :: Double
    } deriving (Show, Eq)

{-
    Data type for the querying of a KNN dataset.
-}
data ICTelQuery =
    ICTelQuery
    { ictqTorque  :: Torque
    , ictqSpeed   :: Rev
    , ictqAmbient :: Temp
    } deriving (Show, Eq)

{-
    Converts a query point to a KNN telemetry node.
-}
toQueryICTel :: ICTelQuery -> ICTel
toQueryICTel tin =
    let Nm t = ictqTorque tin
        RPM r = fRevToRPM (ictqSpeed tin)
        Celsius a = fTempToCelsius (ictqAmbient tin)
        --
        rt = mapTorqueToRange t
        rr = mapRevToRange (fromIntegral r)
        ra = mapTempToRange a
        --
    in ICTel rt rr ra 1 1

{-
    Converts a telemetry input point to a KNN telemetry node.
-}
toICTel :: ICTelInputNode -> ICTel
toICTel tin =
    let t = approximateOfTorque (ictnTorque tin)
        r = approximateOfRev (ictnSpeed tin)
        a = approximateOfTemp (ictnAmbient tin)
        e = ictnEffAvg tin
        --
    in ICTel t r a e 1

{-
    Performs a density control step about the torque feature.
-}
approximateOfTorque :: Torque -> Double
approximateOfTorque (Nm t) =
    let cluster_val = roundToClosestMultiple t _TORQUE_STEP
        --
     in mapTorqueToRange cluster_val
--
mapTorqueToRange :: Double -> Double
mapTorqueToRange t =
    let rangeBefore = (0, 1000)
        rangeAfter = (0, 2)
        --
        ranged_val = mapToRange (t, rangeBefore) rangeAfter
        --
     in ranged_val

{-
    Performs a density control step about the engine speed feature.
-}
approximateOfRev :: Rev -> Double
approximateOfRev _r =
    let RPM r = fRevToRPM _r
        cluster_val = roundToClosestMultiple (fromIntegral r) _REV_STEP
        --
     in mapRevToRange cluster_val
--
mapRevToRange :: Double -> Double
mapRevToRange r =
    let rangeBefore = (1000, 12000)
        rangeAfter = (0, 2)
        --
        ranged_val = mapToRange (r, rangeBefore) rangeAfter
        --
     in ranged_val

{-
    Performs a density control step about the ambient temperature feature.
-}
approximateOfTemp :: Temp -> Double
approximateOfTemp _t =
    let Celsius t = fTempToCelsius _t
        cluster_val = roundToClosestMultiple t _TEMP_STEP
        --
     in mapTempToRange cluster_val
--
mapTempToRange :: Double -> Double
mapTempToRange t =
    let rangeBefore = (-40, 60)
        rangeAfter = (0, 2)
        --
        ranged_val = mapToRange (t, rangeBefore) rangeAfter
        --
     in ranged_val