{-
    Encapsulates data types and functions
    for the composition of a density controlled
    KNN-Regression dataset.
-}
module PowerUnit.EPowerUnit.ETelInputNode where
--
import Commons.ISUnits
import Commons.Utilities
import PowerUnit.EPowerUnit.ETel


-- Constants for density control
-- of the KNN dataset.
_TORQUE_STEP = 10 -- Nm
_REV_STEP = 100 -- RPM
_TEMP_STEP = 5 -- Celsius

{-
    Data type for a mon-normalized input node,
    for the supervised building of a dataset.
-}
data ETelInputNode =
        ETelInputNode
            { etnTorque  :: Torque
            , etnSpeed   :: Rev
            , etnAmbient :: Temp
            , etnEffAvg  :: Double
            } deriving (Show, Eq)

{-
    Data type for the querying of a KNN dataset.
-}
data ETelQuery =
        ETelQuery
            { etqTorque  :: Torque
            , etqSpeed   :: Rev
            , etqAmbient :: Temp
            } deriving (Show, Eq)

{-
    Converts a query point to a KNN telemetry node.
-}
toQueryETel :: ETelQuery -> ETel
toQueryETel tin =
    let Nm t = etqTorque tin
        RPM r = fRevToRPM (etqSpeed tin)
        Celsius a = fTempToCelsius (etqAmbient tin)
        --
        rt = mapTorqueToRange t
        rr = mapRevToRange (fromIntegral r)
        ra = mapTempToRange a
        --
    in ETel rt rr ra 1 1

{-
    Converts a telemetry input point to a KNN telemetry node.
-}
toETel :: ETelInputNode -> ETel
toETel tin =
    let t = approximateOfTorque (etnTorque tin)
        r = approximateOfRev (etnSpeed tin)
        a = approximateOfTemp (etnAmbient tin)
        e = etnEffAvg tin
        --
    in ETel t r a e 1

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
        rangeAfter = (0, 1)
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
    let rangeBefore = (500, 10000)
        rangeAfter = (0, 1)
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
        rangeAfter = (0, 1)
        --
        ranged_val = mapToRange (t, rangeBefore) rangeAfter
        --
     in ranged_val