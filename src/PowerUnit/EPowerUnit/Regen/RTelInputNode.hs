{-
    Encapsulates data types and functions
    for the composition of a density controlled
    KNN-Regression dataset.
-}
module PowerUnit.EPowerUnit.Regen.RTelInputNode where
--
import Commons.ISUnits
import Commons.Utilities
import PowerUnit.EPowerUnit.Regen.RTel

-- Constants for density control
-- of the KNN dataset.
_TORQUE_STEP = 10 -- Nm
_REV_STEP = 100 -- RPM
_SOC_STEP = 5 -- Celsius

{-
    Data type for a mon-normalized input node,
    for the supervised building of a dataset.
-}
data RTelInputNode =
        RTelInputNode
            { rtnTorque :: Torque
            , rtnSpeed  :: Rev
            , rtnSOC    :: Integer
            , rtnEffAvg :: Double
            } deriving (Show, Eq)

{-
    Data type for the querying of a KNN dataset.
-}
data RTelQuery =
        RTelQuery
            { rtqTorque :: Torque
            , rtqSpeed  :: Rev
            , rtqSOC    :: Integer
            } deriving (Show, Eq)

{-
    Converts a query point to a KNN telemetry node.
-}
toQueryRTel :: RTelQuery -> RTel
toQueryRTel tin =
    let Nm t = rtqTorque tin
        RPM r = fRevToRPM (rtqSpeed tin)
        soc = rtqSOC tin
        --
        rt = mapTorqueToRange t
        rr = mapRevToRange (fromIntegral r)
        rs = mapSOCToRange $ fromIntegral soc
        --
    in RTel rt rr rs 1 1

{-
    Converts a telemetry input point to a KNN telemetry node.
-}
toRTel :: RTelInputNode -> RTel
toRTel tin =
    let t = approximateOfTorque (rtnTorque tin)
        r = approximateOfRev (rtnSpeed tin)
        s = approximateOfSOC (rtnSOC tin)
        e = rtnEffAvg tin
        --
    in RTel t r s e 1

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
    Performs a density control step about the ambient temperature feature.
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
    Performs a density control step about the ambient SOC feature.
-}
approximateOfSOC :: Integer -> Double
approximateOfSOC soc =
    let cluster_val = roundToClosestMultiple (fromIntegral soc) _SOC_STEP
     in mapSOCToRange cluster_val
--
mapSOCToRange :: Double -> Double
mapSOCToRange soc =
    let rangeBefore = (0, 100)
        rangeAfter = (0, 1)
        --
        ranged_val = mapToRange (soc, rangeBefore) rangeAfter
        --
     in ranged_val