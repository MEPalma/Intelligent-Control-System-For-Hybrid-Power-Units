{-
    Encapsulates data types and functions for the modelling
    of an induction motor through multivariate polynomial
    regression.
-}
module PowerUnit.EPowerUnit.Regen.RegenEfficiencyModeller where
--
import Commons.ISUnits
import Commons.Utilities
import PowerUnit.EPowerUnit.EEngine
--
import qualified MultivariateRegression.MultivariateRegression as MR

{-
    Data type of a query, this carries the
    power factors.
-}
data RQuery =
        RQuery
            { rQueryTorque :: Torque
            , rQueryRev    :: Rev
            , rQuerySOC    :: Integer
            } deriving (Show, Eq)

{-
    Data type for a single entry of a telemetry.
-}
data RTelemetryEntry =
        RTelemetryEntry
            { rTelemetryEntryFV :: RQuery
            , rTelemetryEntryY  :: Current
            } deriving (Show, Eq)

{-
    Data type for the supervised learning of an engine's telemetry.
-}
data RTelemetrySupply = RTelemetrySupply Voltage [RTelemetryEntry]

{-
    Data type for the supervised learning of engine's telemetry.
-}
data RConsumableTelemetryEntry =
        RConsumableTelemetryEntry
            { rConsumableTelemetryEntryFV :: RQuery
            , rConsumableTelemetryEntryY  :: Double
            } deriving (Show, Eq)

{-
    Normalizes the input of a query.
-}
toNormalizedRQuery :: RQuery -> RQuery
toNormalizedRQuery q =
    let -- Query normalization.
        nTorque = rQueryTorque q
        nRev    = fRevToRPM $ rQueryRev q
        nSOC    = rQuerySOC q
     in RQuery nTorque nRev nSOC

{-
    Converts a query into a normalized feature vector.
-}
toFVRQuery :: RQuery -> [Double]
toFVRQuery (RQuery (Nm _nTorque) (RPM _nRev) _soc) =
    let nTorque = mapToRange (_nTorque, (-800, 0)) (-1, 1)
        nRev    = mapToRange (fromIntegral _nRev, (500, 12000)) (-1, 1)
        nSoc = mapToRange (fromIntegral _soc, (0, 100)) (-1, 1)
        nPow = let HP _pow = fPowerToHP $ fTorqueToKWatt (Nm _nTorque) (RPM _nRev)
               in mapToRange (_pow, (-800, 0)) (-1, 1)
        --
     in [nTorque, nRev, nSoc]
toFVRQuery q = toFVRQuery $ toNormalizedRQuery q

{-
    Converts a telemetry entry to a consumable telemetry data point.
-}
toNormalizedRTelemetryEntry :: RTelemetryEntry -> Voltage -> RConsumableTelemetryEntry
toNormalizedRTelemetryEntry (RTelemetryEntry q y) v =
    let q' = toNormalizedRQuery q
        y' = y
        --
        nTelEntry = RTelemetryEntry q' y'
    in RConsumableTelemetryEntry q' (computeEff nTelEntry v)
    --
    where
    computeEff :: RTelemetryEntry -> Voltage -> Double
    computeEff (RTelemetryEntry (RQuery torque rev nSOC) (Ampere realCurr)) v =
        let -- Theoretical value:
            Ampere teoCurr = getRegenCurrent torque rev v
        in realCurr / teoCurr

{-
    Converts a telemetry entry into a normalized feature vector its
    corresponding estimate.
-}
toFVRTelemetryEntry :: RTelemetryEntry -> Voltage -> ([Double], Double)
toFVRTelemetryEntry telEntry v =
    let RConsumableTelemetryEntry q y = toNormalizedRTelemetryEntry telEntry v
     in (toFVRQuery q, y)

{-
    Converts a telemetry supply to a collection of normalized feature vectors
    and estimates.
-}
reorganizeRTelemetrySupply :: RTelemetrySupply -> ([[Double]], [Double])
reorganizeRTelemetrySupply (RTelemetrySupply v tels) = foldr append ([], []) tels
    where
        append :: RTelemetryEntry -> ([[Double]], [Double]) -> ([[Double]], [Double])
        append telEntry (fvs, ys) = let (fv, y) = toFVRTelemetryEntry telEntry v in (fv:fvs, y:ys)

{-
    Performs a supervised learning of a supply of engine telemetry.
-}
learn :: RTelemetrySupply -> Integer -> Double -> MR.MRConfig
learn rTelSup nPoly bias =
    let (fvs, ys) = reorganizeRTelemetrySupply rTelSup
     in MR.fitPoly fvs ys nPoly bias

{-
    Uses the trained model to predict current harvested
    by the motor to according to the configuration in the
    supplied query.
-}
predict :: MR.MRConfig -> RQuery -> Voltage -> Current
predict c q v =
        let -- Model expected efficiency.
            eff = MR.predictPoly c (toFVRQuery q)
            --
            -- Compute corrected current.
            expCurr = getRegenCurrentWithEff (rQueryTorque q) (rQueryRev q) v eff
            --
         in expCurr
