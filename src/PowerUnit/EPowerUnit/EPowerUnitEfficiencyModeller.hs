{-
    Encapsulates data types and functions for the modelling
    of an induction motor through multivariate polynomial
    regression.
-}
module PowerUnit.EPowerUnit.EPowerUnitEfficiencyModeller where
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
data EQuery =
        EQuery
            { eQueryTorque   :: Torque
            , eQueryRev      :: Rev
            , eQueryCellTemp :: Temp
            } deriving (Show, Eq)

{-
    Data type for a single entry of a telemetry.
-}
data ETelemetryEntry =
        ETelemetryEntry
            { eTelemetryEntryFV :: EQuery
            , eTelemetryEntryY  :: Current
            } deriving (Show, Eq)

{-
    Data type for the supervised learning of an engine's telemetry.
-}
data ETelemetrySupply = ETelemetrySupply Voltage [ETelemetryEntry]

{-
    Data type for the supervised learning of engine's telemetry.
-}
data EConsumableTelemetryEntry =
        EConsumableTelemetryEntry
            { eConsumableTelemetryEntryFV :: EQuery
            , eConsumableTelemetryEntryY  :: Double
            } deriving (Show, Eq)

{-
    Normalizes the input of a query.
-}
toNormalizedEQuery :: EQuery -> EQuery
toNormalizedEQuery q =
    let -- Query normalization.
        nTorque    = eQueryTorque q
        nRev       = fRevToRPM $ eQueryRev q
        nInletTemp = fTempToCelsius $ eQueryCellTemp q
     in EQuery nTorque nRev nInletTemp

{-
    Converts a query into a normalized feature vector.
-}
toFVEQuery :: EQuery -> [Double]
toFVEQuery (EQuery (Nm _nTorque) (RPM _nRev) (Celsius _nInletTemp)) =
    let nTorque = mapToRange (_nTorque, (0, 800)) (-1, 1)
        nRev    = mapToRange (fromIntegral _nRev, (500, 12000)) (-1, 1)
        nInletTemp = mapToRange (_nInletTemp, (-40, 60)) (-1, 1)
        nPow = let HP _pow = fPowerToHP $ fTorqueToKWatt (Nm _nTorque) (RPM _nRev)
               in mapToRange (_pow, (0, 800)) (-1, 1)
        --
     in [nPow, nTorque, nRev, nInletTemp]
toFVEQuery q = toFVEQuery $ toNormalizedEQuery q

{-
    Converts a telemetry entry to a consumable telemetry data point.
-}
toNormalizedETelemetryEntry :: ETelemetryEntry -> Voltage -> EConsumableTelemetryEntry
toNormalizedETelemetryEntry (ETelemetryEntry q y) v =
    let q' = toNormalizedEQuery q
        y' = y
        --
        nTelEntry = ETelemetryEntry q' y'
    in EConsumableTelemetryEntry q' (computeEff nTelEntry v)
    --
    where
    computeEff :: ETelemetryEntry -> Voltage -> Double
    computeEff (ETelemetryEntry (EQuery torque rev temp) (Ampere realCurr)) v =
        let -- Theoretical value:
            Ampere teoCurr = getCurrent torque rev v
        in realCurr / teoCurr

{-
    Converts a telemetry entry into a normalized feature vector its
    corresponding estimate.
-}
toFVETelemetryEntry :: ETelemetryEntry -> Voltage -> ([Double], Double)
toFVETelemetryEntry telEntry v =
    let EConsumableTelemetryEntry q y = toNormalizedETelemetryEntry telEntry v
     in (toFVEQuery q, y)

{-
    Converts a telemetry supply to a collection of normalized feature vectors
    and estimates.
-}
reorganizeETelemetrySupply :: ETelemetrySupply -> ([[Double]], [Double])
reorganizeETelemetrySupply (ETelemetrySupply v tels) = foldr append ([], []) tels
    where
        append :: ETelemetryEntry -> ([[Double]], [Double]) -> ([[Double]], [Double])
        append telEntry (fvs, ys) = let (fv, y) = toFVETelemetryEntry telEntry v in (fv:fvs, y:ys)

{-
    Performs a supervised learning of a supply of engine telemetry.
-}
learn :: ETelemetrySupply -> Integer -> Double -> MR.MRConfig
learn eTelSup nPoly bias =
    let (fvs, ys) = reorganizeETelemetrySupply eTelSup
     in MR.fitPoly fvs ys nPoly bias

{-
    Uses the trained model to predict current required
    by the motor to according to the configuration in the
    supplied query.
-}
predict :: MR.MRConfig -> EQuery -> Voltage -> Current
predict c q v =
        let -- Model expected efficiency.
            eff = MR.predictPoly c (toFVEQuery q)
            --
            -- Compute corrected current.
            expCurr = getCurrentWithEff (eQueryTorque q) (eQueryRev q) v eff
            --
         in expCurr
