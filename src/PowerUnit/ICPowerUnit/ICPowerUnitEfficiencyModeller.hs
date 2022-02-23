{-
    Encapsulates data types and functions for the modelling
    of a spark ignition engine through multivariate polynomial
    regression.
-}
module PowerUnit.ICPowerUnit.ICPowerUnitEfficiencyModeller where
--
import Commons.ISUnits
import Commons.Utilities
import PowerUnit.ICPowerUnit.Fuel.Fuel
import PowerUnit.ICPowerUnit.ICEngine
import qualified MultivariateRegression.MultivariateRegression as MR
--
import Debug.Trace


{-
    Data type of a query, this carries the
    engine configuration, and power factors.
-}
data ICQuery =
        ICQuery
            { icQueryNumCylinders :: Integer
            , icQueryCompRatio :: Ratio
            , icQueryTorque    :: Torque
            , icQueryRev       :: Rev
            , icQueryInletTemp :: Temp
            } deriving (Show, Eq)

{-
    Data type for a single entry of a telemetry.
-}
data ICTelemetryEntry =
        ICTelemetryEntry
            { icTelemetryEntryFV :: ICQuery
            , icTelemetryEntryY  :: MassFlow
            } deriving (Show, Eq)

{-
    Data type for the supervised learning of an engine's telemetry.
-}
newtype ICTelemetrySupply = ICTelemetrySupply [ICTelemetryEntry]

{-
    Data type for the supervised learning of engine's telemetry.
-}
data ICConsumableTelemetryEntry =
        ICConsumableTelemetryEntry
            { icConsumableTelemetryEntryFV :: ICQuery
            , icConsumableTelemetryEntryY  :: Double
            } deriving (Show, Eq)

{-
    Normalizes the input of a query.
-}
toNormalizedICQuery :: ICQuery -> ICQuery
toNormalizedICQuery q =
    let -- Query normalization.
        nTorque    = icQueryTorque q
        nRev       = fRevToRPM $ icQueryRev q
        nInletTemp = fTempToCelsius $ icQueryInletTemp q
     in ICQuery (icQueryNumCylinders q) (icQueryCompRatio q) nTorque nRev nInletTemp

{-
    Converts a query into a normalized feature vector.
-}
toFVICQuery :: ICQuery -> [Double]
toFVICQuery (ICQuery _ _ (Nm _nTorque) (RPM _nRev) (Celsius _nInletTemp)) =
    let nTorque = mapToRange (_nTorque, (0, 800)) (-1, 1)
        nRev    = mapToRange (fromIntegral _nRev, (1000, 12000)) (-1, 1)
        nInletTemp = mapToRange (_nInletTemp, (-40, 60)) (-1, 1)
        nPow = let HP _pow = fPowerToHP $ fTorqueToKWatt (Nm _nTorque) (RPM _nRev)
               in mapToRange (_pow, (0, 800)) (-1, 1)
        --
     in [nPow, nTorque, nRev, nInletTemp]
toFVICQuery q = toFVICQuery $ toNormalizedICQuery q

{-
    Converts a telemetry entry to a consumable telemetry data point.
-}
toNormalizedICTelemetryEntry :: ICTelemetryEntry -> ICConsumableTelemetryEntry
toNormalizedICTelemetryEntry (ICTelemetryEntry q y) = 
    let q' = toNormalizedICQuery q
        y' = massFlowToGramsPerSecond y
        --
        nTelEntry = ICTelemetryEntry q' y'
    in ICConsumableTelemetryEntry q' (computeEff nTelEntry)
    --
    where
    computeEff :: ICTelemetryEntry -> Double
    computeEff (ICTelemetryEntry (ICQuery nCyls cr torque rev temp) (GramPerSecond realAF)) =
        let -- Theoretical value:
            teoAirFuelMixture = fOttoCycleAFMForTorque cr temp torque
            MixtureFlow (GramPerSecond teoAirMassFlow) _ = mixtureToFlow teoAirFuelMixture nCyls rev
        in realAF / teoAirMassFlow

{-
    Converts a telemetry entry into a normalized feature vector its
    corresponding estimate.
-}
toFVICTelemetryEntry :: ICTelemetryEntry -> ([Double], Double)
toFVICTelemetryEntry telEntry = let ICConsumableTelemetryEntry q y = toNormalizedICTelemetryEntry telEntry
                                in (toFVICQuery q, y)

{-
    Converts a telemetry supply to a collection of normalized feature vectors
    and estimates.
-}
reorganizeICTelemetrySupply :: ICTelemetrySupply -> ([[Double]], [Double])
reorganizeICTelemetrySupply (ICTelemetrySupply tels) = foldr append ([], []) tels
    where
        append :: ICTelemetryEntry -> ([[Double]], [Double]) -> ([[Double]], [Double])
        append telEntry (fvs, ys) = let (fv, y) = toFVICTelemetryEntry telEntry in (fv:fvs, y:ys)

{-
    Performs a supervised learning of a supply of engine telemetry.
-}
learn :: ICTelemetrySupply -> Integer -> Double -> MR.MRConfig
learn icTelSup nPoly bias =
    let (fvs, ys) = reorganizeICTelemetrySupply icTelSup
     in MR.fitPoly fvs ys nPoly bias

{-
    Uses the trained model to predict the air and fuel mixture flow
    required by the engine to according to the configuration in the
    supplied query.
-}
predict :: MR.MRConfig -> ICQuery -> Maybe MixtureFlow
predict c q =
    let mixtureFlow = fMixtureFlowInGrams $ predictFlow c q
        (MixtureFlow (GramPerSecond af) (GramPerSecond ff)) = mixtureFlow
    in if af > 0 && ff > 0 then Just mixtureFlow else Nothing
    --
    where
    predictFlow :: MR.MRConfig -> ICQuery -> MixtureFlow
    predictFlow c q =
        let -- Normalize query.
            nQ = toNormalizedICQuery q
            -- Retrieve efficiency.
            eff = MR.predictPoly c (toFVICQuery nQ)
            --
            -- Computer air fuel mixture.
            -- afMix = mixtureForTorque (icQueryTorque nQ)
            afMix = fOttoCycleAFMForTorque (icQueryCompRatio nQ) (icQueryInletTemp nQ) (icQueryTorque nQ)
            --
        in mixtureToFlowCorrected afMix (icQueryNumCylinders nQ) (icQueryRev nQ) eff