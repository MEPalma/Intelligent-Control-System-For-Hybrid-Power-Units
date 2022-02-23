{-
    Encapsulates data types for the modelling
    of spark ignition engines.
-}
module PowerUnit.ICPowerUnit.ICPowerUnitType where
--
import Commons.ISUnits
--
import PowerUnit.ICPowerUnit.ICEngine
import PowerUnit.ICPowerUnit.Fuel.Fuel
--
import PowerUnit.PredictionStrategy
--
import KNNRegression.KNN
import PowerUnit.ICPowerUnit.ICTel
import KNNRegression.KNNConfig
--
import MultivariateRegression.MultivariateRegression

{-
    Data type for the modelling of a spark
    ignition engine.
-}
data ICPowerUnit =
        ICPowerUnit
            { icpuEffConfig          :: MRConfig
            , icpuKnnConfig          :: KNNConfig
            , icpuTelPoints          :: Telemetry ICTel
            , icpuPredictionStrategy :: PredictionStrategy
            , icpuUpperRevLimiter    :: Rev
            , icpuLowerRevLimiter    :: Rev
            , icpuNumCylinders       :: Integer
            , icpuCompRatio          :: Ratio
            , icpuMaxPower           :: Power
            , icpuMaxTorque          :: Torque
            } deriving (Show, Eq)

{-
    Data type for the configuration of operation
    of a spark ignition engine.
-}
data ICPowerUnitConfig =
        ICPowerUnitConfig
            { icpucTorque  :: Torque
            , icpucSpeed   :: Rev
            , icpucMixture :: MixtureFlow
            } deriving (Show, Eq)