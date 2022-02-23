{-
    Encapsulates data types for the modelling
    of induction motors.
-}
module PowerUnit.EPowerUnit.EPowerUnitType where
--
import Commons.ISUnits
--
import PowerUnit.EPowerUnit.EEngine
--
import PowerUnit.PredictionStrategy
import PowerUnit.EPowerUnit.ETel
import PowerUnit.EPowerUnit.Regen.RTel
import KNNRegression.KNN
import KNNRegression.KNNConfig
--
import MultivariateRegression.MultivariateRegression


{-
    Data type for the modelling of an
    induction motor.
-}
data EPowerUnit =
        EPowerUnit
            { epuEffConfig          :: MRConfig
            , epuKnnConfigPower     :: KNNConfig
            , epuTelPoints          :: Telemetry ETel
            , epuPredictionStrategy :: PredictionStrategy
            --
            , epuRegenEffConfig     :: MRConfig
            , epuKnnConfigRegen     :: KNNConfig
            , epuRegenTelPoints     :: Telemetry RTel
            , epuRegenPredictionStrategy :: PredictionStrategy
            --
            , epuUpperRevLimiter    :: Rev
            , epuLowerRevLimiter    :: Rev
            , epuVoltage            :: Voltage
            --
            , epuMaxPower           :: Power
            , epuMaxTorque          :: Torque
            } deriving (Show, Eq)

{-
    Data type for the configuration of operation
    of an induction motor.
-}
data EPowerUnitConfig =
        EPowerUnitConfig
            { epucTorque  :: Torque
            , epucSpeed   :: Rev
            , epucCurrent :: Current
            , epucVoltage :: Voltage
            } deriving (Show, Eq)