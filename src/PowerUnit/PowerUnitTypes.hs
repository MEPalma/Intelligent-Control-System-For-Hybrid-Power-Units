{-
    Encapsulates data types and functions for the modelling
    of a power unit.
-}
module PowerUnit.PowerUnitTypes where
--
import Commons.ISUnits
import Commons.Utilities
import Commons.Currency
import Commons.ColoredOutput
--
import GearboxDynamics.Gearbox
--
import PowerUnit.ICPowerUnit.ICPowerUnitType
import PowerUnit.EPowerUnit.EPowerUnitType
--
import Data.Hashable
import Text.Printf (printf)


{-# ANN module "HLint: ignore Redundant bracket" #-}


{-
    Data type of a power unit, as an aggregation
    of IC engine and induction motor.
-}
data PowerUnit = 
        PowerUnit
            { puICPowerUnit :: ICPowerUnit
            , puEPowerUnit  :: EPowerUnit
            } deriving (Show, Eq)


{-
    Data type to describe the mode of operation of a power unit,
    in terms of how the total load is distributed among the two engines.
-}
data PowerUnitMode =
      PUMPureICEngine
    | PUMPureEEngine
    | PUMCombined Integer -- ratio of ic engine, eg 30 = 30% ic and 70%.
    --
    | PUMFullECompensation
    deriving (Eq)
--
instance Show PowerUnitMode where
    show (PUMCombined r) =
        let pumLabel = showInColor "PUM:" WHITE
            icLabel = showInColor (printf "%3d%s IC" r "%") RED
            eLabel = showInColor (printf "%3d%s E" (100 - r) "%") GREEN
         in printf "%s %s %s" pumLabel icLabel eLabel
    show PUMPureEEngine = show $ PUMCombined 0
    show PUMPureICEngine = show $ PUMCombined 100
    show PUMFullECompensation = (showInColor "PUM: " WHITE) ++ (showInColor "E Compensation" GREEN)
--
instance Hashable PowerUnitMode where
    hash PUMPureICEngine = 100
    hash PUMPureEEngine  = 0
    hash PUMFullECompensation = 101
    hash (PUMCombined n) = fromIntegral n -- if n == 0 or 100 then this is equal to pure modes.
    --
    hashWithSalt salt pum = hashWithSalt salt (hash pum)
--
getPUMonICPercentageUsage :: Integer -> PowerUnitMode
getPUMonICPercentageUsage 0 = PUMPureEEngine
getPUMonICPercentageUsage 100 = PUMPureICEngine
getPUMonICPercentageUsage x
    | inRange x 0 100 = PUMCombined x
    | otherwise = error $ "INVALID PowerUnitMode: " ++ show x
--
isValidPUM :: PowerUnitMode -> Bool
isValidPUM (PUMCombined x) = inRange x 0 100
isValidPUM _ = True
--
getAllPUM :: Integer -> [PowerUnitMode]
getAllPUM step
    | inRange step 1 100 =
        (:) PUMFullECompensation $ map getPUMonICPercentageUsage $ takeWhile isValid (range 0 step) ++ [100 | 100 `mod` step /= 0]
        --
    | otherwise = [PUMPureEEngine, PUMFullECompensation, PUMPureICEngine]
    where
    isValid :: Integer -> Bool
    isValid x = inRange x 0 100
--
fullECompensationToSplit :: PowerUnit -> Torque -> (Torque, Torque)
fullECompensationToSplit (PowerUnit _ ePu) (Nm eTorque) =
    let Nm maxElecTorque = epuMaxTorque ePu
        --
        Nm elecUtilization = Nm $ min eTorque maxElecTorque
        --
        icUtilization = Nm $ eTorque - elecUtilization
        --
     in (icUtilization, Nm elecUtilization)


puConservativeMaxPower :: PowerUnit -> Power
puConservativeMaxPower (PowerUnit icPu ePu) =
    let KWatt icPuPow = fPowerToKWatt $ icpuMaxPower icPu
        KWatt ePuPow = fPowerToKWatt $  epuMaxPower ePu
     in KWatt $ max icPuPow ePuPow

{-
    Data type to contain all the required parameters for the
    running of ic engine and induction motor depending on the
    power unit mode selected. These include fuel flows, current
    and selected gear.
-}
data PowerUnitConfig =
      PUCPureICEngine              GearRatio ICPowerUnitConfig
    | PUCPureEEngine               GearRatio EPowerUnitConfig
    | PUCCombined          Integer GearRatio ICPowerUnitConfig EPowerUnitConfig
    | PUCFullECompensation         GearRatio ICPowerUnitConfig EPowerUnitConfig
    deriving (Eq)
--
instance Show PowerUnitConfig where
    show (PUCPureICEngine gr icConf) = printf "[Mode]: %s\n[Gear]: %s\n[ICEngine Config]:%s\n" (show PUMPureICEngine) (show gr) (show icConf)
    show (PUCPureEEngine gr eConf) = printf "[Mode]: %s\n[Gear]: %s\n[EMotor Config]:%s\n" (show PUMPureEEngine) (show gr) (show eConf)
    show (PUCCombined r gr icConf eConf) = printf "[Mode]: %s\n[Gear]: %s\n[ICEngine Config]:%s\n[EMotor Config]:%s\n" (show (PUMCombined r)) (show gr) (show icConf) (show eConf)
    show (PUCFullECompensation gr icConf eConf) = printf "[Mode]: %s\n[Gear]: %s\n[ICEngine Config]:%s\n[EMotor Config]:%s\n" (show (PUMFullECompensation)) (show gr) (show icConf) (show eConf)
--
powerUnitModeOfPowerUnitConfig :: PowerUnitConfig -> PowerUnitMode
powerUnitModeOfPowerUnitConfig PUCPureICEngine{} = PUMPureICEngine
powerUnitModeOfPowerUnitConfig PUCPureEEngine{} = PUMPureEEngine
powerUnitModeOfPowerUnitConfig (PUCCombined r _ _ _) = PUMCombined r
powerUnitModeOfPowerUnitConfig PUCFullECompensation{} = PUMFullECompensation
--
gearOfPowerUnitConfig :: PowerUnitConfig -> GearRatio
gearOfPowerUnitConfig (PUCPureICEngine gr _) = gr
gearOfPowerUnitConfig (PUCPureEEngine gr _) = gr
gearOfPowerUnitConfig (PUCCombined _ gr _  _) = gr
gearOfPowerUnitConfig (PUCFullECompensation gr _ _) = gr

{-
    Data type for the description of the power factors
    demanded from the power unit. This is meant to aid
    intra function communication only, specific data types
    are available for the modelling of engine's specific
    running costs.
-}
data PowerUnitDeliveryRequirement =
        PowerUnitDeliveryRequirement 
            { pudrTorque :: Torque
            , pudrRev    :: Rev
            , pudrAmbient :: Temp
            } deriving (Show, Eq)
