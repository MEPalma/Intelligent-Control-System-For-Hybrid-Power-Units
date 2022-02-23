{-
    Encapsulates function for the computation of expected running consts
    of an induction motor.
-}
module PowerUnit.EPowerUnit.EPowerUnit where 
--
import Commons.ISUnits
--
import Vehicle.Vehicle
--
import GearboxDynamics.Gearbox
import GearboxDynamics.GearboxDynamics
--
import PowerUnit.PredictionStrategy
--
import PowerUnit.EPowerUnit.EEngine
import PowerUnit.PowerUnitTypes
import PowerUnit.EPowerUnit.EPowerUnitType
--
import qualified PowerUnit.EPowerUnit.Regen.RTelInputNode
import qualified PowerUnit.EPowerUnit.Regen.RegenEfficiencyModeller as REM
--
import PowerUnit.EPowerUnit.Regen.RTel
import qualified PowerUnit.EPowerUnit.Regen.RegenEfficiencyRetrieval as RER


{-
    Computes the total current energy harvested during
    an entire harvesting routine.
-}
computeTotRegenInDeceleration :: [(Speed, CurrentEnergy, PowerUnitConfig)] -> CurrentEnergy
computeTotRegenInDeceleration [] = WattHour 0
computeTotRegenInDeceleration logs =
    WattHour $ sum
        [ wh
        | (_, _wh, _) <- logs
        , let WattHour wh = currentEnergyToWattHour _wh
        ]

{-
    Attaches to each deceleration sample points its
    expected harvested current.
-}
computeDecelerationRegenCurrentEnergy ::
       Vehicle
    -> [(GearRatio, [((Speed, Time), (Torque, Rev))])]
    -> [(GearRatio, [(Speed, CurrentEnergy, PowerUnitConfig)])]
computeDecelerationRegenCurrentEnergy v grlogs =
    let voltage = (epuVoltage . puEPowerUnit . vehiclePowerUnit) v
     in [ (gr, spd_currs)
        | (gr, logs) <- grlogs
        , logs /= []
        , let spd_currs =
                [ (vSpeed, expCurrEnergy, puConf)
                | ((vSpeed, _), (eTorque, eSpeed)) <- logs
                -- Compute expected current and invert sign.
                , let expCurr = let Ampere x = regenCurrent v eTorque eSpeed in Ampere $ -x
                -- Compute ampere-hour based on length of application.
                , let expCurrCharge = ampsToAmpHour expCurr _DEC_SAMPLING_TIME_GAP
                -- Compute watt hour to enter the battery.
                , let expCurrEnergy = ampHourToWattHour expCurrCharge voltage
                -- Create the power unit configuration of this option.
                , let puConf = PUCPureEEngine gr (EPowerUnitConfig eTorque eSpeed expCurr voltage) 
                ]
        ]

{-
    Computes the regenerated current based on the prediction strategy
    selected for the vehicle's induction motor.
-}
regenCurrent :: Vehicle -> Torque -> Rev -> Current
regenCurrent v =
    case (epuRegenPredictionStrategy . puEPowerUnit . vehiclePowerUnit) v of
        KNN -> regenCurrentKNN v
        REG -> regenCurrentREG v
--
regenCurrentKNN :: Vehicle -> Torque -> Rev -> Current
regenCurrentKNN v _eTorque _eSpeed =
    let knnRegenData = (epuRegenTelPoints . puEPowerUnit . vehiclePowerUnit) v
        ePu = (puEPowerUnit . vehiclePowerUnit) v
        voltage = epuVoltage ePu
        eff = RER.effOf2 (epuKnnConfigRegen ePu) (_eTorque, _eSpeed, vehicleSOC v) knnRegenData
        expCurr = getRegenCurrentWithEff _eTorque _eSpeed voltage eff
     in expCurr
--
regenCurrentREG :: Vehicle -> Torque -> Rev -> Current
regenCurrentREG v _eTorque _eSpeed =
    let regConf = (epuRegenEffConfig . puEPowerUnit . vehiclePowerUnit) v
        voltage = (epuVoltage . puEPowerUnit . vehiclePowerUnit) v
        expCurr = REM.predict regConf (REM.RQuery _eTorque _eSpeed (vehicleSOC v)) voltage
    in expCurr

{-
    Computes the total current energy utilized byt the given
    induction motor configuration.
-}
currentEnergyBurned :: EPowerUnitConfig -> Time -> CurrentEnergy
currentEnergyBurned epuConfig _tApp =
    let -- Extract current and voltage of configuration.
        current = epucCurrent epuConfig
        eVoltage = epucVoltage epuConfig
        --
        -- Compute ampere-hour based on length of application.
        currCharge = ampsToAmpHour current _tApp
        -- Compute watt hour to enter the battery.
        currEnergyBurned = ampHourToWattHour currCharge eVoltage
        --
     in currEnergyBurned