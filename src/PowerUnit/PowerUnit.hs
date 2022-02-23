{-
    Encapsulates functions for the modelling of the
    power unit as an aggregation of spark ignition
    engine, induction motor, and vehicle's energy state.
-}
module PowerUnit.PowerUnit where
--
import Commons.ISUnits
import Commons.Currency
--
import PowerUnit.PowerUnitTypes
import PowerUnit.PowerUnitCostCalculator
--
import Vehicle.Vehicle
import Vehicle.VehicleState
--
import Itinerary.Itinerary
import GearboxDynamics.Gearbox
import GearboxDynamics.GearboxDynamics
--
import PowerUnit.ICPowerUnit.ICPowerUnit
import PowerUnit.ICPowerUnit.ICPowerUnitType
import PowerUnit.ICPowerUnit.ICEngine
import PowerUnit.ICPowerUnit.Fuel.Fuel
--
import PowerUnit.EPowerUnit.EPowerUnit
import PowerUnit.EPowerUnit.Electricity
--
import Text.Printf (printf)
import Data.List
import Data.Maybe
--
import Debug.Trace


{-|
    Takes a vehicle, a road unit the PU mode and a set of gear options,
    returns the power unit configuration with the least cost, if there exists
    a valid config.
-}
steadyPaceCost ::
       Vehicle
    -> PowerUnitMode
    -> RoadUnit
    -> Maybe (PowerUnitConfig, Currency)
steadyPaceCost v puMode ru =
    inner v puMode ru (gearsSteadyCourse v puMode ru)
    where
    inner ::
        Vehicle
        -> PowerUnitMode
        -> RoadUnit
        -> [(GearRatio, Torque, Rev)] -- Gear Options.
        -> Maybe (PowerUnitConfig, Currency)
    --
    inner _ _ _ [] = Nothing
    --
    inner v puMode ru grOpts =
        let vSpeed = ruSpeed ru
            ambient = acTemp $ ruAmbient ru
            --
            evalOpts =
                [ (cost, subConf, gr)
                | (gr, eTorque, eSpeed) <- grOpts
                , let (cost, subConf) = propulsionCostPerSecondOfConfig v vSpeed puMode (PowerUnitDeliveryRequirement eTorque eSpeed ambient)
                ]
        in case evalOpts of
            [] -> Nothing
            _  ->
                let -- Get config that leads to minimum cost.
                    (PropulsionCostPerSecond costPerSec, cubConf, gr) =
                        minimumBy
                            (\(PropulsionCostPerSecond c, _, _) (PropulsionCostPerSecond c', _, _) -> compare c c')
                            evalOpts
                    --
                    -- Build the PowerUnitConfig.
                    puConfig = subUnitConfigToPowerUnitConfig cubConf gr
                    --
                    -- Multiply cost per second with time of application to get total cost of this road unit.
                    totCost =
                        let Seconds tApp = fTimeToSeconds $ timeOfRoadUnit ru
                        in costPerSec * fromRational (toRational tApp)
                    --
                 in Just (puConfig, totCost)

{-
    Computes the total cost of an acceleration.
-}
accelerationCost :: [(Speed, Time, Length, PowerUnitConfig, PropulsionCostPerSecond)] -> Currency
accelerationCost [] = 0
accelerationCost ((_, _, _, _, propCostPerSec):xs) =
    let PropulsionCostPerSecond currPerSec = propCostPerSec
        Seconds tApp = fTimeToSeconds _ACC_SAMPLING_TIME_GAP
        cost = currPerSec * fromRational (toRational tApp)
     in cost + accelerationCost xs

{-
    Converts a fuel mass and current energy utilized to a total cost
    of operation.
-}
energyCost :: Vehicle -> (Mass, CurrentEnergy) -> Currency
energyCost v (fuelReq, currReq) =
    let fuelCost = fuelMassCost (vehicleFuelUnitCostPerLiter v) fuelReq
        elecCost = currentEnergyCost (vehicleElectricityCostPerKWattHour v) currReq
     in fuelCost + elecCost

{-
    Of an acceleration sampling points, attaches to each point the costs of running both
    engines in the modes selected.
-}
computeAccelerationSpeedVsCostPerSecond ::
       Vehicle
    -> RoadUnit
    -> [(GearRatio, [((Speed, Time), (Torque, Rev), PowerUnitMode)])]
    -> [(GearRatio, [(Speed, PropulsionCostPerSecond, SubUnitConfiguration)])]
computeAccelerationSpeedVsCostPerSecond v ru perGrSpecs =
    let ambient = acTemp $ ruAmbient ru
        --
        grVsSpeedPropCostSubPuConf =
            [ (gr, speedVsPropCostVsSubPuConf)
            | (gr, tels) <- perGrSpecs
            , let speedVsPropCostVsSubPuConf =
                    [ (vSpeed, propConsumption, subPuConfig)
                    | ((vSpeed, date), (eTorque, eSpeed), puMode) <- tels
                    , let (propConsumption, subPuConfig) = propulsionCostPerSecondOfConfig v vSpeed puMode (PowerUnitDeliveryRequirement eTorque eSpeed ambient)
                    ]
            ]
        --
     in grVsSpeedPropCostSubPuConf

{-
    Computes the total amount of octane and current energy for the
    execution of a power unit configuration for some given amount
    of time.
-}
getPaceEnergyReq :: PowerUnitConfig -> Time -> (Mass, CurrentEnergy)
--
getPaceEnergyReq (PUCPureICEngine _ icpuConfig) tApp =
    let kgPetrolBurned = fMassToKg $ octaneMassBurned icpuConfig tApp
     in (kgPetrolBurned, KWattHour 0)
--
getPaceEnergyReq (PUCPureEEngine _ epuConfig) tApp =
    let kwhBurned = currentEnergyToKWattHour $ currentEnergyBurned epuConfig tApp
     in (Kg 0, kwhBurned)
--
getPaceEnergyReq (PUCFullECompensation gr icpuConfig epuConfig) tApp =
    getPaceEnergyReq (PUCCombined (-1) gr icpuConfig epuConfig) tApp
--
getPaceEnergyReq (PUCCombined _ gr icpuConfig epuConfig) tApp =
    let -- Get new state for operation of ICEngine: change in fuel mass available only.
        (fuelNeeded, _) = getPaceEnergyReq (PUCPureICEngine gr icpuConfig) tApp
        (_, currNeeded)  = getPaceEnergyReq (PUCPureEEngine gr epuConfig ) tApp
        --
     in (fuelNeeded, currNeeded)

{-
    Computes the total amount of octane and current energy for the
    execution of a acceleration configuration: a sequence of power
    unit modes applied every _ACC_SAMPLING_TIME_GAP units of time.
-}
getAccEnergyReq :: [PowerUnitConfig] -> (Mass, CurrentEnergy)
getAccEnergyReq =
    foldl
        (\acc puc ->
            summEnergies acc $ getPaceEnergyReq puc _ACC_SAMPLING_TIME_GAP
        )
        (Kg 0, KWattHour 0)
    --
    where
    summEnergies :: (Mass, CurrentEnergy) -> (Mass, CurrentEnergy) -> (Mass, CurrentEnergy)
    summEnergies (_fm, _ce) (_fm', _ce') =
        let Kg fm  = fMassToKg _fm
            Kg fm' = fMassToKg _fm'
            --
            KWattHour ce  = currentEnergyToKWattHour _ce
            KWattHour ce' = currentEnergyToKWattHour _ce'
            --
         in (Kg (fm + fm'), KWattHour (ce + ce'))

{-
    Computes the vehicle's new energy state after some amount of octane and current energy
    have been utilized. Returns Nothing if either energy source exceeds the vehicle's
    availability.
-}
getVehicleResultingEnergyState :: VehicleState -> (Mass, CurrentEnergy) -> Maybe VehicleState
getVehicleResultingEnergyState vState (fuelMassBurned, currentBurned) =
    let -- Mass in the tank now.
        Kg kgPetrolInTank = fMassToKg $ vehicleStateFuelMass vState
        -- Mass to be burned.
        Kg kgPetrolBurned = fMassToKg fuelMassBurned
        -- Change in petrol available.
        newKgPetrolInTank = Kg $ kgPetrolInTank - kgPetrolBurned
        --
     in if kgPetrolInTank >= kgPetrolBurned
        then
            let -- Current Energy available now.
                KWattHour kwhInBattNow = currentEnergyToKWattHour $ vehicleStateChargeNow vState
                -- Current Energy required.
                KWattHour kwhBurned = currentEnergyToKWattHour currentBurned
                --
                newKwhInBatt = KWattHour $ kwhInBattNow - kwhBurned
                --
             in if kwhInBattNow >= kwhBurned
                then Just vState{ vehicleStateFuelMass = newKgPetrolInTank
                                , vehicleStateChargeNow = newKwhInBatt}
                else Nothing
        else Nothing

{-
    Computes the vehicle's new energy state after applying a power unit configuration
    for some given amount of time.
-}
getPaceResultingVehicleEnergyState :: VehicleState -> PowerUnitConfig -> Time -> Maybe VehicleState
getPaceResultingVehicleEnergyState vState puc tApp =
    let resReq = getPaceEnergyReq puc tApp
     in getVehicleResultingEnergyState vState resReq

{-
    Computes the vehicle's new energy state after completing an acceleration described as an
    application fo power unit configurations for _ACC_SAMPLING_TIME_GAP units of time.
-}
getPickResultingVehicleEnergyState :: VehicleState -> [PowerUnitConfig] -> Maybe VehicleState
getPickResultingVehicleEnergyState vState pucs =
    let resReq = getAccEnergyReq pucs
     in getVehicleResultingEnergyState vState resReq

{-
    Computes the vehicle's new energy state after the completion of a energy harvesting procedure.
-}
getDropResultingVehicleEnergyState :: VehicleState -> [(Speed, CurrentEnergy, PowerUnitConfig)] -> VehicleState
getDropResultingVehicleEnergyState vState decConfig = 
    let kwhRegenerated = currentEnergyToKWattHour $ computeTotRegenInDeceleration decConfig
     in getDropResultingVehicleEnergyStateWithKnownMaxRegen vState kwhRegenerated

{-
    Computes the vehicle's new energy state after the completion of a energy harvesting procedure,
    of which configuration has already been computed.
-}
getDropResultingVehicleEnergyStateWithKnownMaxRegen :: VehicleState -> CurrentEnergy -> VehicleState
getDropResultingVehicleEnergyStateWithKnownMaxRegen vState maxRegen =
    let KWattHour kwhInBattNow = currentEnergyToKWattHour $ vehicleStateChargeNow vState
        KWattHour kwhInBattMax = currentEnergyToKWattHour $ vehicleStateChargeMax vState
        --
        KWattHour kwhRegenerated = currentEnergyToKWattHour maxRegen
        --
        newKwhInBatt = KWattHour $ min kwhInBattMax (kwhInBattNow + kwhRegenerated)
        --
     in vState{vehicleStateChargeNow = newKwhInBatt}
