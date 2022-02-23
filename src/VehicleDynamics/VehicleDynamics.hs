{-
    Encapsulates data type and functions for the computation
    of vehicle's power factor and configurations in an itinerary.
    It is also responsible for the conversion of an itinerary into
    a processed itinerary; aids acceleration, deceleration and
    steady pace configurations.
-}
module VehicleDynamics.VehicleDynamics where
--
import Commons.ISUnits
import Commons.Utilities
--
import VehicleDynamics.AeroDrag
import VehicleDynamics.RoadLoad
import VehicleDynamics.VehiclePhysics
--
import Vehicle.Vehicle
import Vehicle.VehicleState
--
import Itinerary.Itinerary
import Itinerary.ProcessedItinerary
--
import GearboxDynamics.Gearbox
import GearboxDynamics.GearboxDynamics
--
import PowerUnit.PowerUnitTypes
import PowerUnit.PowerUnit
import PowerUnit.PowerUnitCostCalculator
--
import PowerUnit.ICPowerUnit.ICPowerUnitType
import PowerUnit.ICPowerUnit.Fuel.Fuel
--
import PowerUnit.EPowerUnit.EPowerUnit
--
import Itinerary.Parser.MainParser
--
import qualified PowerUnit.ICPowerUnit.ICPowerUnitEfficiencyModeller as ICPUEM
--
import Data.List
--
import Debug.Trace


-- Constants.
--
_CORNERING_UPPER_BOUND_LATERAL_GS = 0.250 * _EARTH_GRAVITY_ACCELERATION
--
_ACCELERATION_RATE_LONG_G = LonG 0.15
_ACCELERATION_TOLL_RATE_LONG_G = LonG 0.025
_ACCELERATION_MIN_DIST_AFTER = Meter 200
--
_DECELERATION_RATE_LONG_G = LonG 0.35
--
_POWER_NEGLIGIBLE_DIFFERENCE = KWatt 5

{-
    Evaluates the needs for a vehicle to increase its speed.
-}
needsAcceleration :: Speed -> RoadUnit -> Bool
needsAcceleration = isChangeOfSpeedInDir (<)

{-
    Evaluates the needs for a vehicle to decrease its speed.
-}
needsDeceleration :: Speed -> RoadUnit -> Bool
needsDeceleration = isChangeOfSpeedInDir (>)

{-
    Evaluates if the speed difference in some direction is
    significant.
-}
isChangeOfSpeedInDir :: (Double -> Double -> Bool) -> Speed -> RoadUnit -> Bool
isChangeOfSpeedInDir f _v ru =
    let MeterSecond v = fSpeedToMeterSecond _v
        MeterSecond vRu = fSpeedToMeterSecond $ ruSpeed ru
        --
        isNegSpeedChange = isNegligibleSpeedChange (MeterSecond v) (MeterSecond vRu)
        --
        dec = f v vRu && not isNegSpeedChange
        --
     in dec

{-
    Of each road unit in the itinerary, adjusts its speed
    requirements based on its corner radius and the maximum
    power output of the vehicle.
-}
adjustSpeedOnPhysical ::
       Vehicle
    -> Itinerary
    -> Itinerary
adjustSpeedOnPhysical _ [] = []
adjustSpeedOnPhysical v (ru:rus)
    -- If corner speed is to high -> set it to max acceptable and restart.
    | not $ isAcceptableCornerSpeed ru = adjustSpeedOnPhysical v ((ru{ruSpeed  = speedForCentripetalAcc ru}):rus)
    -- If speed required is too high -> reduce to performable level.
    | not $ isAcceptablePowerDemand v ru = adjustSpeedOnPhysical v ((ru{ruSpeed  = reducedSpeedForPower v ru}):rus)
    -- Otherwise configuration is acceptable.
    | otherwise = ru : adjustSpeedOnPhysical v rus
    --
    where
        isAcceptableCornerSpeed :: RoadUnit -> Bool
        isAcceptableCornerSpeed ru =
            let Meter r = fLengthToMeter $ ruRadius ru
                MeterSecond v = fSpeedToMeterSecond $ ruSpeed  ru
                --
                lateralGs = if r > 0 then (v ^ 2) / r else 0
             in lateralGs <= _CORNERING_UPPER_BOUND_LATERAL_GS
        --  
        speedForCentripetalAcc :: RoadUnit -> Speed
        speedForCentripetalAcc ru =
            let Meter r = fLengthToMeter $ ruRadius ru
             in MeterSecond $ sqrt $ _CORNERING_UPPER_BOUND_LATERAL_GS * r
        --
        --
        isAcceptablePowerDemand :: Vehicle -> RoadUnit -> Bool
        isAcceptablePowerDemand v ru =
            let KWatt maxPow = fPowerToKWatt $ puConservativeMaxPower $ vehiclePowerUnit v
                KWatt reqPow = fPowerToKWatt $ totalRoadLoadPower v ru
             in reqPow <= maxPow
        --
        reducedSpeedForPower :: Vehicle -> RoadUnit -> Speed
        reducedSpeedForPower v ru =
            let MeterSecond vSpeed = fSpeedToMeterSecond $ ruSpeed  ru
             in if isAcceptablePowerDemand v ru{ruSpeed  = MeterSecond vSpeed}
                    then MeterSecond vSpeed
                    else reducedSpeedForPower v ru{ruSpeed  = MeterSecond $ vSpeed - 0.1}

{-
    Computes how much distance is required for the vehicle
    to complete a smooth deceleration between two speeds.
-}
neededDistanceForDeceleration :: Speed -> Speed -> (Length, Time)
neededDistanceForDeceleration _v0 _v1 =
    let dRate = let LonG x = _DECELERATION_RATE_LONG_G in - x * _EARTH_GRAVITY_ACCELERATION -- Meter/Second2
        -- Deceleration time.
        MeterSecond v0 = fSpeedToMeterSecond _v0
        MeterSecond v1 = fSpeedToMeterSecond _v1
        --
        Seconds dTime = Seconds $ (v1 - v0) / dRate
        -- Deceleration distance.
        Meter dDist = Meter $ (v0*dTime) + 0.5*dRate*(dTime^2)
        --
     in (Meter dDist, Seconds dTime)

{-
    Unifies sequenced road units where change in power demand is negligible.
    The level of negligence is controlled by the constant @_POWER_NEGLIGIBLE_DIFFERENCE,
    at the head of this module.
-}
compressItinerary ::
       Vehicle
    -> Itinerary
    -> Itinerary
compressItinerary _ [] = []
compressItinerary _ [l] = [l] -- Has been compressed previous round.
--
compressItinerary v (ru0:ru1:rus) =
    let pragmas =
            [ isNegligibleSpeedChange (ruSpeed  ru0) (ruSpeed  ru1)
            , isNegligiblePowerDemandChange v ru0 ru1
            --, isNegligentAmbientConditionChange v ru0 ru1
            ]
     in if and pragmas
        -- Then the two can be merged.
        then
            let -- Compute new length.
                newLength = Meter $ sum $ map (\x -> let Meter l = fLengthToMeter $ ruLength  x in l) [ru0, ru1]
                --
                newRuRepl = ru0{ruLength  = newLength}
                --
             -- Continue from this new replacement.
             in compressItinerary v (newRuRepl:rus)
        -- Otherwise ru0 cannot be merged -> continue from ru1.
        else ru0 : compressItinerary v (ru1:rus)
        
    --
    where
        isNegligiblePowerDemandChange :: Vehicle -> RoadUnit -> RoadUnit -> Bool
        isNegligiblePowerDemandChange v ru0 ru1 =
            let KWatt pw0 = fPowerToKWatt $ totalRoadLoadPower v ru0
                KWatt pw1 = fPowerToKWatt $ totalRoadLoadPower v ru1
                --
                KWatt tol = fPowerToKWatt _POWER_NEGLIGIBLE_DIFFERENCE
             in abs (pw0 - pw1) <= tol

{-
    Finds next unit with lower speed than vehicle's state speed,
    returns true if the there exists enough space to stop the vehicle,
    false otherwise.
-}
causesImpossibleDeceleration ::
       Vehicle
    -> Itinerary
    -> Bool
causesImpossibleDeceleration v it =
    let MeterSecond vSpeed = fSpeedToMeterSecond $ vehicleStateSpeed $ vehicleState v
        -- If there is a next deceleration, does the vehicle have enough space to halt?
     in case nextSignificantDeceleration (MeterSecond vSpeed) it (Meter 0) of
            -- There is a deceleration to perform.
            (Just (_newV, _distToNewV)) ->
                let MeterSecond newV = fSpeedToMeterSecond _newV
                    Meter distToNewV= fLengthToMeter _distToNewV
                    --
                    (_dDist, _) = neededDistanceForDeceleration (MeterSecond vSpeed) (MeterSecond newV)
                    Meter dDist = fLengthToMeter _dDist
                    --
                 in dDist <= distToNewV
            -- No immediate decelerations.
            Nothing -> False
    --
    where
        -- Returns the speed and distance to it of the next closest deceleration.
        -- Nothing if there is not a next closest deceleration (there is an acceleration first).
        nextSignificantDeceleration :: Speed -> Itinerary -> Length -> Maybe (Speed, Length)
        nextSignificantDeceleration _ [] _ = Nothing
        nextSignificantDeceleration _v (ru:rus) _currDist
            | needsAcceleration _v ru = Nothing
            | needsDeceleration _v ru = Just (ruSpeed ru, _currDist)
            | otherwise = 
                let Meter currDist = fLengthToMeter _currDist
                    Meter thisRuDist = fLengthToMeter $ ruLength ru
                in nextSignificantDeceleration _v rus (Meter $ currDist + thisRuDist)

{-
    Modifies the itinerary to include acceleration sections.
    The target time and space for acceleration are those
    achieved by the provided power unit mode (this should be
    pure internal combustion).
-}
smoothAcceleration :: Vehicle -> PowerUnitMode -> Itinerary -> Maybe Itinerary
smoothAcceleration _ _ [] = Just []
smoothAcceleration v puMode (ru:rus)
    -- If no acceleration is required (or a deceleration is required) -> skip and update speed.
    | not $ needsAcceleration (vehicleStateSpeed $ vehicleState v) ru
            = case smoothAcceleration v{vehicleState = (vehicleState v){vehicleStateSpeed = ruSpeed ru}} puMode rus of
                    Just it -> Just $ ru : it
                    Nothing -> Nothing
    --
    -- Otherwise an acceleration is required -> proceed.
    | otherwise =
        case accelerationConfigInRoadUnit v puMode ru of
            Just ((_, _accDist), grChanges, grTel) ->
                let -- Compute required acceleration distance.
                    Meter accDist = fLengthToMeter _accDist
                    -- this means that after accDist distance -> v is travelling at speed of ru, if ru was at least accDist long.
                    (_, finalSpeed) = last grChanges
                    --
                    -- This is the available space in ru.
                    Meter ruDist = fLengthToMeter $ ruLength ru
                    Meter addedDistInCostSpeed = _ACCELERATION_MIN_DIST_AFTER
                --
                in
                -- Base case: ru has enough length for the full acceleration.
                --
                if (ruDist - accDist) >= let Meter x = _ACCELERATION_MIN_DIST_AFTER in x
                then
                    let accRU = ru{ruLength = Meter accDist}
                        ruAfterAcc = ru{ruLength = Meter $ ruDist - accDist}
                        --
                        newVehicleState = (vehicleState v){vehicleStateSpeed = finalSpeed}
                        --
                        in -- If reaching this new speed is a problem for later braking.
                        if causesImpossibleDeceleration v{vehicleState = newVehicleState} rus
                        -- Then skip this acceleration.
                        then case smoothAcceleration v puMode rus of
                                Just it -> Just $ ru : it
                                Nothing -> Nothing
                        -- Otherwise do add the acceleration, and continue.
                        else case smoothAcceleration v{vehicleState = newVehicleState} puMode (ruAfterAcc:rus) of 
                                Just it -> Just $ accRU : it
                                Nothing -> Nothing
                --
                -- Otherwise need to evaluate a partial acceleration if the vehicle is at a stand still,
                -- or maintaining its speed if it is already moving, and the distance of the ru is 'short'.
                --
                else
                    let accDistanceLeft = Meter $ accDist - ruDist + addedDistInCostSpeed
                        speedReachable =
                            let sample =
                                    [ (lenDiff, cs)
                                    | (cs, _, _cd, _, _) <- grTel
                                    , let Meter cd = fLengthToMeter _cd
                                    , cd <= ruDist -- System has to be conservative: we need a distance less of equal to that available.
                                    , let lenDiff = cd - ruDist
                                    ]
                            --
                            in case sample of
                                [] -> vehicleCurrentSpeed v
                                _  -> snd $ minimumBy (cmpOn fst) sample
                    --
                    in
                    -- If road ahead does not decrease in speed too soon: reach speed reachable at the end of this ru.
                    if shouldPursueAcc accDistanceLeft (ruSpeed ru) rus
                    then
                        let -- Consider continuing the acceleration: reach max speed in this space.
                            accRU = ru{ruSpeed = speedReachable}
                            -- Vehicle reaches this speed.
                            newVehicleState = (vehicleState v){vehicleStateSpeed = speedReachable}
                            --
                         in -- If reaching this new speed is a problem for later braking.
                            if causesImpossibleDeceleration v{vehicleState = newVehicleState} rus
                            -- Then skip this acceleration.
                            then case smoothAcceleration v puMode rus of
                                    Just it -> Just $ ru : it
                                    Nothing -> Nothing
                            -- Otherwise do add the partial acceleration, and continue. 
                            else case smoothAcceleration v{vehicleState = newVehicleState} puMode rus of
                                    Just it -> Just $ accRU : it
                                    Nothing -> Nothing
                    --
                    -- Otherwise revert to changing the speed of this road unit to the vehicle's current speed, and continue.
                    else -- The vehicle shall not reach the speed of this ru, but still needs to be moving.
                        let newSpeedForRu = if vehicleCurrentSpeed v == MeterSecond 0
                                            then -- Reach the speed reachable in this short section.
                                            speedReachable
                                            else -- Vehicle maintains its speed.
                                            vehicleCurrentSpeed v
                            --
                            newVehicleState = (vehicleState v){vehicleStateSpeed = newSpeedForRu}
                            --
                         in case smoothAcceleration v{vehicleState = newVehicleState} puMode rus of
                            Just it -> Just $ ru{ruSpeed = newSpeedForRu} : it
                            Nothing -> Nothing
            --
            Nothing -> trace ("CANNOT MODEL: " ++ show ru) Nothing
    where
        shouldPursueAcc :: Length -> Speed -> Itinerary -> Bool
        shouldPursueAcc _ _ [] = False -- No more road!
        shouldPursueAcc _d _v (ru:rus) =
            let Meter d = fLengthToMeter _d
                --
                Meter ruDist = fLengthToMeter $ ruLength ru
                --
             in -- If speed is decreasing entering ru AND the speed difference is significant: then no.
                if (_v > ruSpeed ru) && not (isNegligibleSpeedChange _v (ruSpeed ru))
                then False
                -- Otherwise.
                else
                    -- If there is enough space in ru: then yes.
                    if ruDist - d >= let Meter x = _ACCELERATION_MIN_DIST_AFTER in x
                    then True
                    -- Otherwise only if acceleration can carry on in next rus.
                    else shouldPursueAcc (Meter $ d - ruDist) _v rus

{-
    Modifies the itinerary to include deceleration sections.
-}
smoothDeceleration :: Vehicle -> Itinerary -> Itinerary
smoothDeceleration _ [] = []
smoothDeceleration _ [l] = [l]
smoothDeceleration v (ru0:ru:rus)
    -- If no deceleration is required (or an acceleration is required), analyze forward.
    | not $ needsDeceleration (vehicleStateSpeed $ vehicleState v) ru0 =
        let  -- New vehicle's state.
            newVehicleSpeed = ruSpeed $ if rus /= [] then head rus else ru
            newVehicleState = (vehicleState v){vehicleStateSpeed = newVehicleSpeed}
            --
        in ru0 : smoothDeceleration v{vehicleState = newVehicleState} (ru:rus)
    --
    -- Otherwise an deceleration is required -> proceed.
    | otherwise =
        let -- Current speed.
            MeterSecond cSpeed = fSpeedToMeterSecond $ vehicleStateSpeed $ vehicleState v
            -- Target speed.
            MeterSecond tSpeed = fSpeedToMeterSecond $ ruSpeed ru0
            --
            Meter ruAvaDist = fLengthToMeter $ ruLength ru
            --
            -- Compute distance to required to decelerate in comfort.
            dRate = let LonG x = _DECELERATION_RATE_LONG_G in x * _EARTH_GRAVITY_ACCELERATION -- Meter/Second2
            -- Deceleration time.
            Seconds dTime = Seconds $ - (tSpeed - cSpeed) / dRate
            -- Deceleration distance.
            Meter dDist = Meter $ ((cSpeed + tSpeed) / 2) * dTime
            --
         in -- Base case: in the current ru there is enough space to stop.
            if ruAvaDist >= dDist
            then
                -- Split the current road unit into maintaining speed and start deceleration.
                let ruMaintainSpd = ru{ ruLength = Meter $ ruAvaDist - dDist
                                      , ruSpeed = MeterSecond cSpeed}
                    --
                    ruDeceleration = ru0{ruLength = Meter dDist}
                    --
                    -- New vehicle's state.
                    newVehicleSpeed = ruSpeed $ if rus /= [] then head rus else ruMaintainSpd
                    newVehicleState = (vehicleState v){vehicleStateSpeed = newVehicleSpeed}
                    --
                 in ru0 : ruDeceleration : smoothDeceleration v{vehicleState=newVehicleState} (ruMaintainSpd:rus)
            --
            -- Otherwise: not enough space in this road unit to decelerate.
            else
                -- Continue deceleration in next road unit (which is the previous in the original order).
                let -- Final speed in current ru.
                    ruFinalSpeed = MeterSecond $ sqrt $ (cSpeed^2) + (2*dRate*ruAvaDist)
                    -- Set current ru as deceleration.
                    ruDeceleration = ru{ruSpeed = ruFinalSpeed}
                    -- Update vehicle's state.
                    newVehicleSpeed = ruSpeed $ if rus /= [] then head rus else ruDeceleration
                    newVehicleState = (vehicleState v){vehicleStateSpeed = newVehicleSpeed}
                    --
                 in ru0 : smoothDeceleration v{vehicleState=newVehicleState} (ruDeceleration:rus)

{-
    Removes from an itinerary all 'Halt' sections.
    Careful, this should be performed only after accelerations
    and decelerations have been injected in the itinerary.
-}
removeFullStops :: Itinerary -> Itinerary
removeFullStops rus = [ru | ru <-rus, ru /= getHaltRoadUnit]

{-
    Converts an itinerary to a processed itinerary.
    Careful, this should be performed only after accelerations
    and decelerations have been injected in the itinerary.
-}
bindUnitTaskToItinerary :: Vehicle -> Itinerary -> ProcessedItinerary
bindUnitTaskToItinerary v it
    = inner v it 0
    where
    inner :: Vehicle -> Itinerary -> Int -> ProcessedItinerary
    inner _ [] _ = []
    inner v (ru:rus) n
        | needsAcceleration (vehicleCurrentSpeed v) ru =
            PickPaceRoadUnit n PICK ru : inner (updateVehicleCurrentSpeed v (ruSpeed ru)) rus (n+1)
        --
        | needsDeceleration (vehicleCurrentSpeed v) ru =
            let decConfig = decelerationConfigInRoadUnit v ru
                kwhRegenerated = currentEnergyToKWattHour $ computeTotRegenInDeceleration (snd decConfig)
                --
            in DropRoadUnit n ru decConfig kwhRegenerated : inner (updateVehicleCurrentSpeed v (ruSpeed ru)) rus (n+1)
        --
        | otherwise =
            PickPaceRoadUnit n PACE ru : inner (updateVehicleCurrentSpeed v (ruSpeed ru)) rus (n+1)

{-
    In the right steps converts an itinerary into a
    processed itinerary. This operation fails only
    when the given itinerary includes physically impossible
    requirements from the vehicle's power unit.
-}
processItinerary ::
       Vehicle
    -> Itinerary
    -> Maybe ProcessedItinerary
processItinerary v it =
    do
    let it_opt_physical = (compressItinerary v . adjustSpeedOnPhysical v) it
    --
    it_fix_acc <- smoothAcceleration v PUMPureICEngine it_opt_physical
    --
    -- Update vehicle's state to second to last road unit: speed it will be approaching last item.
    let tmpVehicleState = (vehicleState v){vehicleStateSpeed = ruSpeed $ sndToLast it_fix_acc}
    let rev_it_fix_dec = smoothDeceleration v{vehicleState=tmpVehicleState} (reverse it_fix_acc)
    --
    let it_fix_dec = reverse rev_it_fix_dec
    --
    -- Remove full stops.
    let noFullStopsIt = removeFullStops it_fix_dec
    --
    -- Bind speed change info.
    let processedIt = bindUnitTaskToItinerary (updateVehicleCurrentSpeed v (MeterSecond 0)) noFullStopsIt
    --
    return processedIt

{-
    Interface for the retrieval of the optimal acceleration configuration
    for a selected power unit mode. This operation returns Nothing only
    when the mode selected cannot satisfy the time and space constraints
    of the acceleration.
-}
accelerationConfigInRoadUnit ::
       Vehicle
    -> PowerUnitMode
    -> RoadUnit
    -> Maybe AccConfig
accelerationConfigInRoadUnit v preferredPuMode ru =
    let v0 = vehicleCurrentSpeed v
        v1 = ruSpeed ru
        --
     in case accelerationGearing v preferredPuMode PUMPureICEngine v0 v1 _ACCELERATION_RATE_LONG_G _ACCELERATION_TOLL_RATE_LONG_G ru of
         --
        [] -> Nothing
         --
        allAccGearing ->
            let grVsSpeedPropCostSubUnitConf = computeAccelerationSpeedVsCostPerSecond v ru allAccGearing
                --
                -- Remove the sub unit config for the completion of the acceleration config.
                grVsSpeedAndPropCost =
                    [ (gr, speedsVsCosts)
                    | (gr, speedVsCostVsConfig) <- grVsSpeedPropCostSubUnitConf
                    , not $ null speedVsCostVsConfig
                    , let speedsVsCosts = [(s, c) | (s, c, _) <- speedVsCostVsConfig]
                    ]
                --
                (totTimeDist, gearChanges, logs) = completeAccConfig allAccGearing grVsSpeedAndPropCost
                --
                -- Now attach to the logs, their configurations and cost per second.
                refactoredLogs =
                    [ (vSpeed, cTime, cDist, puConf, propCost)
                    | (gr, vSpeed, cTime, cDist, eTorque, eSpeed) <- logs
                    --
                    -- Join the logs on the gear and speed.
                    , let (subUnitConfig, propCost) = head
                            [ join
                            | (gr', spdCostConf) <- grVsSpeedPropCostSubUnitConf
                            --
                            -- Verify it is the right gear.
                            , gr == gr'
                            --
                            -- Now join on speed, and return the corresponding configuration.
                            , let join = head [(conf, cost) | (spd, cost, conf) <- spdCostConf , vSpeed == spd]
                            ]
                    --
                    , let puConf = subUnitConfigToPowerUnitConfig subUnitConfig gr
                    ]
                --
            in
                let (_, _totDist) = totTimeDist
                    Meter totDist = fLengthToMeter _totDist
                    Meter expDist = fLengthToMeter $ ruLength ru
                in if (totDist <= expDist) || abs (totDist - expDist) <= (0.1*expDist)
                    then 
                        case refactoredLogs of
                            [] -> Nothing
                            _  -> Just (totTimeDist, gearChanges, refactoredLogs)
                else Nothing

{-
    Interface for the retrieval of the optimal deceleration configuration.
-}
decelerationConfigInRoadUnit ::
       Vehicle
    -> RoadUnit
    -> DecConfig
decelerationConfigInRoadUnit v ru =
    let decGearOptions = decelerationGearing v (ruSpeed ru) _DECELERATION_RATE_LONG_G ru
        --
        decGearingWattHours = computeDecelerationRegenCurrentEnergy v decGearOptions
        --
        decConfig = decGearSeq decGearingWattHours
     in decConfig
