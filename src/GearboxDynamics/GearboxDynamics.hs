{-# LANGUAGE CPP #-}
{-
    Models the dynamics of a gearbox in the context
    of a vehicle and road. It encapsulates functions
    for the computation fo steady pace and acceleration
    gearing, required engine torque and speed, wheel torque,
    wheel speed, cost of running the power unit in certain
    modes per gear, ...
-}
module GearboxDynamics.GearboxDynamics where
--
import qualified Data.Heap as Heap
import Data.List
import Data.Hashable
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Lazy as HashMap
--
import Commons.ISUnits
import Commons.Currency
import Commons.Utilities
--
import Vehicle.Vehicle
import PowerUnit.PowerUnitTypes
import PowerUnit.ICPowerUnit.ICPowerUnitType
import PowerUnit.EPowerUnit.EPowerUnitType
import PowerUnit.ICPowerUnit.Fuel.Fuel
--
import Itinerary.Itinerary
--
import GearboxDynamics.Gearbox
import VehicleDynamics.AeroDrag
import VehicleDynamics.RoadLoad
import VehicleDynamics.VehiclePhysics
--
import PowerUnit.PowerUnitCostCalculator
--
import Debug.Trace

{-# ANN module "HLint: ignore Reduce duplication" #-}


-- Acceleration and deceleration, time sampling
-- constants.
_ACC_SAMPLING_TIME_GAP = Seconds 0.05
_DEC_SAMPLING_TIME_GAP = Seconds 0.05

{-
    Encapsulates the type of an acceleration gear shift:
    a gear ratio, selected at some speed with a attributed cost per unit
    second.
-}
data GearingShiftNode = GearingShiftNode GearRatio Speed PropulsionCostPerSecond deriving (Show, Eq)
--
instance Hashable GearingShiftNode where
    hash (GearingShiftNode (GRImp gr) spd c) =
        let MeterSecond spdVal = fSpeedToMeterSecond spd
            cVal = applyCurrencyExchange $ currEx $ pcpsCost c
         in hash $ show gr ++ show spdVal ++ show cVal
    --
    hashWithSalt salt gsn = hashWithSalt salt (hash gsn)

{-
    Encapsulates the type of a deceleration gear shift:
    a gear ratio, selected at some speed with a current generated.
-}
data BrakingShiftNode = BrakingShiftNode GearRatio Speed CurrentEnergy deriving (Show, Eq)
--
instance Hashable BrakingShiftNode where
    hash (BrakingShiftNode (GRImp gr) spd c) =
        let MeterSecond spdVal = fSpeedToMeterSecond spd
            KWattHour cVal = currentEnergyToKWattHour c
         in hash $ show gr ++ show spdVal ++ show cVal
    --
    hashWithSalt salt gsn = hashWithSalt salt (hash gsn)

{-
    Computes the circumference of a wheel.
-}
wheelCircum :: Length -> Length
wheelCircum (Meter r) = Meter $ 2 * pi * r
wheelCircum r = wheelCircum $ fLengthToMeter r

{-
    Computes the angular speed of a wheel.
-}
wheelSpeed :: Speed -> Length -> Rev
wheelSpeed (MeterSecond v) wRadius =
    let Meter c = wheelCircum wRadius
     in HZ $ v / c
wheelSpeed v l = wheelSpeed (fSpeedToMeterSecond v) l

{-
    Computes the land speed of a vehicle given the
    angular speed of a wheel.
-}
vehicleSpeed :: Rev -> Length -> Speed
vehicleSpeed (HZ wRevs) wRadius =
    let Meter c = wheelCircum wRadius
     in MeterSecond $ wRevs * c
vehicleSpeed wRevs wRadius = vehicleSpeed (fRevToHZ wRevs) wRadius

{-
    Computes all required engine torque end engine speed required
    by each gear to keep the vehicle moving at the road unit's speed.
-}
gearsSteadyCourse :: Vehicle -> PowerUnitMode -> RoadUnit -> [(GearRatio, Torque, Rev)]
gearsSteadyCourse v puMode ru =
    let -- Constants.
        diff = vehicleDiff v
        -- Required force.
        (wTorque, wSpeed) = steadyPaceWheelReq v ru
        --
        -- Engine requirements for all gears.
        tmp =
            [ (g, t, s)
            | g <- vehicleGearbox v
            --
            -- Compute required engine speed.
            , let s = fRevsInSeq wSpeed [g, diff]
            -- Verify this is feasible engine speed.
            , isValidGearForOutputSpeed v puMode g (ruSpeed ru) == EQ
            --
            -- Compute required engine torque.
            , let t = fTorqueInSeq wTorque [g, diff]
            -- Verify this is feasible torque output.
            , isValidGearForOutputTorque v puMode s t
            ]
        --
     in tmp

{-
    Given a sequence of gear changes at some speed, reduces the list
    to only contain the gear changes.
    eg: [(1, 10kph), (1, 15kph), (2, 15kph)] = [(1, 10kph), (2, 15kph)]
-}
summarizeGearing :: [(GearRatio, Speed)] -> [(GearRatio, Speed)]
summarizeGearing all = inner all (head all)
            where
                inner :: [(GearRatio, Speed)] -> (GearRatio, Speed) -> [(GearRatio, Speed)]
                inner [] _ = []
                inner [l] curr = [curr, l]
                inner ((gr, speed):xs) (currRatio, currSpeed)
                    | gr == currRatio = inner xs (currRatio, currSpeed)
                    | gr /= currRatio = (currRatio, currSpeed) : (currRatio, speed) : inner xs (gr, speed)

{-
    Computes the maximum and minimum engine speed for a given power unit mode.
-}
powerUnitRevRange :: Vehicle -> PowerUnitMode -> (Rev, Rev)
--
powerUnitRevRange v PUMPureICEngine =
    let icPU = vehicleICPowerUnit v
     in (icpuLowerRevLimiter icPU, icpuUpperRevLimiter icPU)
--
powerUnitRevRange v PUMPureEEngine =
    let ePU = vehicleEPowerUnit v
     in (epuLowerRevLimiter ePU, epuUpperRevLimiter ePU)
--
powerUnitRevRange v _ = -- PUMCombined || PUMFullECompensation
    let (minE, maxE)  = powerUnitRevRange v PUMPureEEngine
        (minIC, maxIC) = powerUnitRevRange v PUMPureICEngine
        --
        minR = max minE minIC
        maxR = min maxE maxIC
     in (minR, maxR)

{-
    Computes the slowest the vehicle must be moving when selecting
    a given gear in some given power unit mode.
-}
gearMinSpeed :: Vehicle -> PowerUnitMode -> GearRatio -> Speed
gearMinSpeed v puMode gr =
    let transmission = [gr, vehicleDiff v]
        Meter wRadius = fLengthToMeter $ vehicleWheelRadius v
        --
        (minR, _) = powerUnitRevRange v puMode
        HZ lowerRevLimit = fRevToHZ minR
        --
        HZ wSpeed = fRevsOutSeq (HZ lowerRevLimit) transmission
        --
        minSpeed = MeterSecond $ wSpeed * 2 * pi * wRadius
     in minSpeed

{-
    Computes the fastest the vehicle can move when selecting
    a given gear in some given power unit mode.
-}
gearMaxSpeed :: Vehicle -> PowerUnitMode -> GearRatio -> Speed
gearMaxSpeed v puMode gr =
    let transmission = [gr, vehicleDiff v]
        Meter wRadius = fLengthToMeter $ vehicleWheelRadius v
        --
        (_, maxR) = powerUnitRevRange v puMode
        upperRevLimit = fRevToHZ maxR
        --
        HZ wSpeed = fRevsOutSeq upperRevLimit transmission
        --
        maxSpeed = MeterSecond $ wSpeed * 2 * pi * wRadius
     in maxSpeed

{-
    Evaluates if the selected gear is too high, too low or valid for running
    the vehicle at a certain land speed with the power unit in a certain mode.
    It returns EQ if the gear is valid, GR it requires the power unit to under-spool,
    or LT if requires the power unit to over-spool.
-}
isValidGearForOutputSpeed :: Vehicle -> PowerUnitMode -> GearRatio -> Speed -> Ordering
isValidGearForOutputSpeed v puMode gear vSpeed =
    let HZ wheelRevs = wheelSpeed vSpeed (vehicleWheelRadius v)
        HZ engineSpeedReq = fRevsInSeq (HZ wheelRevs) [gear, vehicleDiff v]
        --
        (minR, maxR) = powerUnitRevRange v puMode
        --
        HZ lowerRevLimit = fRevToHZ minR
        HZ upperRevLimit = fRevToHZ maxR
        --
     in if inRange engineSpeedReq lowerRevLimit upperRevLimit then EQ
        else if engineSpeedReq < lowerRevLimit then LT
        else GT -- case: engineSpeedReq > upperRevLimit


{-
    Evaluates if the power unit in the selected mode can deliver the demanded torque.
-}
isValidGearForOutputTorque :: Vehicle -> PowerUnitMode -> Rev -> Torque -> Bool
isValidGearForOutputTorque v puMode eSpeed eTorque = powerUnitMaxTorque v puMode eSpeed >= eTorque

{-
    Computes the maximum torque the power unit can output when
    operating in a given mode.
-}
powerUnitMaxTorque :: Vehicle -> PowerUnitMode -> Rev -> Torque
--
powerUnitMaxTorque v PUMPureEEngine eSpeed =
    let maxPow = epuMaxPower $ vehicleEPowerUnit v
     in fMaxTorqueForRev maxPow eSpeed
--
powerUnitMaxTorque v PUMPureICEngine eSpeed =
    let maxPow = icpuMaxPower $ vehicleICPowerUnit v
     in fMaxTorqueForRev maxPow eSpeed
--
powerUnitMaxTorque v (PUMCombined _) eSpeed =
    let Nm icpuMax = powerUnitMaxTorque v PUMPureICEngine eSpeed
        Nm epuMax = powerUnitMaxTorque v PUMPureEEngine eSpeed
     in Nm $ icpuMax + epuMax
--
powerUnitMaxTorque v PUMFullECompensation eSpeed =
    let Nm icpuMax = powerUnitMaxTorque v PUMPureICEngine eSpeed 
        Nm epuMax = powerUnitMaxTorque v PUMPureEEngine eSpeed 
     in Nm $ icpuMax + epuMax

{-
    Computes a sequence of engine requirements to allow the vehicle to
    perform an acceleration in some road unit and some preferred power
    unit mode.
    @Vehicle: vehicle which performs the acceleration.
    @PowerUnitMode: preferred power unit mode.
    @PowerUnitMode: backup power unit mode.
    @Speed: initial speed.
    @Speed: final speed.
    @LonG: target acceleration rate.
    @LonG: tolerance acceleration rate.
    @RoadUnit: road unit in which to perform the acceleration

    @returns: a list of sample point per gear. A sample point contains:
                - speed reached
                - time speed was reached
                - torque required to move to next sample point.
                - engine speed required to reach next sample point
                - power unit mode selected
-}
accelerationGearing ::
       Vehicle
    -> PowerUnitMode
    -> PowerUnitMode
    -> Speed
    -> Speed
    -> LonG
    -> LonG
    -> RoadUnit
    -> [(GearRatio, [((Speed, Time), (Torque, Rev), PowerUnitMode)])]
--
accelerationGearing v preferredPuMode backupPuMode vNow vTarget lg lgToll ru =
    case [(gr, logs) | gr <- vehicleGearbox v, let logs = calcForGear gr, logs /= []] of
        [] -> []
        stagedRes ->
            let --
                -- Verify the final speed is met.
                MeterSecond maxSpeed =
                    fSpeedToMeterSecond $ maximum
                    [ locMax
                    | (_, logs) <- stagedRes
                    , let locMax = maximum (map (\((spd, _), _, _) -> spd) logs)
                    ]
                --
                MeterSecond expSpeed = fSpeedToMeterSecond vTarget
                --
            in stagedRes
    where
    calcForGear gr = gearAcceleration v preferredPuMode (preferredPuMode, backupPuMode) gr vNow (Seconds 0) vTarget lg lgToll ru
    --
    -- | Iteratively samples engine requirements for a single gear.
    gearAcceleration ::
           Vehicle
        -> PowerUnitMode
        -> (PowerUnitMode, PowerUnitMode)
        -> GearRatio
        -> Speed
        -> Time
        -> Speed
        -> LonG
        -> LonG
        -> RoadUnit
        -> [((Speed, Time), (Torque, Rev), PowerUnitMode)]
    gearAcceleration v currPuMode (preferredPuMode, backupPuMode) gear (MeterSecond vi) (Seconds ti) (MeterSecond vTarget) (LonG lg) (LonG lgToll) ru
        -- Check speed is reached.
        | vi > vTarget = []
        -- iff gear is too LOW for current speed -> stop: gear is exhausted.
        | isValidGearForOutputSpeed v currPuMode gear (MeterSecond vi) == GT = []
        -- iff gear is too HIGH for current speed -> restart with gear's minimum speed, if this is in range.
        | isValidGearForOutputSpeed v currPuMode gear (MeterSecond vi) == LT =
            let gMinSpd = gearMinSpeed v currPuMode gear
             in if gMinSpd > MeterSecond vi && gMinSpd <= MeterSecond vTarget
                then gearAcceleration v currPuMode (preferredPuMode, backupPuMode) gear gMinSpd (Seconds ti) (MeterSecond vTarget) (LonG lg) (LonG lgToll) ru
                else []
        --
        | otherwise =
            let -- Needed Constants:
                Meter wRadius = fLengthToMeter $ vehicleWheelRadius v
                Kg vMass = fMassToKg $ vehicleMass v
                transmission = [gear, vehicleDiff v]
                Seconds tGaps = fTimeToSeconds _ACC_SAMPLING_TIME_GAP
                --
                -- Engine speed required.
                HZ eSpeed =
                    let HZ wheelRevs = wheelSpeed (MeterSecond vi) (vehicleWheelRadius v)
                     in fRevsInSeq (HZ wheelRevs) transmission
                --
                -- Calculate new speed reached at time ti + tGaps.
                -- Force to overcome for vehicle in motion at ti with speed vi.
                Newton fOpp =
                    let Newton aerof = roadFriction2 v ru{ruSpeed = MeterSecond vi}
                        Newton roadf = aeroDrag2 v ru{ruSpeed = MeterSecond vi}
                     in Newton $ aerof + roadf
                --
                -- Calculate force for acceleration as for lg, and tolerance lgToll.
                MeterSecond2 rAcc = MeterSecond2 $ _EARTH_GRAVITY_ACCELERATION * lg
                MeterSecond2 tollAcc = MeterSecond2 $ _EARTH_GRAVITY_ACCELERATION * lgToll
                --
                Newton fAcc = Newton $ vMass * rAcc
                --
                -- Calculate force to overcome frictions and cause required acceleration.
                Newton fReq = Newton $ fOpp + fAcc
                --
                -- Calculate torque on wheels to produce fReq.
                Nm tWheels = Nm $ fReq * wRadius
                --
                -- Torque applicable to the wheels.
                --
                (Nm tEngine, puMode) =
                    let desiredEngineTorque = fTorqueInSeq (Nm tWheels) transmission
                     in biasedPuModeForTorque v (preferredPuMode, backupPuMode) (HZ eSpeed) desiredEngineTorque
                --
                Nm tApplicable = fTorqueOutSeq (Nm tEngine) transmission
                --
                --
                -- Calculate speed at ti + tGap applying tWheels.
                --
                -- Force applied (given by t).
                Newton fApplied = Newton $ tApplicable / wRadius
                -- Force resultant for acceleration.
                Newton fResultantAcc = Newton $ fApplied - fOpp
                --
                -- Acceleration given by fApplied.
                acc = fResultantAcc / vMass
                --
                -- Speed at ti + tGap
                MeterSecond newVi = MeterSecond $ (acc * tGaps) + vi
                --
                in
                if isValidGearForOutputSpeed v puMode gear (MeterSecond vi) /= EQ
                -- Reverting to backup power unit mode changed the scope of the gear: repeat step with newly selected pum.
                then gearAcceleration v puMode (preferredPuMode, backupPuMode) gear (MeterSecond vi) (Seconds ti) (MeterSecond vTarget) (LonG lg) (LonG lgToll) ru
                -- Power Unit Mode did not change scope of the gear.
                else
                    -- Verify it can provide acceleration.
                    if newVi <= vi
                    then [] -- PowerUnit cannot deliver acceleration power.
                    
                    else
                        let nextStep = gearAcceleration v puMode (preferredPuMode, backupPuMode) gear (MeterSecond newVi) (Seconds $ ti + tGaps) (MeterSecond vTarget) (LonG lg) (LonG lgToll) ru
                         in -- Verify at this stage, this gear can provide enough acceleration, else move without logging the step.
                            if acc >= tollAcc
                            then ((MeterSecond vi, Seconds ti), (Nm tEngine, HZ eSpeed), puMode) : nextStep
                            else nextStep
        --
        where
        -- | Decides which power unit mode should be selected between the
        -- | preferred one and the backup option. In particular, it reverts
        -- | to backup if the given mode cannot produce enough torque.
        biasedPuModeForTorque :: Vehicle -> (PowerUnitMode, PowerUnitMode) -> Rev -> Torque -> (Torque, PowerUnitMode)
        biasedPuModeForTorque v (preferredPuMode, backupPuMode) eSpeed eTorque =
            let puMode =
                    if isValidGearForOutputTorque v preferredPuMode eSpeed eTorque
                    then preferredPuMode
                    else backupPuMode
                --
                newTorque = min eTorque (powerUnitMaxTorque v puMode eSpeed)
                --
             in (newTorque, puMode)
    --
    gearAcceleration v currPuMode (preferredPuMode, backupPuMode) g vi ti vTarget lg lgToll ru =
        gearAcceleration v currPuMode (preferredPuMode, backupPuMode) g (fSpeedToMeterSecond vi) (fTimeToSeconds ti) (fSpeedToMeterSecond vTarget) lg lgToll ru

{-
    Greedily chooses gearing for an acceleration.
    THIS FUNCTION DOES NOT COMPUTE OPTIMAL GEARING AND
    IS THEREFORE DEPRECATED: USE DIJKSTRA VARIANT.
-}
{-# DEPRECATED accGearSeqGreedy "Use Dijkstra approach instead: accGearSeq" #-}
accGearSeqGreedy :: [(GearRatio, [(Speed, PropulsionCostPerSecond)])] -> [(GearRatio, Speed, PropulsionCostPerSecond)]
accGearSeqGreedy [] = []
accGearSeqGreedy all =
    let -- Find starting speed: minimum speed in dataset.
        vStart = minimum [ spd | (gr, logs) <- all, let (spd, cost) = head logs]
        --
        -- Find the best configuration at vStart.
        (gearStart, _, costStart) = minimumBy (cmpOn (\(_, _, x) -> x)) [ (gr, spd, cost) | (gr, logs) <- all, let (spd, cost) = head logs, spd == vStart]
        --
        -- Drop all lower gears.
        gearOptions = takeAllThat (\(gr, _) -> gr >= gearStart) all
        --
        fullGearing = inner gearOptions [(gearStart, vStart, costStart)]
        --
     in fullGearing
    --
    where
    inner :: [(GearRatio, [(Speed, PropulsionCostPerSecond)])] -> [(GearRatio, Speed, PropulsionCostPerSecond)] -> [(GearRatio, Speed, PropulsionCostPerSecond)]
    inner [] buff = reverse buff
    inner _gearOpts buff =
        let -- Retrieve the current state.
            (gr0, v0, c0) = head buff
            --
            -- Remove all gear options with lower current speed.
            gearOpts =
                [ (gr, skimmedLogs)
                | (gr, grLogs) <- _gearOpts
                , let skimmedLogs = takeAllThat (\x -> fst x > v0) grLogs
                , skimmedLogs /= []
                ]
            --
            -- Next speed on this gear.
            nextSpeedOnGr0 =
                let mSpd = [ spd | (gr, grLogs) <- gearOpts, gr == gr0, let (spd, _) = head grLogs]
                 in case mSpd of
                        -- Gear is exhausted -> then at least next smallest speed.
                        [] -> head [spd | (gr, grLogs) <- gearOpts, let (spd, _) = head grLogs]
                        --
                        [spd] -> spd
            --
            -- Aggregate all next options: next higher speed.
            nextOpts =
                [ (gr1, v1, c1)
                | (gr1, gr1Logs) <- gearOpts
                , let (v1, c1) = head gr1Logs
                -- Cannot be greater speed than the next acceleration target.
                , v1 <= nextSpeedOnGr0
                ]
            --
         in if nextOpts /= []
            --
            then
                let -- Take the best option.
                    nextState = minimumBy (cmpOn (\(_, _, x) -> x)) nextOpts
                    --
                    -- From next options take away the last used gear if gear change occurred in this step.
                    nextGearOpts
                        | gr0 == (let (x, _, _) = nextState in x) = gearOpts
                        | otherwise = tail gearOpts
                    --
                 in inner nextGearOpts (nextState:buff)
            --
            else reverse buff

{-
    Computes the optimal gearing with Dijkstra approach, from a
    collection of sample points.
-}
accGearSeq :: [(GearRatio, [(Speed, PropulsionCostPerSecond)])] -> [(GearRatio, Speed, PropulsionCostPerSecond)]
accGearSeq [] = []
accGearSeq all =
    let -- Find starting speed: minimum speed in dataset.
        _vStart = minimum [ spd | (gr, logs) <- all, let (spd, cost) = head logs]
        -- Find the best configuration at vStart.
        startNodes = [ (node, (0, node)) | (gr, logs) <- all, let (spd, cost) = head logs, spd == _vStart, let node = GearingShiftNode gr spd cost]
        --
        -- Find end speed: maximum speed in dataset.
        vEnd = maximum [ spd | (_, logs) <- all, let (spd, cost) = last logs]
        --
        -- Build gearing.
        fullGearing = buildGearing all (HashMap.fromList startNodes) HashSet.empty vEnd HashMap.empty
        --
        res = map (\(GearingShiftNode gr spd c) -> (gr, spd, c)) fullGearing
        --
     in reverse res
    --
    where
    getNeighbors :: [(GearRatio, [(Speed, PropulsionCostPerSecond)])] -> GearingShiftNode-> [GearingShiftNode]
    getNeighbors allLogs (GearingShiftNode gr0 v0 _) =
        let -- Remove all gear options with lower current speed, or higher gear.
            gearOpts =
                [ (gr, skimmedLogs)
                | (gr, grLogs) <- allLogs
                , gr >= gr0
                , let skimmedLogs = takeAllThat (\x -> fst x > v0) grLogs
                , skimmedLogs /= []
                ]
            --
            -- Next speed on this gear.
            nextSpeedOnGr0 =
                let mSpd = [ spd | (gr, grLogs) <- gearOpts, gr == gr0, let (spd, _) = head grLogs]
                 in case mSpd of
                        -- Gear is exhausted -> then at least next smallest speed.
                        [] -> head [spd | (gr, grLogs) <- gearOpts, let (spd, _) = head grLogs]
                        --
                        [spd] -> spd
            --
            -- Aggregate all next options: next higher speed.
            nextOpts =
                [ GearingShiftNode gr1 v1 c1
                | (gr1, gr1Logs) <- gearOpts
                , let (v1, c1) = head gr1Logs
                -- Cannot be greater speed than the next acceleration target.
                , v1 <= nextSpeedOnGr0
                ]
            --
         in nextOpts
    --
    fromNodeCost :: GearingShiftNode -> Double
    fromNodeCost (GearingShiftNode _ _ _c) = applyCurrencyExchange $ currEx $ pcpsCost _c
    --
    backTrackMap :: GearingShiftNode -> HashMap.HashMap GearingShiftNode (Double, GearingShiftNode) -> [GearingShiftNode]
    backTrackMap currNode m =
        case HashMap.lookup currNode m of
            -- Finished.
            Nothing -> [currNode]
            -- Next jump.
            Just (_, nextNode) -> currNode : backTrackMap nextNode (HashMap.delete currNode m)
    --
    buildGearing ::
           [(GearRatio, [(Speed, PropulsionCostPerSecond)])]
        -> HashMap.HashMap GearingShiftNode (Double, GearingShiftNode)
        -> HashSet.HashSet GearingShiftNode
        -> Speed
        -> HashMap.HashMap GearingShiftNode (Double, GearingShiftNode)
        -> [GearingShiftNode]
    buildGearing [] _ _ _ _ = []
    buildGearing logs m s goalSpeed allFullyEvals =
        if HashMap.size m == 0
        then
            let -- Find node with target speed and minimum cost.
                hasGoalSpeed (GearingShiftNode _ spd _, (_, _)) = spd == goalSpeed
                allNodesWithGoalSpeed = takeAllThat hasGoalSpeed (HashMap.toList allFullyEvals)
                (goalNode, _) = minimumBy (cmpOn (\(_, (x, _)) -> x)) allNodesWithGoalSpeed
             in backTrackMap goalNode allFullyEvals
        else
            let -- Get the next best node.
                (node, (accNodeCost, nodeSrc)) = minimumBy (cmpOn (\(_, (x, _)) -> x)) (HashMap.toList m)
                --
                -- Get its non fully evaluated neighbors.
                nbs = takeAllThat (\x -> not $ HashSet.member x s) (getNeighbors logs node)
                --
                -- The accumulated cost of moving to a neighbor.
                nbNewCost = accNodeCost + fromNodeCost node
                --
                -- Update the map.
                newMap = updateMap nbNewCost node nbs (HashMap.delete node m)
                --
                -- Add fully evaluated node to set, and buff map.
                newSet = HashSet.insert node s
                newAllFullyEvals = HashMap.insert node (accNodeCost, nodeSrc) allFullyEvals
                --
            in buildGearing logs newMap newSet goalSpeed newAllFullyEvals
         --
        where
        updateMap ::
               Double
            -> GearingShiftNode
            -> [GearingShiftNode]
            -> HashMap.HashMap GearingShiftNode (Double, GearingShiftNode)
            -> HashMap.HashMap GearingShiftNode (Double, GearingShiftNode)
        --
        updateMap _ _ [] m = m
        --
        updateMap accCost srcNode (gsn:xs) m =
            case HashMap.lookup gsn m of
                --
                Nothing -> updateMap accCost srcNode xs (HashMap.insert gsn (accCost, srcNode) m)
                --
                Just (prevAccCost, prevSrcNode) ->
                    if prevAccCost > accCost
                    then updateMap accCost srcNode xs (HashMap.insert gsn (accCost, srcNode) m)
                    else updateMap accCost srcNode xs m

{-
    Completes an acceleration configuration by computing optimal gearing
    given the specific fuel consumption of each gear in its speed range.
-}
completeAccConfig ::
       [(GearRatio, [((Speed, Time), (Torque, Rev), PowerUnitMode)])]
    -> [(GearRatio, [(Speed, PropulsionCostPerSecond)])]
    -> ((Time, Length), [(GearRatio, Speed)], [(GearRatio, Speed, Time, Length, Torque, Rev)])
--
completeAccConfig allPerGr consumptions =
    let -- Compute optimal gearing.
        optGearingWithConsumption = accGearSeq consumptions
        -- Drop consumption term from optimal gearing.
        optGearing = map (\(g, s, _) -> (g, s)) optGearingWithConsumption
        -- Now summarize the optimal gearing.
        summOptGearing = summarizeGearing optGearing
        --
        -- Copy allPerGr into a hashmap. Type of map: HashMap.HashMap Double [((Speed, Time), (Torque, Rev))]
        perGgMap = foldl
                    (\m (GRImp gr, vals) ->
                        if vals /= []
                        then HashMap.insert gr [(st, tr) | (st, tr, _) <- vals] m
                        else m
                    )
                    HashMap.empty allPerGr
        --
        (totTimeAndTotLength, gearAndPUDemandsInTimeSpace) = accTandD perGgMap summOptGearing (Seconds 0, Meter 0) []
        --
     in (totTimeAndTotLength, summOptGearing, gearAndPUDemandsInTimeSpace)
    where
        accTandD ::
               HashMap.HashMap Double [((Speed, Time), (Torque, Rev))]
            -> [(GearRatio, Speed)] -- Summarized!!!
            -> (Time, Length)
            -> [(GearRatio, Speed, Time, Length, Torque, Rev)]
            -> ((Time, Length), [(GearRatio, Speed, Time, Length, Torque, Rev)])
        accTandD m [] buff tms = (buff, tms)
        accTandD m [l] buff tms = (buff, tms)
        accTandD m ((GRImp gr0, v0):(gr1, v1):xs) (Seconds ctt, Meter cdt) tms =
            let -- For this gear, sum all time and distance covered in sampled data.
                Just grEntries = HashMap.lookup gr0 m
                --
                allInSpeedGap = getGearAccTelGap grEntries v0 v1
                --
                -- We know the time gaps: product of length with constant.
                Seconds tGaps = fTimeToSeconds _ACC_SAMPLING_TIME_GAP
                -- Seconds allTime = Seconds $ (*) tGaps (fromIntegral $ length allInSpeedGap)
                Seconds allTime =
                    if allInSpeedGap /= []
                    then
                        let ((_, _tFirst), _) = head allInSpeedGap
                            ((_, _tLast), _)  = last allInSpeedGap
                            --
                            Seconds tFirst = fTimeToSeconds _tFirst
                            Seconds tLast  = fTimeToSeconds _tLast
                        in Seconds $ tLast - tFirst
                    else Seconds 0 -- negligible speed gap.
                -- Total Distance.
                Meter allDist = computeDistanceCovered allInSpeedGap (Seconds tGaps) 0
                -- Distances and times from current buffer at each log.
                attachedTimestamps = attachTimeStamps (GRImp gr0) allInSpeedGap (Seconds ctt, Meter cdt) (Seconds tGaps)
                --
                newCurrTimestamps = (Seconds $ ctt + allTime, Meter $ cdt + allDist)
             in accTandD m xs newCurrTimestamps (tms++attachedTimestamps)
            where
            --
            getGearAccTelGap :: [((Speed, Time), (Torque, Rev))] -> Speed -> Speed -> [((Speed, Time), (Torque, Rev))]
            getGearAccTelGap all v0 v1 =
                let pragmaGTmin ((v, _), _) = v >= v0
                    pragmaLTmax ((v, _), _) = v <= v1
                in takeAllThat (\x -> pragmaGTmin x && pragmaLTmax x) all
            --
            computeDistanceCovered :: [((Speed, Time), (Torque, Rev))] -> Time -> Double -> Length
            computeDistanceCovered [] _ buff = Meter buff
            computeDistanceCovered (((_v0, _), _):xs) (Seconds tGaps) buff =
                let MeterSecond v0 = fSpeedToMeterSecond _v0
                    dist = v0 * tGaps
                    --
                in computeDistanceCovered xs (Seconds tGaps) (buff + dist)
            --
            attachTimeStamps :: GearRatio -> [((Speed, Time), (Torque, Rev))] -> (Time, Length) -> Time -> [(GearRatio, Speed, Time, Length, Torque, Rev)]
            attachTimeStamps _ [] _ _ = []
            attachTimeStamps gr (((_v0, t0), (eT0, eS0)):xs) (cT, cD) (Seconds tGaps) =
                (gr, _v0, cT, cD, eT0, eS0) : inner gr (((_v0, t0), (eT0, eS0)):xs) (cT, cD) (Seconds tGaps)
                where
                inner :: GearRatio -> [((Speed, Time), (Torque, Rev))] -> (Time, Length) -> Time -> [(GearRatio, Speed, Time, Length, Torque, Rev)]
                inner _ [] _ _ = []
                inner _ [l] _ _ = []
                inner gr (((_v0, _), (eT0, eS0)):((_v1, t1), (eT1, eS1)):xs) (Seconds cT, Meter cD) (Seconds tGaps) =
                    let MeterSecond v0 = fSpeedToMeterSecond _v0
                        MeterSecond v1 = fSpeedToMeterSecond _v1
                        --
                        distElapsed = ((v1 - v0) / 2) * tGaps
                        --
                        newTimestamp = Seconds $ cT + tGaps
                        newReachedLength = Meter $ cD + distElapsed
                        --
                        l1 = (gr, _v1, newTimestamp, newReachedLength, eT1, eS1)
                    in l1 : attachTimeStamps gr (((_v1, t1), (eT1, eS1)):xs) (newTimestamp, newReachedLength) (Seconds tGaps)

{-
    Computes a sequence of braking requirements to allow the vehicle to
    perform a deceleration in some road unit in full electric mode.
    @Vehicle: vehicle which performs the acceleration (with updated current speed).
    @Speed: final speed.
    @LonG: target deceleration rate.
    @RoadUnit: road unit in which to perform the acceleration.

    @returns: a list of sample point per gear. A sample point contains:
                - speed reached
                - time speed was reached
                - torque required to move to next sample point.
                - engine speed required to reach next sample point
-}
decelerationGearing :: Vehicle -> Speed -> LonG -> RoadUnit -> [(GearRatio, [((Speed, Time), (Torque, Rev))])]
decelerationGearing v vf lg ru =
    [ (gr, logs)
    | gr <- vehicleGearbox v
    , let logs = inner v vf gr lg ru (Seconds 0)
    ]
    where
    inner :: Vehicle -> Speed -> GearRatio -> LonG -> RoadUnit -> Time -> [((Speed, Time), (Torque, Rev))]
    inner v vf gr lg ru t0
        -- Reached final speed.
        | vehicleCurrentSpeed v <= vf = []
        -- if gear is under revving for current speed -> stop: gear is exhausted.
        | isValidGearForOutputSpeed v PUMPureEEngine gr (vehicleCurrentSpeed v) == LT = []
        -- iff gear is ove revving for current speed -> restart with gear's maximum speed, if this is in range.
        | isValidGearForOutputSpeed v PUMPureEEngine gr (vehicleCurrentSpeed v) == GT =
            let gMaxSpd = gearMaxSpeed v PUMPureEEngine gr
             in if gMaxSpd >= vehicleCurrentSpeed v && gMaxSpd >= vf
                then inner (updateVehicleCurrentSpeed v gMaxSpd) vf gr lg ru t0
                else []
        --
        | otherwise =
            let MeterSecond vSpeed = fSpeedToMeterSecond $ vehicleCurrentSpeed v
                -- Braking at this time.
                (_, wTorque) = brakingForce v ru lg
                -- Wheel speed at this time.
                wSpeed = wheelSpeed (MeterSecond vSpeed) (vehicleWheelRadius v)
                --
                -- Compute eTorque and eSpeed.
                transmission = [gr, vehicleDiff v]
                eTorque = fTorqueInSeq wTorque transmission
                eSpeed = fRevsInSeq wSpeed transmission
                --
                needsBraking = let Nm tmp = eTorque in tmp < 0
                --
                -- Output this step
                log = ((MeterSecond vSpeed, t0), (eTorque, eSpeed))
                --
                -- Compute vehicle's new speed.
                dec = let LonG gs = lg in - (gs * _EARTH_GRAVITY_ACCELERATION)
                Seconds sampleTime = fTimeToSeconds _DEC_SAMPLING_TIME_GAP
                vSpeedNextStep = MeterSecond $ vSpeed + (dec * sampleTime)
                --
                newV = updateVehicleCurrentSpeed v vSpeedNextStep
                newT = Seconds $ let Seconds t = fTimeToSeconds t0 in t + sampleTime
                --
             in if needsBraking then log : inner newV vf gr lg ru newT else []

{-
    Computes the optimal gearing with Dijkstra approach, from a
    collection of sample points.
-}
decGearSeq ::
       [(GearRatio, [(Speed, CurrentEnergy, PowerUnitConfig)])]
    -> ([(GearRatio, Speed)], [(Speed, CurrentEnergy, PowerUnitConfig)])
decGearSeq [] = ([], [])
decGearSeq conslogs =
    let -- Reduce logs for gearing reduction.
        gearingSpeedCostLogs =
            [ (gr, grLogs)
            | (gr, _grLogs) <- conslogs
            , let grLogs = [ (spd, currEn) | (spd, currEn, _) <- _grLogs]
            , _grLogs /= []
            ]
        --
        fullGearing = map (\(x, y, _) -> (x, y)) $ decFullGearingSeq gearingSpeedCostLogs
        --
        -- Summarize full gearing.
        summGearing = summarizeGearing $ reverse fullGearing
        --
        -- Now compose the full configuration by drawing from the conslogs those configurations
        -- in range of the summGearing.
        fullConfig = buildConfigFromSumm conslogs summGearing
        --
     in (summGearing, fullConfig)
    --
    where
    buildConfigFromSumm ::
           [(GearRatio, [(Speed, CurrentEnergy, PowerUnitConfig)])]
        -> [(GearRatio, Speed)]
        -> [(Speed, CurrentEnergy, PowerUnitConfig)]
    --
    buildConfigFromSumm [] _  = []
    buildConfigFromSumm _ []  = []
    buildConfigFromSumm _ [l] = []
    --
    buildConfigFromSumm all ((gr0, v0):(_, v1):xs) =
        let -- Get the logs in range for this gear.
            logs = concat
                    [allInRange
                    | (gr, allForGr) <- all
                    , gr == gr0
                    , let allInRange = takeAllThat (\(v, _, _) -> inRange v v1 v0) allForGr
                    ]
         in logs ++ buildConfigFromSumm all xs

{-
    Greedily chooses gearing for an deceleration.
    THIS FUNCTION DOES NOT COMPUTE OPTIMAL GEARING AND
    IS THEREFORE DEPRECATED: USE DIJKSTRA VARIANT.
-}
{-# DEPRECATED decFullGearingSeqGreedy "Use Dijkstra approach instead: decGearSeq" #-}
decFullGearingSeqGreedy :: [(GearRatio, [(Speed, CurrentEnergy)])] -> [(GearRatio, Speed, CurrentEnergy)]
decFullGearingSeqGreedy [] = []
decFullGearingSeqGreedy all =
    let -- Find starting speed: maximum speed in dataset.
        vStart = maximum [ spd | (gr, logs) <- all, logs /= [], let (spd, cost) = head logs]
        --
        -- Find the best configuration at vStart.
        (gearStart, _, costStart) = maximumBy (cmpOn (\(_, _, x) -> x)) [ (gr, spd, cost) | (gr, logs) <- all, let (spd, cost) = head logs, spd == vStart]
        --
        -- Drop all higher gears.
        gearOptions = takeAllThat (\(gr, _) -> gr <= gearStart) all
        --
        fullGearing = inner gearOptions [(gearStart, vStart, costStart)]
        --
     in fullGearing
    --
    where
    inner :: [(GearRatio, [(Speed, CurrentEnergy)])] -> [(GearRatio, Speed, CurrentEnergy)] -> [(GearRatio, Speed, CurrentEnergy)]
    inner [] buff = buff
    inner _gearOpts buff =
        let -- Retrieve the current state.
            (gr0, v0, c0) = head buff
            --
            -- Remove all gear options with higher current speed.
            gearOpts =
                [ (gr, skimmedLogs)
                | (gr, grLogs) <- _gearOpts
                , let skimmedLogs = takeAllThat (\x -> fst x < v0) grLogs
                , skimmedLogs /= []
                ]
            --
            -- Next speed on this gear.
            nextSpeedOnGr0 =
                let mSpd = [ spd | (gr, grLogs) <- gearOpts, gr == gr0, let (spd, _) = last grLogs]
                 in case mSpd of
                        -- Gear is exhausted -> then at least next highest speed.
                        [] -> last [spd | (gr, grLogs) <- gearOpts, let (spd, _) = last grLogs]
                        --
                        [spd] -> spd
            --
            -- Aggregate all next options: next lowest speed.
            nextOpts =
                [ (gr1, v1, c1)
                | (gr1, gr1Logs) <- gearOpts
                , let (v1, c1) = last gr1Logs
                -- Cannot be greater speed than the next acceleration target.
                , v1 <= nextSpeedOnGr0
                ]
            --
         in if nextOpts /= []
            --
            then
                let -- Take the best option.
                    nextState = maximumBy (cmpOn (\(_, _, x) -> x)) nextOpts
                    --
                    -- From next options take away the last used gear if gear change occurred in this step.
                    nextGearOpts
                        | gr0 == (let (x, _, _) = nextState in x) = gearOpts
                        | otherwise = tail gearOpts
                    --
                 in inner nextGearOpts (nextState:buff)
            --
            else buff

{-
    Completes a deceleration configuration by computing optimal gearing.
-}
decFullGearingSeq :: [(GearRatio, [(Speed, CurrentEnergy)])] -> [(GearRatio, Speed, CurrentEnergy)]
decFullGearingSeq [] = []
decFullGearingSeq all =
    let -- Find starting speed: maximum speed in dataset.
        _vStart = maximum [ spd | (gr, logs) <- all, logs /= [], let (spd, cost) = head logs]
        -- Find the best configuration at vStart.
        startNodes = [ (node, (0, node)) | (gr, logs) <- all, let (spd, curr) = head logs, spd == _vStart, let node = BrakingShiftNode gr spd curr]
        --
        -- Find end speed: maximum speed in dataset.
        vEnd = minimum [ spd | (_, logs) <- all, logs /= [], let (spd, cost) = last logs]
        --
        -- Build gearing.

        fullGearing = buildGearing all (HashMap.fromList startNodes) HashSet.empty vEnd HashMap.empty
        --
        res = map (\(BrakingShiftNode gr spd c) -> (gr, spd, c)) fullGearing
        --
     in res
    --
    where
    getNeighbors :: [(GearRatio, [(Speed, CurrentEnergy)])] -> BrakingShiftNode -> [BrakingShiftNode]
    getNeighbors allLogs (BrakingShiftNode gr0 v0 _) =
        let -- Remove all gear options with higher current speed, or lower gear.
            gearOpts =
                [ (gr, skimmedLogs)
                | (gr, grLogs) <- allLogs
                , gr <= gr0
                , grLogs /= []
                , let skimmedLogs = takeAllThat (\x -> fst x < v0) grLogs
                , skimmedLogs /= []
                ]
         in case gearOpts of
                [] -> []
                _ ->
                    let -- Next speed on this gear.
                        nextSpeedOnGr0 =
                            case [spd | (gr, grLogs) <- gearOpts, gr == gr0, let spd = maximum (map fst grLogs)] of
                                -- Gear is exhausted -> then at least next highest speed.
                                [] -> maximum [spd | (gr, grLogs) <- gearOpts, let (spd, _) = head grLogs]
                                --
                                [spd] -> spd
                        --
                        -- Aggregate all next options: next lower speed.
                        nextOpts =
                            [ BrakingShiftNode gr1 v1 c1
                            | (gr1, gr1Logs) <- gearOpts
                            , let (v1, c1) = head gr1Logs
                            -- Cannot be smaller speed than the next deceleration target.
                            , v1 >= nextSpeedOnGr0
                            ]
                        --
                     in nextOpts
    --
    fromNodeCost :: BrakingShiftNode -> Double
    fromNodeCost (BrakingShiftNode _ _ c) = let KWattHour x = currentEnergyToKWattHour c in -x
    --
    backTrackMap :: BrakingShiftNode -> HashMap.HashMap BrakingShiftNode (Double, BrakingShiftNode) -> [BrakingShiftNode]
    backTrackMap currNode m =
        case HashMap.lookup currNode m of
            -- Finished.
            Nothing -> [currNode]
            -- Next jump.
            Just (_, nextNode) -> currNode : backTrackMap nextNode (HashMap.delete currNode m)
    --
    buildGearing ::
           [(GearRatio, [(Speed, CurrentEnergy)])]
        -> HashMap.HashMap BrakingShiftNode (Double, BrakingShiftNode)
        -> HashSet.HashSet BrakingShiftNode
        -> Speed
        -> HashMap.HashMap BrakingShiftNode (Double, BrakingShiftNode)
        -> [BrakingShiftNode]
    buildGearing [] _ _ _ _ = []
    buildGearing logs m s goalSpeed allFullyEvals =
        if HashMap.size m == 0
        then
            let -- Find node with target speed and minimum cost.
                hasGoalSpeed (BrakingShiftNode _ spd _, (_, _)) = spd == goalSpeed
                allNodesWithGoalSpeed = takeAllThat hasGoalSpeed (HashMap.toList allFullyEvals)
                (goalNode, _) = minimumBy (cmpOn (\(_, (x, _)) -> x)) allNodesWithGoalSpeed
             in backTrackMap goalNode allFullyEvals
        else
            let -- Get the next best node.
                (node, (accNodeCost, nodeSrc)) = minimumBy (cmpOn (\(_, (x, _)) -> x)) (HashMap.toList m)
                --
                -- Get its non fully evaluated neighbors.
                nbs = takeAllThat (\x -> not $ HashSet.member x s) (getNeighbors logs node)
                --
                -- The accumulated cost of moving to a neighbor.
                nbNewCost = accNodeCost + fromNodeCost node
                --
                -- Update the map.
                newMap = updateMap nbNewCost node nbs (HashMap.delete node m)
                --
                -- Add fully evaluated node to set, and buff map.
                newSet = HashSet.insert node s
                newAllFullyEvals = HashMap.insert node (accNodeCost, nodeSrc) allFullyEvals
                --
            in buildGearing logs newMap newSet goalSpeed newAllFullyEvals
         --
        where
        updateMap ::
               Double
            -> BrakingShiftNode
            -> [BrakingShiftNode]
            -> HashMap.HashMap BrakingShiftNode (Double, BrakingShiftNode)
            -> HashMap.HashMap BrakingShiftNode (Double, BrakingShiftNode)
        --
        updateMap _ _ [] m = m
        --
        updateMap accCost srcNode (gsn:xs) m =
            case HashMap.lookup gsn m of
                --
                Nothing -> updateMap accCost srcNode xs (HashMap.insert gsn (accCost, srcNode) m)
                --
                Just (prevAccCost, prevSrcNode) ->
                    if prevAccCost > accCost
                    then updateMap accCost srcNode xs (HashMap.insert gsn (accCost, srcNode) m)
                    else updateMap accCost srcNode xs m
