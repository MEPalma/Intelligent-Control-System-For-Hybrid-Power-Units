{-
    Encapsulates a miscellaneous of general purpose functions
    required by all search routines. 
-}
module Searches.Commons where
--
import Debug.Trace
import Data.Hashable
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe
import qualified RIO
--
import Commons.Currency
import Commons.ISUnits
import Commons.ColoredOutput
--
import Vehicle.Vehicle
import Vehicle.VehicleState
import VehicleDynamics.VehicleDynamics
--
import PowerUnit.PowerUnitTypes
import PowerUnit.PowerUnit
--
import Itinerary.Itinerary
import Itinerary.ProcessedItinerary
--
import Searches.Solution


{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}


{-
    Data type for a variant expansion which does not consider
    the vehicle's energy capacity, hence unsafe as it includes
    physically available expansions, but not necessarily energy
    available. 
-}
data UnsafeExpansion =
        UnsafePaceExpansion
            { unsafeExpFinalVehicleState :: Maybe VehicleState
            , unsafeExpRoadUnit          :: ProcessedRoadUnit
            , unsafeExpPUC               :: PowerUnitConfig
            , unsafeExpTotalCost         :: Currency
            }
        | UnsafePickExpansion
            { unsafeExpFinalVehicleState :: Maybe VehicleState
            , unsafeExpRoadUnit          :: ProcessedRoadUnit
            , unsafeExpAccConfig         :: AccConfig
            , unsafeExpTotalCost         :: Currency
            }
        | UnsafeDropExpansion
            { unsafeExpFinalVehicleState :: Maybe VehicleState
            , unsafeExpRoadUnit          :: ProcessedRoadUnit
            , unsafeExpDecConfig         :: DecConfig
            }
        deriving (Show)

{-
    Attempts to convert an unsafe expansion into an expansions by
    verifying the vehicle state can provide the energy demanded.
-}
unsafeExpansionToExpansion :: UnsafeExpansion -> Maybe Expansion
unsafeExpansionToExpansion unfExp =
    case unsafeExpFinalVehicleState unfExp of
        --
        Nothing -> Nothing
        --
        Just newState -> Just $
            case unfExp of
                UnsafePaceExpansion{} -> PaceExpansion newState (unsafeExpRoadUnit unfExp) (unsafeExpPUC unfExp)       (unsafeExpTotalCost unfExp)
                UnsafePickExpansion{} -> PickExpansion newState (unsafeExpRoadUnit unfExp) (unsafeExpAccConfig unfExp) (unsafeExpTotalCost unfExp)
                UnsafeDropExpansion{} -> DropExpansion newState (unsafeExpRoadUnit unfExp) (unsafeExpDecConfig unfExp)

{-
    From cache it retrieves an unsafe expansion of a processed road unit
-}
pruToCacheUnsafePickPaceExpansion :: Vehicle -> ProcessedRoadUnit -> ExpCacheEntry -> Maybe UnsafeExpansion
pruToCacheUnsafePickPaceExpansion v pru expEntry =
    case pru of
        PickPaceRoadUnit _ PACE _ -> pruToCacheUnsafePaceExpansion v pru expEntry
        PickPaceRoadUnit _ PICK _ -> pruToCacheUnsafePickExpansion v pru expEntry
    --
    where
    pruToCacheUnsafePaceExpansion :: Vehicle -> ProcessedRoadUnit -> ExpCacheEntry -> Maybe UnsafeExpansion
    pruToCacheUnsafePaceExpansion v (PickPaceRoadUnit _ruid PACE ru) (ExpCacheEntryPace (fuelReq, currReq, puc, totCost)) =
        case getVehicleResultingEnergyState (vehicleState v) (fuelReq, currReq) of
            --
            Nothing -> Just $ UnsafePaceExpansion Nothing (PickPaceRoadUnit _ruid PACE ru) puc totCost
            --
            Just newEnergyState ->
                let newState = newEnergyState{vehicleStateSpeed = ruSpeed ru}
                in Just $ UnsafePaceExpansion (Just newState) (PickPaceRoadUnit _ruid PACE ru) puc totCost
    --
    pruToCacheUnsafePickExpansion :: Vehicle -> ProcessedRoadUnit -> ExpCacheEntry -> Maybe UnsafeExpansion
    pruToCacheUnsafePickExpansion v (PickPaceRoadUnit _ruid PICK ru) (ExpCacheEntryPick (fuelReq, currReq, accConfig, totCost)) =
        case getVehicleResultingEnergyState (vehicleState v) (fuelReq, currReq) of
            --
            Nothing -> Just $ UnsafePickExpansion Nothing (PickPaceRoadUnit _ruid PICK ru) accConfig totCost
            --
            Just newEnergyState ->
                let newState = newEnergyState{vehicleStateSpeed = ruSpeed ru}
                in Just $ UnsafePickExpansion (Just newState) (PickPaceRoadUnit _ruid PICK ru) accConfig totCost

{-
    Data type for caching of physically available power unit modes and hence
    power unit configurations per each processed road unit in an itinerary.
-}
type ExpCache =
        HashMap.HashMap
            ProcessedRoadUnit 
            ( HashMap.HashMap
                PowerUnitMode
                (Maybe ExpCacheEntry)
            )
--
data ExpCacheEntry = 
          ExpCacheEntryPace (Mass, CurrentEnergy, PowerUnitConfig, Currency)
        | ExpCacheEntryPick (Mass, CurrentEnergy, AccConfig,       Currency)
        deriving (Eq)
--
instance Show ExpCacheEntry where
    show (ExpCacheEntryPace (fm, ce, puc, cost)) = "ExpCacheEntryPace: " ++ (show fm) ++ " " ++ (show ce) ++ " " ++ show cost
    show (ExpCacheEntryPick (fm, ce, acc, cost)) = "ExpCacheEntryPick: " ++ (show fm) ++ " " ++ (show ce) ++ " " ++ show cost

{-
    From cache it retrieves the cache entry for using a power unit mode
    in a processed road unit.
-}
getFromCache :: ExpCache -> ProcessedRoadUnit -> PowerUnitMode -> Either (Maybe ExpCacheEntry) ()
getFromCache cache pru puMode =
    case HashMap.lookup pru cache of
        Nothing      -> Right ()
        Just inCache -> 
            case HashMap.lookup puMode inCache of
                Nothing -> Right ()
                Just mSumm -> Left mSumm

{-
    Injects into a cache instance the power unit configuration for using a power unit mode
    in a processed road unit.
-}
updateCache :: ExpCache -> ProcessedRoadUnit -> PowerUnitMode -> Maybe ExpCacheEntry -> ExpCache
updateCache cache pru puMode summ =
    case HashMap.lookup pru cache of
        --
        Nothing ->
            let newInCache = HashMap.insert puMode summ HashMap.empty
             in HashMap.insert pru newInCache cache
        --
        Just inCache ->
            let updatedInCache = HashMap.insert puMode summ inCache
             in HashMap.insert pru updatedInCache cache

{-
    Creates a new instance of an empty cache.
-}
emptyCache :: ExpCache
emptyCache = HashMap.empty :: ExpCache

{-
    Constructs a full cache for all power unit modes in all
    processed road units of a processed itinerary.
-}
buildCache :: Vehicle -> ProcessedItinerary -> [PowerUnitMode] -> ExpCache
buildCache v it puModes =
    inner v it puModes emptyCache
    where
    inner :: Vehicle -> ProcessedItinerary -> [PowerUnitMode] -> ExpCache -> ExpCache
    inner _ [] _ cache = cache
    inner v (pru:prus) puModes _cache =
        let (_, newCache) = expandRoadUnitWithCache _cache v pru puModes
        in inner (updateVehicleCurrentSpeed v (ruSpeed $ pruRoadUnit pru)) prus puModes newCache

{-
    Given a processed road unit and a power unit mode, it evaluates
    the feasibility of utilizing such mode in the given road unit.
-}
expandRoadUnitSingle :: Vehicle -> ProcessedRoadUnit -> PowerUnitMode -> Maybe Expansion
expandRoadUnitSingle v pru puMode = head $ expandRoadUnit v pru [puMode]

{-
    Given a processed road unit, for each power unit mode, it evaluates
    the feasibility of completing the road unit, in the order the power
    unit modes are given.
-}
expandRoadUnit ::
       Vehicle
    -> ProcessedRoadUnit
    -> [PowerUnitMode]
    -> [Maybe Expansion]
--
expandRoadUnit v pru puModes =
    let unsafeExps = unsafeExpandRoadUnit v pru puModes
     in [ res
        | mUnfExp <- unsafeExps
        , let res =
                case mUnfExp of
                    --
                    Nothing -> Nothing
                    --
                    Just unfExp -> unsafeExpansionToExpansion unfExp
        ]

{-
    Variant of @expandRoadUnitSingle but utilizes cache to speed up
    the evaluation.
-}
expandRoadUnitFromCacheSingle :: RIO.MVar ExpCache -> Vehicle -> ProcessedRoadUnit -> PowerUnitMode -> IO (Maybe Expansion)
expandRoadUnitFromCacheSingle ioCache v pru puMode = head <$> expandRoadUnitFromCache ioCache v pru [puMode]

{-
    Variant of @expandRoadUnit but utilizes cache to speed up
    the evaluation.
-}
expandRoadUnitFromCache ::
       RIO.MVar ExpCache
    -> Vehicle
    -> ProcessedRoadUnit
    -> [PowerUnitMode]
    -> IO [Maybe Expansion]
--
expandRoadUnitFromCache ioCache v pru puModes =
    do
    unsafeExps <- inner ioCache v pru puModes []
    let allExpansions =
            [ res
            | mUnfExp <- unsafeExps
            , let res =
                    case mUnfExp of
                        Nothing -> Nothing
                        Just unfExp -> unsafeExpansionToExpansion unfExp
            ]
    --
    return allExpansions
    where
    inner ::
           RIO.MVar ExpCache
        -> Vehicle
        -> ProcessedRoadUnit
        -> [PowerUnitMode]
        -> [Maybe UnsafeExpansion]
        -> IO ([Maybe UnsafeExpansion])
    --
    inner _ _ _ [] buff = return (reverse buff)
    --
    inner ioCache v pru (puMode:puModes) buff =
        do
        cache <- RIO.readMVar ioCache
        --
        case pru of
            --
            PickPaceRoadUnit{} ->
                --
                -- Lookup the cache for this road unit in this puMode.
                case getFromCache cache pru puMode of
                    --
                    -- Entry exists: use this lookup value.
                    Left mSumm ->
                        case mSumm of
                            --
                            -- Recorded valid config.
                            Just summ ->
                                let newUnsafeExp = pruToCacheUnsafePickPaceExpansion v pru summ
                                in inner ioCache v pru puModes (newUnsafeExp:buff)
                            --
                            -- Recorded invalid config.
                            Nothing -> inner ioCache v pru puModes (Nothing:buff)
                    --
                    Right () -> error $! showInColor "INCOMPLETE CACHE" RED
            --
            (DropRoadUnit _ruid ru decConfig maxRegen) ->
                let allExps = 
                        replicate (1 + length puModes) $
                        let newEnergyState = getDropResultingVehicleEnergyStateWithKnownMaxRegen (vehicleState v) maxRegen
                            newState = newEnergyState{vehicleStateSpeed = ruSpeed ru}
                        in Just $ UnsafeDropExpansion (Just newState) (DropRoadUnit _ruid ru decConfig maxRegen) decConfig
                    --
                in return allExps

{-
    Variant of @expandRoadUnitFromCache but updates the cache if one
    power unit mode is not present.
-}
expandRoadUnitWithCache ::
       ExpCache
    -> Vehicle
    -> ProcessedRoadUnit
    -> [PowerUnitMode]
    -> ([Maybe Expansion], ExpCache)
--
expandRoadUnitWithCache cache v pru puModes =
    let (unsafeExps, newCache) = unsafeExpandRoadUnitWithCache v pru puModes ([], cache)
        allExpansions =
            [ res
            | mUnfExp <- unsafeExps
            , let res =
                    case mUnfExp of
                        Nothing -> Nothing
                        Just unfExp -> unsafeExpansionToExpansion unfExp
            ]
        --
     in (allExpansions, newCache)

{-
    Expands a processed road unit with all power unit modes supplied.
    In the same order appends to a list the corresponding expansion (if any).
    If the cache does not include a mapping for any of the power unit modes or
    processed road unit, this updates the cache and appends the newly computed
    entry to the output list.
-}
unsafeExpandRoadUnitWithCache ::
       Vehicle
    -> ProcessedRoadUnit
    -> [PowerUnitMode]
    -> ([Maybe UnsafeExpansion], ExpCache)
    -> ([Maybe UnsafeExpansion], ExpCache)
unsafeExpandRoadUnitWithCache _ _ [] (exps, cache) = (reverse exps, cache)
unsafeExpandRoadUnitWithCache v pru (puMode:puModes) (_exps, cache) =
    --
    case pru of
        --
        PickPaceRoadUnit{} ->
            --
            -- Lookup the cache for this road unit in this puMode.
            case getFromCache cache pru puMode of
                --
                -- Entry exists: use this lookup value.
                Left mSumm ->
                    case mSumm of
                        --
                        -- Recorded valid config.
                        Just summ ->
                            let newUnsafeExp = pruToCacheUnsafePickPaceExpansion v pru summ
                                --
                                newBuff = (newUnsafeExp:_exps, cache)
                                --
                             in unsafeExpandRoadUnitWithCache v pru puModes newBuff
                        --
                        -- Recorded invalid config.
                        Nothing -> unsafeExpandRoadUnitWithCache v pru puModes (Nothing:_exps, cache)
                --
                -- No mapping recorded in cache: needs updating.
                Right () ->
                    case pru of
                        PickPaceRoadUnit _ PACE ru ->
                            case steadyPaceCost v puMode ru of
                                --
                                -- New invalid config found.
                                Nothing ->
                                    let newCache = updateCache cache pru puMode Nothing
                                     in unsafeExpandRoadUnitWithCache v pru puModes (Nothing:_exps, newCache)
                                --
                                -- New valid config found.
                                Just (puc, totCost) ->
                                    let (fuelReq, currReq) = getPaceEnergyReq puc (timeOfRoadUnit ru)
                                        newCacheEntry = ExpCacheEntryPace (fuelReq, currReq, puc, totCost)
                                        newCache = updateCache cache pru puMode (Just newCacheEntry)
                                        --
                                        newExp = pruToCacheUnsafePickPaceExpansion v pru newCacheEntry
                                        --
                                     in unsafeExpandRoadUnitWithCache v pru puModes (newExp:_exps, newCache)
                        --
                        PickPaceRoadUnit _ PICK ru ->
                            case accelerationConfigInRoadUnit v puMode ru of
                                --
                                -- New invalid config found.
                                Nothing ->
                                    let newCache = updateCache cache pru puMode Nothing
                                     in unsafeExpandRoadUnitWithCache v pru puModes (Nothing:_exps, newCache)
                                --
                                -- New valid config found.
                                Just accConfig ->
                                    let accLogs = let (_, _, x) = accConfig in x
                                        --
                                        (fuelReq, currReq) = getAccEnergyReq (map (\(_, _, _, puc, _) -> puc) accLogs)
                                        accCost = energyCost v (fuelReq, currReq)
                                        --
                                        newCacheEntry = ExpCacheEntryPick (fuelReq, currReq, accConfig, accCost)
                                        newCache = updateCache cache pru puMode (Just newCacheEntry)
                                        --
                                        newExp = pruToCacheUnsafePickPaceExpansion v pru newCacheEntry
                                        --
                                     in unsafeExpandRoadUnitWithCache v pru puModes (newExp:_exps, newCache)
        --
        (DropRoadUnit _ruid ru decConfig maxRegen) ->
            let allExps = 
                    replicate (length puModes) $
                    let newEnergyState = getDropResultingVehicleEnergyStateWithKnownMaxRegen (vehicleState v) maxRegen
                        newState = newEnergyState{vehicleStateSpeed = ruSpeed ru}
                    in Just $ UnsafeDropExpansion (Just newState) (DropRoadUnit _ruid ru decConfig maxRegen) decConfig
                --
                res = (allExps, cache)
                --
             in res

{-
    Unsafely returns the cost associated with a
    only physically available expansion.
-}
unsafeExpTotalCostSafe :: UnsafeExpansion -> Currency
unsafeExpTotalCostSafe UnsafeDropExpansion{} = 0
unsafeExpTotalCostSafe exp = unsafeExpTotalCost exp


{-
    Expands a processed road unit with all requested power unit modes, evaluating
    their feasibility only on a physical level, hence ignoring the if energy state
    of the vehicle is sufficient to satisfy the energy requirements of the mode.
-}
unsafeExpandRoadUnit :: Vehicle -> ProcessedRoadUnit -> [PowerUnitMode] -> [Maybe UnsafeExpansion]
unsafeExpandRoadUnit v (PickPaceRoadUnit _ruid PACE ru) puModes =
    let pru = PickPaceRoadUnit _ruid PACE ru
    in
    [ result
    | puMode <- puModes
    --
    -- Get the cost.
    , let maybeCost = steadyPaceCost v puMode ru
    --
    , let result =
            case maybeCost of
                --
                -- There exists a valid configuration.
                Just (puConfig, totCost) ->
                    -- Now check this configuration can be sustained by the vehicle
                    -- in terms of resources.
                    case getPaceResultingVehicleEnergyState (vehicleState v) puConfig (timeOfRoadUnit ru) of
                        -- There is a config: return it together with the new state.
                        Just newEnergyState ->
                            let newState = newEnergyState{vehicleStateSpeed = ruSpeed ru}
                             in Just $ UnsafePaceExpansion (Just newState) (PickPaceRoadUnit _ruid PACE ru) puConfig totCost
                        -- There is a config but the vehicle cannot support it: kill this branch.
                        Nothing -> Just $ UnsafePaceExpansion Nothing (PickPaceRoadUnit _ruid PACE ru) puConfig totCost
                --
                -- There is not a valid configuration: kill this branch.
                Nothing -> Nothing
    --
    ]
--
unsafeExpandRoadUnit v (PickPaceRoadUnit _ruid PICK ru) puModes =
    [ result
    | puMode <- puModes
    --
    -- Get the acceleration config for this puMode.
    , let maybeAccConfig = accelerationConfigInRoadUnit v puMode ru
    --
    , let result =
            case maybeAccConfig of
                -- There is an executable acceleration config.
                Just accConfig ->
                    --
                    -- Verify the vehicle can execute this plan, if so return it.
                    let accLogs = let (_, _, x) = accConfig in x
                        --
                        (fuelReq, currReq) = getAccEnergyReq (map (\(_, _, _, puc, _) -> puc) accLogs)
                        accCost = energyCost v (fuelReq, currReq)
                        --
                        maybeNewEnergyState = getVehicleResultingEnergyState (vehicleState v) (fuelReq, currReq)
                    in case maybeNewEnergyState of
                            -- Vehicle can execute the acc config.
                            Just newEnergyState -> 
                                let newState = newEnergyState{vehicleStateSpeed = ruSpeed ru}
                                 in Just $ UnsafePickExpansion (Just newState) (PickPaceRoadUnit _ruid PICK ru) accConfig accCost
                            -- Vehicle cannot execute the acc config.
                            Nothing -> Just $ UnsafePickExpansion Nothing (PickPaceRoadUnit _ruid PICK ru) accConfig accCost
                --
                -- No acceleration config available.
                Nothing -> Nothing
    --
    ]
--
unsafeExpandRoadUnit v (DropRoadUnit _ruid ru decConfig maxRegen) puModes = replicate (length puModes) $
    let newEnergyState = getDropResultingVehicleEnergyStateWithKnownMaxRegen (vehicleState v) maxRegen
        newState = newEnergyState{vehicleStateSpeed = ruSpeed ru}
     in Just $ UnsafeDropExpansion (Just newState) (DropRoadUnit _ruid ru decConfig maxRegen) decConfig
