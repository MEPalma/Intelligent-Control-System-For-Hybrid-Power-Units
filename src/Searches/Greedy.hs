{-
    Encapsulates functions to perform a greedy search
    for a sequence of power unit modes of a vehicle in
    an itinerary. If this approach succeeds, then its
    output is the optimal solution.
-}
module Searches.Greedy where
--
import Commons.ISUnits
import Commons.Utilities
import Commons.Currency
--
import Searches.Commons
import Searches.Solution
--
import Vehicle.Vehicle
import Vehicle.VehicleState
--
import Itinerary.Itinerary
import Itinerary.ProcessedItinerary
--
import PowerUnit.PowerUnitTypes
--
import Data.List
--
import Debug.Trace


-- Resolution of power unit modes.
_PUM_STEP :: Integer
_PUM_STEP = 2
--
_PUM_ALL :: [PowerUnitMode]
_PUM_ALL = getAllPUM _PUM_STEP

{-
    Performs a greedy search for a sequence of power unit
    modes for the completion of an itinerary. If this routine
    does succeed, then its output is the optimal solution.
-}
search :: 
       Vehicle
    -> ProcessedItinerary
    -> Maybe Solution
--
search v it =
    case inner v it [] of
        --
        Nothing -> Nothing
        --
        Just exps -> Just (Solution $ reverse exps)
    --
    where
    inner ::
        Vehicle
        -> ProcessedItinerary
        -> [Expansion]
        -> Maybe [Expansion]
    --
    inner _ [] exps = Just exps
    --
    inner v (pru:prus) buff =
        let allMUnsOpts = unsafeExpandRoadUnit v pru _PUM_ALL
            -- Remove invalid configs.
            allUnsOpts = filterJusts allMUnsOpts
            --
        in case allUnsOpts of
            [] -> trace ("Cannot overcome RoadUnit: " ++ show pru) Nothing
            _  ->
                let -- Get the theoretical best.
                    bestUnfOpt = minimumBy (cmpOn unsafeExpTotalCostSafe) allUnsOpts
                    -- Get its safe version ie. verify it is executable.
                    mBestOpt = unsafeExpansionToExpansion bestUnfOpt
                 in case mBestOpt of 
                        Nothing -> Nothing
                        Just bestOpt ->
                            let newState = expFinalVehicleState bestOpt
                             in inner v{vehicleState = newState} prus (bestOpt:buff)
