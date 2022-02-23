{-
    Aggregation of functions for the building of
    a solution which only utilizes the spark ignition
    engine of a vehicle. Hence simulating the behavior
    of a non hybrid vehicle in an itinerary.
-}
module Searches.NoHybridSimulated where
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

{-
    computes a solution which only utilizes the spark ignition
    engine of a vehicle. Hence simulating the behavior
    of a non hybrid vehicle in an itinerary. This fails only if
    the vehicle does not have enough octane to complete the journey.
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
        case pru of
            --
            DropRoadUnit{} ->
                case expandRoadUnitSingle v pru PUMPureEEngine of
                    --
                    Nothing -> trace ("Cannot recharge in RoadUnit: " ++ show pru) Nothing
                    --
                    Just exp -> expToNextCall v prus buff exp
            --
            PickPaceRoadUnit{} ->
                case filterJusts $ expandRoadUnit v pru [PUMPureICEngine] of
                    -- No resources or infeasible requirements.
                    [] -> trace ("Cannot overcome RoadUnit: " ++ show pru) Nothing
                    --
                    opts -> expToNextCall v prus buff (minimumBy (cmpOn expTotalCost) opts)
    --
    --
    expToNextCall ::
           Vehicle
        -> [ProcessedRoadUnit]
        -> [Expansion]
        -> Expansion
        -> Maybe [Expansion]
    expToNextCall v prus buff exp =
        let newState = expFinalVehicleState exp
         in inner v{vehicleState = newState} prus (exp:buff)