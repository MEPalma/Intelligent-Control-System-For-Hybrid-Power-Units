{-
    Encapsulates the functions required for the
    building of a solution which simulates the
    expected behavior of an hybrid vehicle.
-}
module Searches.Simulated where
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
    Attempts to compute a solution which simulates the
    expected behavior of an hybrid vehicle. This is achieved
    by at every processed road unit (non braking) choosing the
    most cost efficient option between an fully assisted
    electric mode, and a pure ic engine mode. This routine
    fails only if the vehicle does not provide enough charge
    and/or petrol for the completion of the journey.
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
                case filterJusts $ expandRoadUnit v pru [PUMFullECompensation, PUMPureICEngine] of
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