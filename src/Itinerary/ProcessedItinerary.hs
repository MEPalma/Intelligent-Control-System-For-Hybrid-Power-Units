{-
    Encapsulated data types and utility functions
    to enrich the information carried by a Itinerary
    to also include power unit's dynamics information.
-}
module Itinerary.ProcessedItinerary where
--
import Data.Hashable
--
import Commons.Currency
import Commons.ISUnits
--
import GearboxDynamics.Gearbox
--
import Itinerary.Itinerary
--
import Vehicle.VehicleState
import PowerUnit.PowerUnitTypes

{-
    Data type which describes the task the vehicle
    should perform in a road unit:
        @PICK: acceleration
        @PACE: constant speed
        #DROP: deceleration
-}
data UnitTask = PICK | PACE | DROP deriving (Show, Eq)

{-
    Data type for a processed road unit. These: are index for
    constant retrieval in unordered containers; include the unit
    task to perform, and in the case of deceleration also the
    deceleration configuration and the current it harvests.
-}
data ProcessedRoadUnit =
          PickPaceRoadUnit
            { pruId       :: Int
            , pruUnitTask :: UnitTask
            , pruRoadUnit :: RoadUnit
            } 
        --
        | DropRoadUnit
            { pruId         :: Int
            , pruRoadUnit   :: RoadUnit
            , pruDecConfig  :: DecConfig
            , pruCurrEnergy :: CurrentEnergy
            }
        --
        deriving (Eq)
--
instance Show ProcessedRoadUnit where
    show pru = 
        case pru of
            PickPaceRoadUnit{} -> show (pruId pru) ++ ") " ++ show (pruUnitTask pru) ++ show (pruRoadUnit pru)
            DropRoadUnit{} -> show (pruId pru) ++ ") " ++ show DROP ++ show (pruRoadUnit pru)
--
instance Hashable ProcessedRoadUnit where
    hash = pruId
    hashWithSalt salt pru = hashWithSalt salt (hash pru)


type ProcessedItinerary = [ProcessedRoadUnit]

{-
    Data types for expansion of a processed road unit.
    These encapsulate possible power unit modes and power
    unit configuration for physically complete the road unit.
-}
type AccConfig = ((Time, Length), [(GearRatio, Speed)], [(Speed, Time, Length, PowerUnitConfig, PropulsionCostPerSecond)])
type DecConfig = ([(GearRatio, Speed)], [(Speed, CurrentEnergy, PowerUnitConfig)])
--
data Expansion =
          PaceExpansion
            { expFinalVehicleState :: VehicleState
            , expRoadUnit          :: ProcessedRoadUnit
            , expPUC               :: PowerUnitConfig
            , expTotalCost         :: Currency
            }
        | PickExpansion
            { expFinalVehicleState :: VehicleState
            , expRoadUnit          :: ProcessedRoadUnit
            , expAccConfig         :: AccConfig
            , expTotalCost         :: Currency
            }
        | DropExpansion
            { expFinalVehicleState :: VehicleState
            , expRoadUnit          :: ProcessedRoadUnit
            , expDecConfig         :: DecConfig
            }
        deriving (Show)
--
instance Hashable Expansion where
    hash = hash . expRoadUnit
    hashWithSalt salt exp = hashWithSalt salt (hash exp)

{-
    Safely computes the total cost of an expansion;
    this is zero in regenerative conditions (drop).
-}
expTotalCostSafe :: Expansion -> Currency
expTotalCostSafe DropExpansion{} = 0
expTotalCostSafe exp = expTotalCost exp