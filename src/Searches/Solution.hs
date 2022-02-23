{-
    Encapsulates data types and functions for the definition
    and manipulation of solutions to the search problems.
-}
module Searches.Solution where
--
import Commons.ISUnits
import Commons.Utilities
import Commons.Currency
import Commons.ColoredOutput
--
import Vehicle.Vehicle
import Vehicle.VehicleState
import VehicleDynamics.VehicleDynamics
--
import GearboxDynamics.Gearbox
import GearboxDynamics.GearboxDynamics
--
import PowerUnit.PowerUnitTypes
import PowerUnit.PowerUnit
import PowerUnit.PowerUnitCostCalculator
--
import Itinerary.Itinerary
import Itinerary.ProcessedItinerary
--
import Text.Printf (printf)
--
import Debug.Trace


{-
    Data type of a solution, a sequence of road unit
    expansions.
-}
newtype Solution = Solution [Expansion]

{-
    String representation of a solution.
-}
showSolution :: Vehicle -> Solution -> String
showSolution v (Solution []) = showInColor "EMPTY SOLUTION" RED
showSolution v (Solution es) = concatMap (prettyPrintExpansion v) es

{-
    Extracts the power unit mode from an expansion.
-}
expPuMode :: Expansion -> PowerUnitMode
expPuMode PaceExpansion{expPUC = puc} = powerUnitModeOfPowerUnitConfig puc
expPuMode PickExpansion{expAccConfig = (_, _, (_,_,_,puc,_):logs)} = powerUnitModeOfPowerUnitConfig puc
expPuMode _ = PUMPureEEngine

{-
    Computes the total cost of a solution, as the summation of
    the cost of running each selected expansion.
-}
solutionTotalCost :: [Expansion] -> Currency
solutionTotalCost [] = 0
solutionTotalCost (PaceExpansion{expTotalCost = c}:es) = c + solutionTotalCost es
solutionTotalCost (PickExpansion{expTotalCost = c}:es) = c + solutionTotalCost es
solutionTotalCost (_:es) = solutionTotalCost es -- In case of DropExpansion: no cost associated.

{-
    String representation of an expansion.
-}
prettyPrintExpansion :: Vehicle -> Expansion -> String
prettyPrintExpansion v (PaceExpansion vState pru puc cost) =
    printf
        "> {%s %s %s} <-> {%s} -> {%s %s} || %s\n"
        (showItemInColor (pruUnitTask pru) CYAN)
        (showItemInColor (fSpeedToKmHour $ ruSpeed $ pruRoadUnit pru) YELLOW)
        (showItemInColor (fLengthToKm $ ruLength $ pruRoadUnit pru) YELLOW)

        (show $ powerUnitModeOfPowerUnitConfig puc)
        -- (vehicleGearboxNaming v $ gearOfPowerUnitConfig puc)

        (showItemInColor (vehicleStateFuelMass vState) RED)
        (showItemInColor (vehicleStateChargeNow vState) GREEN)

        (showItemInColor cost MAGENTA)
--
prettyPrintExpansion v (PickExpansion vState pru accConf cost) =
    printf
        "> {%s %s %s} <-> {%s} -> {%s %s} || %s\n"
        (showItemInColor (pruUnitTask pru) CYAN)
        (showItemInColor (fSpeedToKmHour $ ruSpeed $ pruRoadUnit pru) YELLOW)
        (showItemInColor (fLengthToKm $ ruLength $ pruRoadUnit pru) YELLOW)

        (show $ powerUnitModeOfPowerUnitConfig $ let (_, _, (_,_,_,puc,_):xs) = accConf in puc)

        (showItemInColor (vehicleStateFuelMass vState) RED)
        (showItemInColor (vehicleStateChargeNow vState) GREEN)

        (showItemInColor cost MAGENTA)
--
prettyPrintExpansion v (DropExpansion vState pru decConf) =
    printf
        "> {%s %s %s} <-> {%s} -> {%s %s}\n"
        (showItemInColor DROP CYAN)
        (showItemInColor (fSpeedToKmHour $ ruSpeed $ pruRoadUnit pru) YELLOW)
        (showItemInColor (fLengthToKm $ ruLength $ pruRoadUnit pru) YELLOW)

        (show PUMPureEEngine)

        (showItemInColor (vehicleStateFuelMass vState) RED)
        (showItemInColor (vehicleStateChargeNow vState) GREEN)
