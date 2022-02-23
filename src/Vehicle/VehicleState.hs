{-
    Encapsulates data types and functions for the
    definition and derivation of a vehicle's state.
-}
module Vehicle.VehicleState where
--
import Commons.ISUnits

{-
    Data type for the definition of a vehicle's state.
-}
data VehicleState = VehicleState
    { vehicleStateSpeed :: Speed
    --
    , vehicleStateFuelMass :: Mass
    --
    , vehicleStateChargeNow :: CurrentEnergy
    , vehicleStateChargeMax :: CurrentEnergy
    } deriving (Show, Eq)

{-
    Computes the state of charge of the vehicle as an
    integer percentage.
-}
vehicleStateSOC :: VehicleState -> Integer
vehicleStateSOC VehicleState{vehicleStateChargeNow = _nowCharge, vehicleStateChargeMax = _maxCharge} =
    let KWattHour nowCharge = currentEnergyToKWattHour _nowCharge
        KWattHour maxCharge = currentEnergyToKWattHour _maxCharge
     in round $ (nowCharge / maxCharge) * 100