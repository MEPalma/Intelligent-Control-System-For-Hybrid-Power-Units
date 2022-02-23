{-
    Encapsulates the data types and functions for the
    definition of manipulation of vehicles.
-}
module Vehicle.Vehicle where
--
import Commons.ISUnits
import Commons.Currency
--
import Vehicle.VehicleState
--
import GearboxDynamics.Gearbox
--
import PowerUnit.PredictionStrategy
--
import PowerUnit.PowerUnitTypes
import PowerUnit.ICPowerUnit.ICPowerUnitType
import PowerUnit.ICPowerUnit.Fuel.Fuel
import PowerUnit.ICPowerUnit.ICTel
import KNNRegression.KNN
--
import PowerUnit.EPowerUnit.EPowerUnitType
import PowerUnit.EPowerUnit.Electricity
--
import MultivariateRegression.MultivariateRegression

{-
    Data type of a vehicle.
-}
data Vehicle =
        Vehicle
            { vehicleNakedMass     :: Mass
            , vehicleGearbox       :: Gearbox
            , vehicleGearboxNaming :: GearRatio -> String
            , vehicleDiff          :: GearRatio
            , vehicleWheelRadius   :: Length
            , vehicleDragCoeff     :: Double
            , vehicleFrontalArea   :: Area
            , vehicleRollingCoeff  :: Double
            , vehiclePowerUnit     :: PowerUnit
            , vehicleFuelUnitCostPerLiter        :: FuelUnitCostPerLiter
            , vehicleElectricityCostPerKWattHour :: ElectricityCostPerKWattHour
            , vehicleState :: VehicleState
            }

{-
    Returns the mass of a vehicle, as the summation of its
    naked mass and the fuel it is carrying.
-}
vehicleMass :: Vehicle -> Mass
vehicleMass v =
    let Kg nakedMass = (fMassToKg . vehicleNakedMass) v
        Kg fuelMass = (fMassToKg . vehicleStateFuelMass . vehicleState) v
     in Kg $ nakedMass + fuelMass

{-
    Computes the string representation of a gear ratio.
-}
genericGearNaming :: Gearbox -> GearRatio -> String
genericGearNaming = inner 1
    where
    inner :: Int -> Gearbox -> GearRatio -> String
    inner _ [] _ = "Unknown-Gear"
    inner n (g:gs) g'
        | g == g' = "Gear: " ++ show g'
        | otherwise = inner (n+1) gs g'

{-
    Composite function for the retrieval of a vehicle's
    IC power unit.
-}
vehicleICPowerUnit :: Vehicle -> ICPowerUnit
vehicleICPowerUnit = puICPowerUnit . vehiclePowerUnit

{-
    Composite function for the retrieval of a vehicle's
    electric power unit.
-}
vehicleEPowerUnit :: Vehicle -> EPowerUnit
vehicleEPowerUnit = puEPowerUnit . vehiclePowerUnit

{-
    Composite function for the retrieval of a vehicle's
    current state of charge (SOC).
-}
vehicleSOC :: Vehicle -> Integer
vehicleSOC = vehicleStateSOC . vehicleState

{-
    Composite function for the retrieval of a vehicle's
    current speed.
-}
vehicleCurrentSpeed :: Vehicle -> Speed
vehicleCurrentSpeed = vehicleStateSpeed . vehicleState

{-
    Composite function for the updating of a vehicle's
    current speed.
-}
updateVehicleCurrentSpeed :: Vehicle -> Speed -> Vehicle
updateVehicleCurrentSpeed v s = v{vehicleState = (vehicleState v){vehicleStateSpeed = s}}
