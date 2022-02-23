{-
    Encapsulates function for the computation of expected running consts
    of a spark ignition engine.
-}
module PowerUnit.ICPowerUnit.ICPowerUnit where
--
import Commons.ISUnits
--
import PowerUnit.PowerUnitTypes
import PowerUnit.ICPowerUnit.ICPowerUnitType
import PowerUnit.ICPowerUnit.Fuel.Fuel


{-
    Converts an engine's fuel consumption to costs per second.
-}
fICEngineFuelConsumptionToPropulsionCostPerSecond :: FuelConsumption -> Speed -> FuelUnitCostPerLiter -> PropulsionCostPerSecond
fICEngineFuelConsumptionToPropulsionCostPerSecond fc _vSpeed fcpl =
    let costPerLiter = fucplUnitCost fcpl -- Cost/Liter
        KmPerLiter kmPerLiter = fFuelConsumptionToKmPerLiter fc -- Km/Liter
        meterPerLiter = kmPerLiter * 1000 -- Meter/Liter -> Km/Liter * m/km -> km/Liter * 1000
        MeterSecond meterPerSecond = fSpeedToMeterSecond _vSpeed -- Meter/Second
        --
        literPerSecond = fromRational $ toRational $ (1 / meterPerLiter) * meterPerSecond
        --
        costPerSecond = costPerLiter * literPerSecond
    in PropulsionCostPerSecond costPerSecond

{-
    Computes the total mass of octane burned for operating
    an engine and some given regimes for some amount of time.
-}
octaneMassBurned :: ICPowerUnitConfig -> Time -> Mass
octaneMassBurned icpuConfig _tApp =
    let KgPerSecond fMassPerSecond = fuelFlow $ fMixtureFlowInKg $ icpucMixture icpuConfig
        --
        Seconds tApp = fTimeToSeconds _tApp
        --
        totMass = Kg $ fMassPerSecond * tApp
        --
     in totMass