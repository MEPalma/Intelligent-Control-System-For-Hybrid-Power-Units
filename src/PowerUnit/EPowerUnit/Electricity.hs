{-
    Encapsulated data types and functions for the
    modelling of electricity utilization of an
    induction motor in power and harvest mode.
-}
module PowerUnit.EPowerUnit.Electricity where
--
import Commons.ISUnits
import Commons.Currency
--
import Text.Printf (printf)


newtype ElectricityCostPerKWattHour =
            ElectricityCostPerKWattHour
                { ecpkwaUnitCost :: Currency
                }
                deriving (Eq, Ord)
--
instance Show ElectricityCostPerKWattHour where
    show (ElectricityCostPerKWattHour fc) = printf "Cost/KWh: (%s)" (show fc)

{-
    Computes the total cost of some current energy.
-}
currentEnergyCost :: ElectricityCostPerKWattHour -> CurrentEnergy -> Currency
currentEnergyCost ecpkwa _currEnergy =
    let curr = ecpkwaUnitCost ecpkwa -- cost/kwh
        KWattHour currEnergy = currentEnergyToKWattHour _currEnergy
     in curr * fromRational (toRational currEnergy)