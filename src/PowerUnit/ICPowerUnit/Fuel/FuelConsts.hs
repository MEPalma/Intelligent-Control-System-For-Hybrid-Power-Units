{-
    Collection of fuel related constants.
-}
module PowerUnit.ICPowerUnit.Fuel.FuelConsts where
--
import Commons.ISUnits

-- Define constant for standard enthalpy heat of formation
--
_HEAT_FORMATION_TEMP     = 298.150  -- Kelvin
--
_OCTANE_HEAT_FORMATION   = 249952.000 -- kJ/kmol At 298.150°K
_OXYGEN_HEAT_FORMATION   = 0.000      -- kJ/kmol At 298.150°K
_NITROGEN_HEAT_FORMATION = 0.000      -- kJ/kmol At 298.150°K
_WATER_HEAT_FORMATION    = 241827.000 -- kJ/kmol At 298.150°K
_CO2_HEAT_FORMATION      = 393522.000 -- kJ/kmol At 298.150°K


-- Define standard heat capacity at 500 Kelvin. kJ/kmol or J/mol*K
_HEAT_CAPACITY_REF_TEMP = 298.15 -- Kelvin
--
_HEAT_CAPACITY_C8H18 = 255.68
_HEAT_CAPACITY_CO2   = 45.0
_HEAT_CAPACITY_H2O   = 35.0
_HEAT_CAPACITY_O2    = 30.0
_HEAT_CAPACITY_N2    = 30.0


-- Define constant molar masses (Kg/kmol) (g/mol)
_OCTANE_MOLAR_MASS   = 114.22852
_OXYGEN_MOLAR_MASS   = 31.9988
_NITROGEN_MOLAR_MASS = 28.0134


-- Molar Heat of Combustion as J/mol
_OCTANE_MOLAR_HEAT_COMBUSTION = 5474000.0 -- J/mol


-- Octane Kg per Liter.
_OCTANE_KG_PER_LITER = 0.91786
-- Octane liters per kg.
_OCTANE_LITER_PER_KG = recip _OCTANE_KG_PER_LITER
-- Octane weight of 1cc in grams.
_OCTANE_WEIGHT_1_CC_IN_GRAMS = _OCTANE_LITER_PER_KG
-- Octane weight of 1ml in grams.
_OCTANE_WEIGHT_1_ML_IN_GRAMS = _OCTANE_WEIGHT_1_CC_IN_GRAMS

-- Air constant head capacities.
_AIR_HEAT_CAPACITY_CONST_PRESSURE = 1005.0 -- J/kg.K
_AIR_HEAT_CAPACITY_CONST_VOLUME = 718.0 -- J/kg.K
_AIR_K = _AIR_HEAT_CAPACITY_CONST_PRESSURE / _AIR_HEAT_CAPACITY_CONST_VOLUME
