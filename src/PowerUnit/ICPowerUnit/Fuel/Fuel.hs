{-
    Encapsulate data types and functions for
    the computation of fuel mixtures and fuel
    flows a spark ignition engines.
-}
module PowerUnit.ICPowerUnit.Fuel.Fuel where
--
import Commons.ISUnits
import Commons.Currency
--
import PowerUnit.ICPowerUnit.Fuel.FuelConsts
--
import Text.Printf (printf)


data AirFuelMixture = AirFuelMixture {airMass :: Mass, fuelMass :: Mass} deriving (Eq, Ord)
--
instance Show AirFuelMixture where
    show (AirFuelMixture (Gram a) (Gram f)) = printf "AirFuelMixture (Air: %s, Octane: %s, AFR: %s) " (show (Gram a)) (show (Gram f)) (show (fMixtureAFR (AirFuelMixture (Gram a) (Gram f))))
    show airFuelMixture = show $ fAirFuelMixtureInGram airFuelMixture


data MixtureFlow = MixtureFlow {airFlow :: MassFlow, fuelFlow :: MassFlow} deriving (Eq)
--
instance Ord MixtureFlow where
    compare (MixtureFlow _ (GramPerSecond ff1)) (MixtureFlow _ (GramPerSecond ff2)) = compare ff1 ff2
    compare m1 m2 = compare (fMixtureFlowInGrams m1) (fMixtureFlowInGrams m2)
--
instance Show MixtureFlow where
    show (MixtureFlow (GramPerSecond af) (GramPerSecond ff)) = printf "MixtureFlow (Air: %s, Octane: %s)" (show (GramPerSecond af)) (show (GramPerSecond ff))
    show mixtureFlow = show $ fMixtureFlowInKg mixtureFlow
--
_NO_MIXTURE_FLOW = MixtureFlow (GramPerSecond 0) (GramPerSecond 0)

{-| Converts the both quantities of the AirFuelMixture to Kg Mass. -}
fAirFuelMixtureInKg :: AirFuelMixture -> AirFuelMixture
fAirFuelMixtureInKg (AirFuelMixture (Kg airMass) (Kg fuelMass)) = AirFuelMixture (Kg airMass) (Kg fuelMass)
fAirFuelMixtureInKg (AirFuelMixture airMass fuelMass) = AirFuelMixture (fMassToKg airMass) (fMassToKg fuelMass)


{-| Converts the both quantities of the AirFuelMixture to Gram Mass. -}
fAirFuelMixtureInGram :: AirFuelMixture -> AirFuelMixture
fAirFuelMixtureInGram (AirFuelMixture (Gram airMass) (Gram fuelMass)) = AirFuelMixture (Gram airMass) (Gram fuelMass)
fAirFuelMixtureInGram (AirFuelMixture airMass fuelMass) = AirFuelMixture (fMassToGram airMass) (fMassToGram fuelMass)


{-| Converts the both quantities of the AirFuelMixture to Kg Mass. -}
fMixtureFlowInKg :: MixtureFlow -> MixtureFlow
fMixtureFlowInKg (MixtureFlow (KgPerSecond af) (KgPerSecond fuelMass)) = MixtureFlow (KgPerSecond af) (KgPerSecond fuelMass) 
fMixtureFlowInKg (MixtureFlow af ff) = MixtureFlow (massFlowToKgPerSecond af) (massFlowToKgPerSecond ff)


{-| Converts the both quantities of the AirFuelMixture to Gram Mass. -}
fMixtureFlowInGrams :: MixtureFlow -> MixtureFlow
fMixtureFlowInGrams (MixtureFlow (GramPerSecond af) (GramPerSecond ff)) = MixtureFlow (GramPerSecond af) (GramPerSecond ff)
fMixtureFlowInGrams (MixtureFlow af ff) = MixtureFlow (massFlowToGramsPerSecond af) (massFlowToGramsPerSecond ff)


{-| Computes the AFR of the AirFuelMixture. It converts quantities to Kg. -}
fMixtureAFR :: AirFuelMixture -> Ratio
fMixtureAFR (AirFuelMixture (Kg a) (Kg f)) = Ratio $ a / f
fMixtureAFR airFuelMixture = fMixtureAFR $ fAirFuelMixtureInKg airFuelMixture


fFuelFlowToFuelConsumption :: MixtureFlow -> Speed -> FuelConsumption
fFuelFlowToFuelConsumption (MixtureFlow _ (KgPerSecond ff)) (MeterSecond speed) =
    let kmPerSecond = speed / 1000 -- Km/Second
        literPerSecond = ff * _OCTANE_LITER_PER_KG -- l/sec = kg/sec * l/kg
     in KmPerLiter $ kmPerSecond / literPerSecond -- km/l = km/sec * sec/l
fFuelFlowToFuelConsumption mix speed = fFuelFlowToFuelConsumption (fMixtureFlowInKg mix) (fSpeedToMeterSecond speed)


{-|
    Calculates the energy delta and heat of flame for an adiabatic combustion.

    Assumes adiabatic combustion with balanced formula (in kmol):
        (1)[C8H18] + (12.5)[O2] -> (8)[CO2] + (9)[H2O]

        Any extra amount of octane or air (which cannot take part in the combustion)
    is modelled as coolant for the reaction.

    @Mass: mass of Octane C8H18 in the mixture.
    @Temp: temperature of the fuel in the mixture.
    @Mass: mass od air in the mixture (21% oxygen, 79% nitrogen).
    @Temp: temperature of the air in the mixture.

    @Return: a tuple containing the energy delta of the combustion's reactants
             and products, and the peak adiabatic flame temperature.
-}
fAdiabaticCombustionEnergy :: AirFuelMixture -> Temp -> Temp
fAdiabaticCombustionEnergy (AirFuelMixture (Kg octaneKg) (Kg airKg)) (Kelvin tempIn)
                           = let
                                -- Compute number of kMol in fuel mass and air mass.
                                KMol octKmol = fKMolesOctaneInFuel (Kg octaneKg)
                                (KMol oxyKmol, KMol nitKmol) = fKMolesOxygenAndNitrogenInAir (Kg airKg)

                                -- ------------------- --
                                -- Energy of reactants --
                                -- ------------------- --
                                KJoules qReactants = let dt = (tempIn - _HEAT_CAPACITY_REF_TEMP)
                                                         allReactants =
                                                            [
                                                                octKmol * (_OCTANE_HEAT_FORMATION   + (_HEAT_CAPACITY_C8H18 * dt))
                                                            ,   oxyKmol * (_OXYGEN_HEAT_FORMATION   + (_HEAT_CAPACITY_O2 * dt))
                                                            ,   nitKmol * (_NITROGEN_HEAT_FORMATION + (_HEAT_CAPACITY_N2 * dt))
                                                            ]
                                                     in KJoules $ sum allReactants


                                -- Maximum number of combustible pairs of octane and oxygen.
                                -- Every molecule of octane needs 12.5 molecules of oxygen
                                -- to produce 8 molecules of CO2 and 9 of H20:
                                -- 1[C8H18] + 12.5[O2] --> 8[CO2] + 9[H2O]
                                nBalPairs = min octKmol (oxyKmol / 12.5)


                                -- Number of moles of each product.
                                nsAllProducts = [  -- New products.
                                                nBalPairs * 8 -- CO2
                                             ,  nBalPairs * 9 -- H20
                                                --
                                                -- Unburned N2.
                                             ,  nitKmol -- N2
                                                --
                                                -- Unburned C8H18 and O2
                                             ,  octKmol - nBalPairs -- C8H18
                                             ,  oxyKmol - (nBalPairs * 12.5) -- O2
                                             ]
                                --
                                -- Sum of all heat of formations.
                                hfProducts = let hfs =  [
                                                            -- New products.
                                                            _CO2_HEAT_FORMATION -- CO2
                                                        ,   _WATER_HEAT_FORMATION -- H2O
                                                            --
                                                            -- Unburned N2.
                                                        ,   _NITROGEN_HEAT_FORMATION -- N2
                                                            --
                                                            -- Unburned C8H18 and O2
                                                        ,   _OCTANE_HEAT_FORMATION -- C8H18
                                                        ,   _OXYGEN_HEAT_FORMATION -- O2
                                                        ]
                                            in sum [x * y | (x, y) <- zip nsAllProducts hfs]
                                --
                                -- Products of all number of molecules and heat capacities.
                                nCpAllProducts = let nCps =  [
                                                               -- New products.
                                                               _HEAT_CAPACITY_CO2 -- CO2
                                                             , _HEAT_CAPACITY_H2O -- H2O
                                                               --
                                                               -- Unburned N2.
                                                             , _HEAT_CAPACITY_N2 -- N2
                                                               --
                                                               -- Unburned C8H18 and O2
                                                             , _HEAT_CAPACITY_C8H18 -- C8H18
                                                             , _HEAT_CAPACITY_O2 -- O2
                                                             ]
                                              in [x * y | (x, y) <- zip nsAllProducts nCps]
                                --
                                -- Sum of all terms: (n_i)(cp_i)
                                nCpProducts = sum nCpAllProducts
                                --
                                -- Sum of all terms: (n_i)(cp_i)(refTemperature)
                                nCpRefProducts = sum [x * _HEAT_CAPACITY_REF_TEMP | x <- nCpAllProducts]
                                --
                                -- Adiabatic Flame Temperature.
                                Kelvin tad = Kelvin $ (qReactants + hfProducts - nCpRefProducts) / nCpProducts
                                --
                                in Kelvin tad

fAdiabaticCombustionEnergy afMixture inletTemp = fAdiabaticCombustionEnergy (fAirFuelMixtureInKg afMixture) (fTempToKelvin inletTemp)


{-|
    Computes the mass of air needed to allow for a stoichiometric
    combustion with octane to occur.

    Assumes air is made of 21% Oxygen (O2) and 79% Nitrogen (N2).

    @Mass: Mass of octane.

    @Return: Mass of air.
-}
fStoichiometricAirForOctane :: Mass -> Mass
fStoichiometricAirForOctane _octMass =
    let Kg octMass = fMassToKg _octMass
        --
        noct = octMass / _OCTANE_MOLAR_MASS
        --
        oxyMass = noct * 12.5 * _OXYGEN_MOLAR_MASS
        nitMass = noct * 12.5 * (79/21) * _NITROGEN_MOLAR_MASS
        --
     in Kg $ oxyMass + nitMass


{-|
    Computes the mass of fuel needed to stoichiometrically
    match the amount of oxygen moles in the mass of air given.

    @Mass: Mass of air.

    @Return: Stoichiometric octane mass.
-}
fStoichiometricOctaneForAir :: Mass -> Mass
fStoichiometricOctaneForAir _airMass =
    let Kg airMass = fMassToKg _airMass
        (KMol oxyKmol, _) = fKMolesOxygenAndNitrogenInAir _airMass
        -- 1 Kmol of octane per 12.5 Kmol of oxygen.
        KMol octKmol = KMol $ oxyKmol / 12.5
     in Kg $ octKmol * _OCTANE_MOLAR_MASS


{-|
    Computes the number of kmol of octane (C8H18) in the given octane mass.

    Assumes the mixture is pure octane C8H18

    @Mass: mass of octane fuel.

    @Return: kmol of octane.
-}
fKMolesOctaneInFuel :: Mass -> Mole
fKMolesOctaneInFuel (Kg fuelMass) = KMol $ fuelMass / _OCTANE_MOLAR_MASS
fKMolesOctaneInFuel fuelMass = fKMolesOctaneInFuel $ fMassToKg fuelMass


{-|
    Computes the number of kmol of oxygen (O2) and nitrogen (N2)
    in the given mass of air.

    Assumes air is made of 21% Oxygen (O2) and 79% Nitrogen (N2).

    @Mass: mass of air.

    @Return: (kmol of oxygen, kmol of nitrogen).
-}
fKMolesOxygenAndNitrogenInAir :: Mass -> (Mole, Mole)
fKMolesOxygenAndNitrogenInAir (Kg airMass) =
    let multiplier = airMass / (_OXYGEN_MOLAR_MASS + ((79/21) * _NITROGEN_MOLAR_MASS))
     in (KMol multiplier, KMol $ (79/21) * multiplier)
fKMolesOxygenAndNitrogenInAir airMass = fKMolesOxygenAndNitrogenInAir $ fMassToKg airMass

{-|
    Computes the Kg of substance going into the cylinder,
    given the engine speed and number of cylinders, and
    the mass flow of the substance.

    If assumes the engine is a 4 stroke engine.

    @MassFlow: mass flow of the substance.
    @Rev: speed of the engine

    @Return: Kg entering the each cylinder at every instant.
-}
flowToValuePerCylinder :: MassFlow -> Integer -> Rev -> Mass
flowToValuePerCylinder (KgPerSecond mps) n (HZ rev) =
    let _CYCLES_PER_REV_4_STROKE_ICENGINE = 0.5
        massPerRev = mps / rev -- Kg/Revolution
        nFiresPerRev = fromIntegral n * _CYCLES_PER_REV_4_STROKE_ICENGINE -- fire/Revolution
        massPerCylinder = massPerRev / nFiresPerRev -- Kg/fire
    in Kg massPerCylinder
flowToValuePerCylinder v n revs = flowToValuePerCylinder (massFlowToKgPerSecond v) n (fRevToHZ revs)



mixtureToFlow :: AirFuelMixture -> Integer -> Rev -> MixtureFlow
mixtureToFlow (AirFuelMixture (Gram am) (Gram fm)) nCyl (HZ rev) =
    let _CYCLES_PER_REV_4_STROKE_ICENGINE = 0.5
        nFiresPerRev = fromIntegral nCyl * _CYCLES_PER_REV_4_STROKE_ICENGINE -- fire/rev
        --
        nFiresPerSecond = nFiresPerRev * rev -- fire/rev * rev/second = fire/second
        --
        af = GramPerSecond $ nFiresPerSecond * am -- fire/second * mass/fire = mass/second
        ff = GramPerSecond $ nFiresPerSecond * fm -- fire/second * mass/fire = mass/second
    in MixtureFlow af ff
mixtureToFlow afm nCyl rev = mixtureToFlow (fAirFuelMixtureInGram afm) nCyl (fRevToHZ rev)


mixtureToFlowCorrected :: AirFuelMixture -> Integer -> Rev -> Double -> MixtureFlow
mixtureToFlowCorrected mix nCyl rev eff =
    let MixtureFlow (GramPerSecond teoAirMassFlow) (GramPerSecond fuelMassFlow) = mixtureToFlow mix nCyl rev
     in MixtureFlow (GramPerSecond (teoAirMassFlow * eff)) (GramPerSecond (fuelMassFlow * eff))


mixtureFlowToTotalFuelMass :: MixtureFlow -> Time -> Mass
mixtureFlowToTotalFuelMass (MixtureFlow af (KgPerSecond ff)) (Seconds t) = Kg $ ff * t
mixtureFlowToTotalFuelMass mf t = mixtureFlowToTotalFuelMass (fMixtureFlowInKg mf) (fTimeToSeconds t)




newtype FuelUnitCostPerLiter = FuelUnitCostPerLiter {fucplUnitCost :: Currency} deriving (Eq, Ord)
--
instance Show FuelUnitCostPerLiter where
    show (FuelUnitCostPerLiter fc) = printf "Fuel-Cost/Liter: (%s)" (show fc)


mixtureFlowToUnitCostFlow :: MixtureFlow -> FuelUnitCostPerLiter -> Currency
mixtureFlowToUnitCostFlow mf fucpl =
    let unitCurr = fucplUnitCost fucpl
        KgPerSecond ff = fuelFlow $ fMixtureFlowInKg mf -- Kg/Sec
        --
        literPerSecond = fromRational $ toRational $ ff * _OCTANE_LITER_PER_KG -- Liter/Sec -> Kg/Sec * Lit/Kg
        --
        pricePerSecond = unitCurr * literPerSecond -- Price/Sec -> Liter/Sec * Price/Liter
     in pricePerSecond


fuelMassCost :: FuelUnitCostPerLiter -> Mass -> Currency
fuelMassCost fucpl fuelM =
    let unitCurr = fucplUnitCost fucpl -- Cost/Liter
        --
        -- Cost per Kg.
        costPerKg = unitCurr * fromRational(toRational _OCTANE_LITER_PER_KG) -- Cost/Kg = Cost/Liter * Liter.Kg
        --
        Kg massUsed = fMassToKg fuelM
     in costPerKg * fromRational(toRational massUsed) -- cost = cost/kg * kg