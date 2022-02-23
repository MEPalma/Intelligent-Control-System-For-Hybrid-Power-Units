{-# LANGUAGE CPP #-}
module PowerUnit.ICPowerUnit.ICEngine where
--
import Commons.ISUnits
import PowerUnit.ICPowerUnit.Fuel.Fuel
import PowerUnit.ICPowerUnit.Fuel.FuelConsts


{-|
    Computes the engine output power given its number of cylinders, revolutions per unit time,
    and mixture mass per combustion per cylinder.

    Returns best theoretical value assuming the combustion is adiabatic at constant pressure.

    @Integer: Number of cylinders in the engine.
    @Rev: Revolutions per unit time of the engine.
    @AirFuelMixture: amount of air and fuel entering the combustion chamber of each cylinder per combustion.

    @Return: The theoretical output power.
-}
fEnginePowerOut :: Integer -> Rev -> AirFuelMixture -> Temp -> Power
fEnginePowerOut nCylinders (HZ revs) airFuelMixture inletTemp =
    let _CYCLES_PER_REV_4_STROKE_ICENGINE = 0.5
        Kelvin flameTemp = fAdiabaticCombustionEnergy airFuelMixture inletTemp
        energyProductionFrequency = revs * _CYCLES_PER_REV_4_STROKE_ICENGINE * fromIntegral nCylinders
     in KWatt $ (energyProductionFrequency * flameTemp) / 1000
--
fEnginePowerOut nCylinders engineRevs airFuelMixture inletTemp = fEnginePowerOut nCylinders (fRevToHZ engineRevs) airFuelMixture (fTempToKelvin inletTemp)

{-
    Computes the engine's required stoichiometric air fuel mixture for the production of
    some amount of torque.

    @Ratio: compression ratio.
    @Temp: intake air temperature.
    @Torque: target torque delivery.
-}
fOttoCycleAFMForTorque ::
       Ratio
    -> Temp
    -> Torque
    -> AirFuelMixture
fOttoCycleAFMForTorque (Ratio cr) _t1 (Nm eT) =
    let Kelvin t1 = fTempToKelvin _t1
        Kelvin t3 = fTempToKelvin $ Celsius 8000
        --
        Kelvin t2 = Kelvin $ t1 * (cr ** (_AIR_K - 1))
        Kelvin t4 = Kelvin $ t3 * ((1 / cr) ** (_AIR_K - 1))
        --
        -- Temperature component
        Kelvin tempCmp = Kelvin $ t3 - t2 - t4 + t1
        --
        airUnitMass = Kg $ eT / (tempCmp * _AIR_HEAT_CAPACITY_CONST_VOLUME)
        octUnitMass = fStoichiometricOctaneForAir airUnitMass
        --
        mixture = AirFuelMixture airUnitMass octUnitMass
     in mixture

{-# DEPRECATED mixtureForTorque "Use fOttoCycleAFMForTorque process instead" #-}
mixtureForTorque :: Torque -> AirFuelMixture
mixtureForTorque (Nm t) =
    let nOctaneMoles = t / _OCTANE_MOLAR_HEAT_COMBUSTION
        octMass = Gram $ nOctaneMoles * _OCTANE_MOLAR_MASS
        --
        airMass = fStoichiometricAirForOctane octMass
     in AirFuelMixture airMass octMass

{-# DEPRECATED mixtureForTorqueAdjusted "Use fOttoCycleAFMForTorque process instead" #-}
mixtureForTorqueAdjusted :: Torque -> Double -> AirFuelMixture
mixtureForTorqueAdjusted (Nm t) eff | eff <= 1 = mixtureForTorque (Nm $ t * (1 + eff))
                                    | otherwise = mixtureForTorque (Nm $ t * eff)

{-|
    Calculates the cylinder's swept volume: volume traversed
    when transitioning from BDC to TDC (or vice versa).

    @Length: bore of the piston.
    @Length: stroke of the piston.

    @Return: piston swept volume.
-}
{-# DEPRECATED fPistonSweptVolume "Use fOttoCycleAFMForTorque process instead" #-}
fPistonSweptVolume :: Length -> Length -> Volume
fPistonSweptVolume (Meter bore) (Meter stroke) = Meter3 $ (pi / 4) * (bore ** 2) * stroke
fPistonSweptVolume bore stroke = fPistonSweptVolume (fLengthToMeter bore) (fLengthToMeter stroke)

{-|
    Computes the cylinder minimum volume given its compression ratio, bore, stroke.

    Computes:
            (pi / 4) * (bore ^ 2) * stroke
    Vmin = --------------------------------
                     cr - 1

    Derived from:

          ((pi / 4) * (bore ^ 2) * stroke) + vmin
    cr = -----------------------------------------
                         vmin

    @Length: Piston's bore.
    @Length: Piston's stroke.
    @Ratio:  Cylinder/Engine's compression ratio.

    @Return: Cylinder's minimum volume.
-}
{-# DEPRECATED fCylinderMinVolume "Use fOttoCycleAFMForTorque process instead" #-}
fCylinderMinVolume :: Length -> Length -> Ratio -> Volume
fCylinderMinVolume (Meter bore) (Meter stroke) (Ratio cr) =
    let Meter3 sweptVolume = fPistonSweptVolume (Meter bore) (Meter stroke)
     in Meter3 $ sweptVolume / (cr - 1)
fCylinderMinVolume bore stroke cr = fCylinderMinVolume (fLengthToMeter bore) (fLengthToMeter stroke) cr

{-|
    Computes the free volume in the cylinder when the piston is
    at BCD.
    Hence calculates the minimum volume plus the stroke volume.

    @Length: piston bore.
    @Length: piston stroke.
    @Ratio:  piston compression ratio.

    @Return: free volume when piston at BDC.
-}
{-# DEPRECATED fCylinderMaxVolume "Use fOttoCycleAFMForTorque process instead" #-}
fCylinderMaxVolume :: Length -> Length -> Ratio -> Volume
fCylinderMaxVolume (Meter bore)
                   (Meter stroke)
                   (Ratio cr)
                   = let Meter3 volumeTDC = fCylinderMinVolume (Meter bore) (Meter stroke) (Ratio cr)
                         Meter3 volumeBDC = fPistonSweptVolume (Meter bore) (Meter stroke)
                     in Meter3 $ volumeTDC + volumeBDC
fCylinderMaxVolume bore stroke cr = fCylinderMaxVolume (fLengthToMeter bore) (fLengthToMeter stroke) cr

{-|
Otto Cycle Output Power
ottoCycleOutputPower nCylinders bore stroke minEncVolume rpm inAirTemp inAirPress maxCycleTemp = HP 0
-}
{-# DEPRECATED fOttoCycleOutputPower "Use fOttoCycleAFMForTorque process instead" #-}
fOttoCycleOutputPower :: Integer -> Length -> Length -> Ratio -> Rev -> Temp -> Pressure -> Temp -> Power
fOttoCycleOutputPower nCylinders
                      (Meter bore)
                      (Meter stroke)
                      (Ratio compRatio)
                      (HZ rev)
                      (Kelvin t1)
                      (Pascal inAirPress)
                      (Kelvin t3)
                      = fPowerToHP $ KWatt powerOut
                      where
                            _AIR_HEAT_CAPACITY_CONST_PRESSURE = 1005.0 -- J/kg.K
                            _AIR_HEAT_CAPACITY_CONST_VOLUME = 718.0 -- J/kg.K
                            _AIR_K = _AIR_HEAT_CAPACITY_CONST_PRESSURE / _AIR_HEAT_CAPACITY_CONST_VOLUME
                            --
                            _AIR_MOLAR_MASS = Kg 0.02897
                            --
                            _AIR_IDEAL_GAS_CONSTANT = 287.058 -- Pascal*m3 / celsius or J/KgK

                            workPerCylinder = _AIR_HEAT_CAPACITY_CONST_VOLUME * (t3 - t2 - t4 + t1) --J/Kg
                                            where Kelvin t2 = Kelvin $ t1 * (compRatio ** (_AIR_K - 1))
                                                  Kelvin t4 = Kelvin $ t3 * ((1 / compRatio) ** (_AIR_K - 1))

                            Kg intakeAirMass = let Meter3 volumeOfEngine = Meter3 $ fromIntegral nCylinders * (pi / 4) * (bore ** 2) * stroke
                                               in Kg $ (inAirPress * volumeOfEngine) / (_AIR_IDEAL_GAS_CONSTANT * t1)

                            _CYCLES_PER_REV_4_STROKE_ICENGINE = 0.5

                            KWatt powerOut = KWatt $ rev * _CYCLES_PER_REV_4_STROKE_ICENGINE * workPerCylinder * intakeAirMass / 1000
-- -- Units conversion step.
fOttoCycleOutputPower nCylinders _bore _stroke (Ratio compRatio) _rev _t1 _inAirPress _t3
                      = fOttoCycleOutputPower nCylinders bore stroke (Ratio compRatio) rev t1 inAirPress t3
                      where bore       = fLengthToMeter _bore
                            stroke     = fLengthToMeter _stroke
                            rev        = fRevToHZ _rev
                            t1         = fTempToKelvin _t1
                            inAirPress = fPressureToPascal _inAirPress
                            t3         = fTempToKelvin _t3
