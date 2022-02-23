module VehicleDynamics.AeroDrag where

import Commons.ISUnits
import Vehicle.Vehicle
import Itinerary.Itinerary

{-| Specific gas constant of dry air in j/Kgk. -}
_SGC_DRY_AIR = 287.058 -- j/Kgk

{-| Specific gas constant of water vapour in j/Kgk. -}
_SGC_WATER_VAPOUR = 461.495 -- j/Kgk


{-|
    Calculates the air density given its temperature,
    pressure and humidity.

    @Pressure: The pressure of dry air.
    @Pressure: The pressure of water vapour.
    @Temp: The air temperature.

    @Return: The air density.
-}
fAirDensity :: Pressure -> Pressure -> Temp -> Density
fAirDensity (Pascal dryAirPreassure)
            (Pascal vapourPreassure)
            (Kelvin airTemp)
            = let Kg_Meter3 dryAirDens = Kg_Meter3 $ dryAirPreassure / (_SGC_DRY_AIR      * airTemp)
                  Kg_Meter3 vapourDens = Kg_Meter3 $ vapourPreassure / (_SGC_WATER_VAPOUR * airTemp)
              in Kg_Meter3 $ dryAirDens + vapourDens

fAirDensity dryAirPreassure vapourPreassure airTemp = fAirDensity (fPressureToPascal dryAirPreassure) (fPressureToPascal vapourPreassure) (fTempToKelvin airTemp)

{-|
    Calculates the force opposing the motion of an object
    in terms of the aerodynamic drag acting on its frontal
    area.

    @Density: Air density.
    @Speed: Speed at which the object is travelling,
            or the speed of the air compared to the speed of the vehicle
    @Double: The drag coefficient of the vehicle.
    @Area: The frontal area of the vehicle.

    @Returns the force acting against the vehicle's motion.
-}
fAeroDrag :: Density -> Speed -> Double -> Area -> Force
fAeroDrag (Kg_Meter3 airDensity)
          (MeterSecond speed)
          dragCoeff
          (Meter2 frontArea)
          = Newton $ 0.5 * airDensity * (speed ** 2) * dragCoeff * frontArea -- TODO: subtract wind speed
--
fAeroDrag airDensity speed dragCoeff frontArea = fAeroDrag airDensity (fSpeedToMeterSecond speed) dragCoeff frontArea


{-|
    Calculates the force opposing the motion of an object
    in terms of the aerodynamic drag acting on its frontal
    area.
-}
aeroDrag2 :: Vehicle -> RoadUnit -> Force
aeroDrag2 v ru = let ac = ruAmbient ru
                     Kg_Meter3 airDensity = fAirDensity (acDryAirPressure ac) (acVapourPressure ac) (acTemp ac)
                 in fAeroDrag (Kg_Meter3 airDensity) (ruSpeed ru) (vehicleDragCoeff v) (vehicleFrontalArea v)