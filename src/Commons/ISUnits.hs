{-
    Data types for units of measure.
-}
module Commons.ISUnits where
--
import Data.Hashable
--
import Commons.UnitShow
import Commons.Currency
--
import Text.Printf (printf)

-- HLint pragma for camelCase
{-# ANN module "HLint: ignore Use camelCase" #-}

_EARTH_GRAVITY_ACCELERATION = 9.81


newtype Ratio = Ratio Double deriving (Eq, Ord)
instance Show Ratio where
    show (Ratio d) = showUnit4 d "" 

data Mole = Mol Double 
          | KMol Double
          deriving (Eq, Ord)
instance Show Mole where
    show (Mol a)  = showUnit4 a "mol"
    show (KMol a) = showUnit4 a "kmol"

fMoleToKMol :: Mole -> Mole
fMoleToKMol (Mol a)  = KMol $ a / 1000
fMoleToKMol (KMol a) = KMol a

fMoleToMol :: Mole -> Mole
fMoleToMol (Mol a)  = Mol a
fMoleToMol (KMol a) = Mol $ a * 1000


{-|
    Mass
-}
data Mass = Gram Double
            | Kg Double
            deriving (Eq, Ord)

instance Show Mass where
    show (Kg a)   = showUnit4 a "Kg"
    show (Gram a) = showUnit4 a "g"

fMassToKg :: Mass -> Mass
fMassToKg (Kg x) = Kg x
fMassToKg (Gram x) = Kg (x / 1000)

fMassToGram :: Mass -> Mass
fMassToGram (Gram x) = Gram x
fMassToGram (Kg x) = Gram (x * 1000)

{-|
    Length
-}
data Length = Mmeter Double
            | Cmeter Double
            | Meter Double
            | Kmeter Double
            | Mile Double
            deriving (Eq)

instance Show Length where
    show (Mmeter a) = showUnit4 a "mm"
    show (Cmeter a) = showUnit4 a "cm"
    show (Meter a)  = showUnit4 a "m"
    show (Kmeter a) = showUnit4 a "km"
    show (Mile a)   = showUnit4 a "miles"

fLengthToMeter :: Length -> Length
fLengthToMeter (Meter x)  = Meter x
fLengthToMeter (Mmeter x) = Meter (x / 1000)
fLengthToMeter (Cmeter x) = Meter (x / 100)
fLengthToMeter (Kmeter x) = Meter (x * 1000)
fLengthToMeter (Mile x)   = Meter (x / 0.00062137)

fLengthToKm :: Length -> Length
fLengthToKm x = let Meter l = fLengthToMeter x
                 in Kmeter $ l / 1000



{-|
    Volume
-}
data Volume = Cmeter3 Double
            | Meter3 Double
            | Liter Double
            deriving (Eq)

instance Show Volume where
    show (Cmeter3 a) = showUnit4 a "cm3"
    show (Meter3 a)  = showUnit4 a "m3"
    show (Liter a)   = showUnit4 a "L"

fVolumeToMeter3 :: Volume -> Volume
fVolumeToMeter3 (Meter3 x) = Meter3 x
fVolumeToMeter3 (Cmeter3 x) = Meter3 (x * 10^6)
fVolumeToMeter3 (Liter x) = Meter3 (x * 0.001)

newtype Area = Meter2 Double deriving (Eq, Ord)
instance Show Area where
    show (Meter2 a) = showUnit4 a "m2"

{-|
    Torque
-}
newtype Torque = Nm Double
--
instance Show Torque where
    show (Nm a) = showUnit4 a "Nm"
--
instance Eq Torque where
    (==) _a _b =
        let Nm a = _a
            Nm b = _b
         in (==) a b
    --
    (/=) _a _b = not $ (==) _a _b
--
instance Ord Torque where
    compare _a _b =
        let Nm a = _a
            Nm b = _b
         in compare a b
--
instance Hashable Torque where
    hash (Nm t) = hash t
    hashWithSalt salt (Nm t) = hashWithSalt salt t


{-|
    p = t * w | w = 2pi * revPerSecond
-}
fTorqueToKWatt :: Torque -> Rev -> Power
fTorqueToKWatt (Nm t) (HZ r) = KWatt $ t * r * 2 * pi / 1000
fTorqueToKWatt t r = fTorqueToKWatt t (fRevToHZ r)

fTorqueToHP :: Torque -> Rev -> Power
fTorqueToHP t r = fPowerToHP $ fTorqueToKWatt t r

{-|
    Power
-}
data Power = KWatt Double
           | Watt Double
           | HP Double
--
instance Show Power where
    show (KWatt a) = showUnit4 a "kW"
    show (Watt a)  = showUnit4 a "W"
    show (HP a)    = showUnit4 a "HP"
--
instance Eq Power where
    (==) _p _p' =
        let KWatt p = fPowerToKWatt _p
            KWatt p' = fPowerToKWatt _p'
         in (==) p p'
    --
    (/=) _p _p' = not $ (==) _p _p'
--
instance Ord Power where
    compare _p _p' =
        let KWatt p = fPowerToKWatt _p
            KWatt p' = fPowerToKWatt _p'
         in compare p p'

fPowerToHP :: Power -> Power
fPowerToHP (HP x) = HP x
fPowerToHP (Watt x) = fPowerToHP $ fPowerToKWatt (Watt x)
fPowerToHP (KWatt x) = HP (x / 0.745699872)

fPowerToKWatt :: Power -> Power
fPowerToKWatt (KWatt x) = KWatt x
fPowerToKWatt (Watt x) = KWatt (x / 1000)
fPowerToKWatt (HP x) = KWatt (x * 0.745699872)

fPowerToWatt :: Power -> Power
fPowerToWatt p = let (KWatt a) = fPowerToKWatt p in Watt $ a * 1000

fPowerToTorque :: Power -> Rev -> Torque
fPowerToTorque (KWatt p) (HZ f) = Nm $ (p * 1000) / (2 * pi * f)
fPowerToTorque p r = fPowerToTorque (fPowerToKWatt p) (fRevToHZ r)

fMaxTorqueForRev :: Power -> Rev -> Torque
fMaxTorqueForRev _maxPow _rev =
    let Watt maxPow = fPowerToWatt _maxPow
        HZ rev = fRevToHZ _rev
        --
        maxTorque = maxPow / (2 * pi * rev)
     in Nm maxTorque


{-|
    Energy
-}
data Energy = Joules Double
            | KJoules Double
            deriving (Eq)

instance Show Energy where
    show (Joules a)  = showUnit4 a "J"
    show (KJoules a) = showUnit4 a "kJ"

fEnergyToJoules :: Energy -> Energy
fEnergyToJoules (Joules x)  = Joules x
fEnergyToJoules (KJoules x) = Joules (x * 1000)

{-|
    Temperature
-}
data Temp = Celsius Double
          | Fahrenheit Double
          | Rankine Double
          | Kelvin Double
          deriving (Eq)

instance Show Temp where
    show (Celsius a)    = showUnit4 a "°C"
    show (Fahrenheit a) = showUnit4 a "°F"
    show (Rankine a)    = showUnit4 a "°R"
    show (Kelvin a)     = showUnit4 a "°K"


fTempToCelsius :: Temp -> Temp
fTempToCelsius (Celsius x) = Celsius x
fTempToCelsius (Fahrenheit x) = Celsius ((x - 32) * 5/9)
fTempToCelsius (Rankine x) = Celsius ((x - 491.67) * 5/9)
fTempToCelsius (Kelvin x) = Celsius (x - 273.15)


fTempToKelvin :: Temp -> Temp
fTempToKelvin (Kelvin x) = Kelvin x
fTempToKelvin (Celsius x) = Kelvin (x + 273.15)
fTempToKelvin (Fahrenheit x) = Kelvin (((x - 32) * 5/9) + 273.15)
fTempToKelvin (Rankine x) = Kelvin (x* 5/9)


{-|
    Pressure
-}
data Pressure = Pascal Double
              | KPascal Double
              | PSIA Double
              | Bar Double
              | Torr Double
              deriving (Eq)

instance Show Pressure where
    show (Pascal a)    = showUnit4 a "Pascal"
    show (KPascal a)   = showUnit4 a "KPascal"
    show (PSIA a)      = showUnit4 a "psia"
    show (Bar a)       = showUnit4 a "bar"
    show (Torr a)      = showUnit4 a "Torr"


fPressureToPascal :: Pressure -> Pressure
fPressureToPascal (Pascal x) = Pascal x
fPressureToPascal (KPascal x) = Pascal (x * 10^3)
fPressureToPascal (PSIA x) = Pascal (x * 6894.757)
fPressureToPascal (Bar x) = Pascal (x * 10^5)
fPressureToPascal (Torr x) = Pascal (x * 133.322)

{-|
    Revolution
-}
data Rev = RPM Integer
         | HZ Double
--
instance Show Rev where
    show (RPM a)    = show a ++ "rpm"
    show (HZ a)     = showUnit4 a "HZ"
--
instance Eq Rev where
    (==) _a _b =
        let HZ a = fRevToHZ _a
            HZ b = fRevToHZ _b
         in (==) a b
    --
    (/=) a b = not $ (==) a b
--
instance Ord Rev where
    compare _a _b =
        let HZ a = fRevToHZ _a
            HZ b = fRevToHZ _b
         in compare a b
--
--
fRevToHZ :: Rev -> Rev
fRevToHZ (HZ x) = HZ x
fRevToHZ (RPM x) = HZ (fromIntegral x / 60)
--
fRevToRPM :: Rev -> Rev
fRevToRPM (RPM x) = RPM x
fRevToRPM (HZ x) = RPM $ round (x * 60)

{-|
    Speed
-}
data Speed = MeterSecond Double
           | KmHour Double
           | MileHour Double

instance Show Speed where
    show (MeterSecond a) = showUnit4 a "m/s"
    show (KmHour a)      = showUnit4 a "kph"
    show (MileHour a)    = showUnit4 a "mph"
instance Ord Speed where
    compare s_ s_' =
        let MeterSecond s = fSpeedToMeterSecond s_
            MeterSecond s' = fSpeedToMeterSecond s_'
         in compare s s'
instance Eq Speed where
    (==) s_ s_' =
        let MeterSecond s = fSpeedToMeterSecond s_
            MeterSecond s' = fSpeedToMeterSecond s_'
         in s == s'
    (/=) s_ s_' = not $ (==) s_ s_'

fSpeedToMeterSecond :: Speed -> Speed
fSpeedToMeterSecond (MeterSecond x) = MeterSecond x
fSpeedToMeterSecond (KmHour x)      = MeterSecond $ x / 3.6
fSpeedToMeterSecond (MileHour x)    = MeterSecond $ x / 2.23694

fSpeedToKmHour :: Speed -> Speed
fSpeedToKmHour (KmHour x)      = KmHour x
fSpeedToKmHour (MeterSecond x) = KmHour $ x * 3.6
fSpeedToKmHour (MileHour x)    = KmHour $ x * 0.6213727366

fSpeedToMileHour :: Speed -> Speed
fSpeedToMileHour (MileHour x)    = MileHour x
fSpeedToMileHour (KmHour x)      = MileHour (x / 0.621371)
fSpeedToMileHour (MeterSecond x) = MileHour (x * 2.23694)

fSpeedRoundToClosestKmHour :: Speed -> Speed
fSpeedRoundToClosestKmHour s =
    let KmHour x = fSpeedToKmHour s
     in KmHour $ fromIntegral $ round x

{-|
    Rate
-}

data Rate = KgSecond Double
          | MoleSecond Double
          deriving (Eq)

instance Show Rate where
    show (KgSecond a)   = showUnit4 a "Kg/s"
    show (MoleSecond a) = showUnit4 a "Mole/s"


{-|
    Angle
-}
data Angle = Degrees Double
           | Radians Double
           deriving (Eq, Ord)

instance Show Angle where
    show (Degrees a) = showUnit4 a "°"
    show (Radians a) = showUnit4 a "rad"

fAngleToDegrees :: Angle -> Angle
fAngleToDegrees (Degrees a) = Degrees a
fAngleToDegrees (Radians a) = Degrees $ a * 180 / pi

fAngleToRadians :: Angle -> Angle
fAngleToRadians (Radians a) = Radians a
fAngleToRadians (Degrees a) = Radians $ a * pi / 180

newtype Force = Newton Double deriving (Eq, Ord)

instance Show Force where
    show (Newton a) = showUnit4 a "N"


data Density = Kg_Meter3 Double
             | Gram_Meter3 Double
             deriving (Eq, Ord)
instance Show Density where
    show (Kg_Meter3 a) = showUnit4 a "Kg/m3"
    show (Gram_Meter3 a) = showUnit4 a "g/m3"

densityToKg_Meter3 :: Density -> Density
densityToKg_Meter3 (Kg_Meter3 a) = Kg_Meter3 a
densityToKg_Meter3 (Gram_Meter3 a) = Kg_Meter3 (a * 1000)


data MassFlow = GramPerSecond Double
              | KgPerSecond Double
              deriving (Eq, Ord)
instance Show MassFlow where
    show (GramPerSecond a) = showUnit4 a "g/s"
    show (KgPerSecond a)   = showUnit4 a "Kg/s"

massFlowToKgPerSecond :: MassFlow -> MassFlow
massFlowToKgPerSecond (KgPerSecond a)   = KgPerSecond a
massFlowToKgPerSecond (GramPerSecond a) = KgPerSecond (a / 1000)

massFlowToGramsPerSecond :: MassFlow -> MassFlow
massFlowToGramsPerSecond (GramPerSecond a) = GramPerSecond a
massFlowToGramsPerSecond (KgPerSecond a)   = GramPerSecond (a * 1000)


data Time = Seconds Double
          | Hours Double
          deriving (Eq, Ord)

instance Show Time where
    show (Seconds a) = showUnit4 a "s"
    show (Hours a)   = showUnit4 a "h"

fTimeToSeconds :: Time -> Time
fTimeToSeconds (Seconds a) = Seconds a
fTimeToSeconds (Hours a) = Seconds (a * 3600)

fTimeToHours :: Time -> Time
fTimeToHours (Hours a) = Hours a
fTimeToHours (Seconds a) = Hours $ a / 3600


newtype Acceleration = MeterSecond2 Double deriving (Eq, Ord)

instance Show Acceleration where
    show (MeterSecond2 a) = showUnit4 a "m/s2"



newtype LateralG = LateralG Double deriving (Eq, Ord)

instance Show LateralG where
    show (LateralG n) = showUnit4 n " lateral g-force"


newtype LonG = LonG Double deriving (Eq, Ord)

instance Show LonG where
    show (LonG n) = showUnit4 n " longitudinal g-force"



data FuelConsumption = LiterPer100Km Double
                     | KmPerLiter    Double
                     | MilePerGallon Double
                     deriving (Eq, Ord)
--
instance Show FuelConsumption where
    show (LiterPer100Km n) = showUnit4 n "l/100km"
    show (KmPerLiter    n) = showUnit4 n "km/l"
    show (MilePerGallon n) = showUnit4 n "mpg"
--
fFuelConsumptionToLiterPer100Km :: FuelConsumption -> FuelConsumption
fFuelConsumptionToLiterPer100Km (LiterPer100Km n) = LiterPer100Km n
fFuelConsumptionToLiterPer100Km (KmPerLiter    n) = LiterPer100Km (100 / n)
fFuelConsumptionToLiterPer100Km (MilePerGallon n) = KmPerLiter (235.214421461606 / n)
--
fFuelConsumptionToKmPerLiter :: FuelConsumption -> FuelConsumption
fFuelConsumptionToKmPerLiter (LiterPer100Km n) = KmPerLiter (100 / n)
fFuelConsumptionToKmPerLiter (KmPerLiter    n) = KmPerLiter n
fFuelConsumptionToKmPerLiter (MilePerGallon n) = KmPerLiter (n * 0.425144)
--
fFuelConsumptionToMilePerGallon :: FuelConsumption -> FuelConsumption
fFuelConsumptionToMilePerGallon (LiterPer100Km n) = MilePerGallon (235.2145 / n)
fFuelConsumptionToMilePerGallon (KmPerLiter    n) = MilePerGallon (n / 0.425144)
fFuelConsumptionToMilePerGallon (MilePerGallon n) = MilePerGallon n



newtype Current = Ampere Double
                deriving (Eq, Ord)
--
instance Show Current where
    show (Ampere x) = showUnit4 x "A"


newtype CurrentCharge = AmpereHour Double
                    deriving (Eq, Ord)
--
instance Show CurrentCharge where
    show (AmpereHour x) = showUnit4 x "AmpHour"
--
ampsToAmpHour :: Current -> Time -> CurrentCharge
ampsToAmpHour (Ampere c) (Hours t) = AmpereHour $ c * t
ampsToAmpHour c t = ampsToAmpHour c (fTimeToHours t)



data CurrentEnergy =
          WattHour Double
        | KWattHour Double
--
instance Show CurrentEnergy where
    show (WattHour x) = showUnit4 x "Wh"
    show (KWattHour x) = showUnit4 x "KWh"
--
instance Eq CurrentEnergy where
    (==) _x _y =
        let WattHour x = currentEnergyToWattHour _x
            WattHour y = currentEnergyToWattHour _y
         in (==) x y
    (/=) _x _y = not $ (==) _x _y
--
instance Ord CurrentEnergy where
    compare _a _b =
        let WattHour a = currentEnergyToWattHour _a
            WattHour b = currentEnergyToWattHour _b
         in compare a b
--
currentEnergyToWattHour :: CurrentEnergy -> CurrentEnergy
currentEnergyToWattHour (WattHour x) = WattHour x
currentEnergyToWattHour (KWattHour x) = WattHour $ x * 1000
--
currentEnergyToKWattHour :: CurrentEnergy -> CurrentEnergy
currentEnergyToKWattHour (KWattHour x) = KWattHour x
currentEnergyToKWattHour (WattHour x) = KWattHour $ x / 1000
--
ampHourToWattHour :: CurrentCharge -> Voltage -> CurrentEnergy
ampHourToWattHour (AmpereHour c) (Volts v) = WattHour $ c * v



newtype Voltage = Volts Double
                deriving (Eq, Ord)
--
instance Show Voltage where
    show (Volts x) = showUnit4 x "V"




newtype PropulsionCostPerSecond =
    PropulsionCostPerSecond
        {pcpsCost :: Currency}
        deriving (Eq, Ord)
--
instance Show PropulsionCostPerSecond where
    show (PropulsionCostPerSecond c) = printf "Propulsion-Cost/Second : %s" (show c)
--
fPropulsionCostPerSecondToUSD :: PropulsionCostPerSecond -> Double
fPropulsionCostPerSecondToUSD pcps = applyCurrencyExchange $ currEx $ pcpsCost pcps
