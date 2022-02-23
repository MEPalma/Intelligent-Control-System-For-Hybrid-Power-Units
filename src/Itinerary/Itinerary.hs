{-
    Encapsulates data types and utility functions
    for compositions and presentation of itineraries.
-}
module Itinerary.Itinerary where
--
import Commons.ISUnits
import Text.Printf (printf)


{-
    Data type of an ambient condition.
-}
data AmbientCondition =
        AmbientCondition
            { acTemp          :: Temp
            , acHeadWind      :: Speed
            , acTailWind      :: Speed
            , acDryAirPressure :: Pressure
            , acVapourPressure :: Pressure
            } deriving (Eq)
--
instance Show AmbientCondition where
    show (AmbientCondition t hw tw ap vp) = printf "Ambient: { Temperature: %s, Head-Wind: %s, Tail-Wind: %s, Dry-Air-Pressure: %s, Vapour-Pressure: '%s' }" (show t) (show hw) (show tw) (show ap) (show vp)

{-
    Data type of a road unit.
-}
data RoadUnit =
        RoadUnit 
            { ruSpeed :: Speed
            , ruLength :: Length
            , ruSlope :: Angle
            , ruRadius :: Length
            , ruAmbient :: AmbientCondition
            } deriving (Eq)
--
instance Show RoadUnit where
    show (RoadUnit s l a cr amb) = printf "Road-Unit : { Speed: %s, Length: %s, Slope: %s, Corner-Radius: %s, %s }" (show s) (show l) (show a) (show cr) (show amb)
    -- Shorter output:
    -- show (RoadUnit s l a cr _) = printf "RU::{Speed: %s, Length: %s, Slope: %s, Corner-Radius: %s}\n" (show $ fSpeedToKmHour s) (show $ fLengthToKm l) (show a) (show cr)

type Itinerary = [RoadUnit]

{-
    Returns the expected time of completion of a road unit.
-}
timeOfRoadUnit :: RoadUnit -> Time
timeOfRoadUnit ru =
    let MeterSecond vel = fSpeedToMeterSecond $ ruSpeed ru
        Meter dist = fLengthToMeter $ ruLength ru
        time = Seconds $ dist / vel
     in time

{-
    Returns the equivalent of a Halt road unit.
-}
getHaltRoadUnit :: RoadUnit
getHaltRoadUnit = RoadUnit (MeterSecond 0) (Meter 0) (Radians 0) (Meter 0) (AmbientCondition (Celsius 0) (MeterSecond 0) (MeterSecond 0) (Pascal 0) (Pascal 0))