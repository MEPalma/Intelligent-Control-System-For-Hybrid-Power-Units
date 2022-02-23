{-
    Extends functionalities in the @VehicleDynamics.VehicleDynamics
    module, for non processed itinerary related computations.
-}
module VehicleDynamics.VehiclePhysics where
--
import Commons.ISUnits
--
import VehicleDynamics.AeroDrag
import VehicleDynamics.RoadLoad
--
import Vehicle.Vehicle
import Itinerary.Itinerary

-- Constants.
_NEGLIGIBLE_SPEED_CHANGE_TOLERANCE_PERCENT = 0.1


steadyPaceWheelReq :: Vehicle -> RoadUnit -> (Torque, Rev)
steadyPaceWheelReq v ru =
    let Meter wRadius = fLengthToMeter $ vehicleWheelRadius v
        --
        Newton fReq =
            let Newton aerof = roadFriction2 v ru
                Newton roadf = aeroDrag2 v ru
            in Newton $ aerof + roadf
        --
        wTorque = Nm $ fReq * wRadius
        wSpeed = wheelSpeed (ruSpeed ru) (Meter wRadius)
        --
     in (wTorque, wSpeed)
    where
    wheelCircum :: Length -> Length
    wheelCircum _r =
        let Meter r = fLengthToMeter _r
        in Meter $ 2 * pi * r
    --
    wheelSpeed :: Speed -> Length -> Rev
    wheelSpeed _v wRadius =
        let MeterSecond v = fSpeedToMeterSecond _v
            Meter c = wheelCircum wRadius
        in HZ $ v / c


brakingForce :: Vehicle -> RoadUnit -> LonG -> (Force, Torque)
brakingForce v ru (LonG gs) =
    let -- Compute natural brake.
        Newton aeroBrake = roadFriction2 v ru{ruSpeed = vehicleCurrentSpeed v}
        Newton roadBrake = aeroDrag2 v ru{ruSpeed = vehicleCurrentSpeed v}
        Newton naturalBrake = Newton $ - (aeroBrake + roadBrake)
        --
        -- Compute deceleration force.
        MeterSecond2 dec = MeterSecond2 $ gs * _EARTH_GRAVITY_ACCELERATION
        Newton decForce = Newton $ - (let Kg m = fMassToKg (vehicleMass v) in m * dec)
        --
        -- Left over force.
        Newton brakeRequired = Newton $ decForce - naturalBrake
        --
        -- Torque on wheels for braking required.
        Nm tBrakeTorque = Nm $ brakeRequired * let Meter r = fLengthToMeter (vehicleWheelRadius v) in r
        --
     in (Newton brakeRequired, Nm tBrakeTorque)


isNegligibleSpeedChange ::
     Speed
  -> Speed
  -> Bool
isNegligibleSpeedChange _v _vLimit =
  let MeterSecond v = fSpeedToMeterSecond _v
      MeterSecond vLimit = fSpeedToMeterSecond _vLimit
      --
      -- Set the tolerance: if it requires stopping then tolerance is (0), else (percent*limit + 2mph).
      tolerance = (_NEGLIGIBLE_SPEED_CHANGE_TOLERANCE_PERCENT * vLimit) + if vLimit /= 0 then 0.89408 else 0 
      --
      diff = abs $ v - vLimit
      --
      isTolerableChange = diff < tolerance
   in isTolerableChange
