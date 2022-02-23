module VehicleDynamics.RoadLoad where

import Commons.ISUnits
import VehicleDynamics.AeroDrag
import Itinerary.Itinerary
import Vehicle.Vehicle


{-|
    Calculate the frictional force of an object.

    Computes: F = mgu*cos(theta) | u = the friction coefficient.

    @Mass: Mass of the object.
    @Double: Drag coefficient of the object with the surface.
    @Angle: The andle of the slope the object is on.

    @Return: The frictional force.
-}
fFrictionalForce :: Mass -> Double -> Angle -> Force
fFrictionalForce (Kg m) frictionCoeff (Radians a) = Newton $ m * _EARTH_GRAVITY_ACCELERATION * frictionCoeff * cos a
fFrictionalForce m f a = fFrictionalForce (fMassToKg m) f (fAngleToRadians a)


{-|
    Calculates the force equal to the weight component of the
    vehicle parallel to the slope.

    Performs: F = mg*sin(theta) | theta = angle of the slope.

    @Mass = weight of the vehicle
    @Angle = angle of the slope (can be negative, 0 and positive)

    @returns the force in Newton of the weight component parallel
    to the sloper.
-}
fWeightComponent :: Mass -> Angle -> Force
fWeightComponent (Kg m) (Radians a) = Newton $ m * _EARTH_GRAVITY_ACCELERATION * sin a
fWeightComponent m a = fWeightComponent (fMassToKg m) (fAngleToRadians a)


{-|
    Calculates the sum of the non aerodynamic forces acting
    against an object's motion.

    @Mass: Mass of the object.
    @Double: Friction Coefficient of the object with the road.
    @Angle: Angle of the slope the object is moving against.

    @Returns: The sum of the forces acting from the road against
              the motion of the vehicle.
-}
fRoadFriction :: Mass -> Double -> Angle -> Force
fRoadFriction mass frictionCoeff angle =
    let Newton frictionalF = fFrictionalForce mass frictionCoeff angle
        Newton weightComp  = fWeightComponent mass angle
     in Newton $ frictionalF + weightComp


roadFriction2 :: Vehicle -> RoadUnit -> Force
roadFriction2 v ru = fRoadFriction (vehicleMass v) (vehicleRollingCoeff v) (ruSlope ru)

{-|
    Calculates the power required for an object to move against
    a road load at a certain speed.

    @Force: The aerodynamic drag opposing to the motion at the given speed.
    @Force: The road friction opposing the motion of the object.
    @Speed: The speed the object should maintain.

    @Return: The power required to the speed given with the opposing forces given.
-}
fRoadLoadPower :: Force -> Force -> Speed -> Power
fRoadLoadPower (Newton aeroDrag) (Newton roadFriction) (MeterSecond speed) = KWatt $ (aeroDrag + roadFriction) * speed / 1000
fRoadLoadPower aeroDrag roadFriction speed = fRoadLoadPower aeroDrag roadFriction (fSpeedToMeterSecond speed)


totalRoadLoadPower :: Vehicle -> RoadUnit -> Power
totalRoadLoadPower v ru = fRoadLoadPower (aeroDrag2 v ru) (roadFriction2 v ru) (ruSpeed ru)