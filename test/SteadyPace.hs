module SteadyPace where

import Commons.ISUnits
import Commons.Utilities
import Commons.Currency

import Vehicle.Vehicle
import Itinerary.Itinerary

import Vehicle.VehicleConsts.TestVehicles.TestVehicle

import GearboxDynamics.Gearbox
import GearboxDynamics.GearboxDynamics
import VehicleDynamics.VehicleDynamics


import PowerUnit.PowerUnitTypes
import PowerUnit.PowerUnit
import PowerUnit.PowerUnitCostCalculator


import PowerUnit.ICPowerUnit.Fuel.Fuel
import PowerUnit.ICPowerUnit.ICPowerUnitType


testRu = RoadUnit (KmHour 100.0) (Kmeter 5.0) (Degrees 0) (Meter 0) (AmbientCondition (Celsius 21) (KmHour 0) (KmHour 0) (KPascal 100) (Torr 23.8))

main :: IO ()
main =
    do

    -- Constants.
    let v1 = updateVehicleCurrentSpeed testVehicle (KmHour 80)
    --
    let r1 = testRu{ruSpeed = KmHour 80, ruSlope = Degrees 0}
    print r1


    putStrLn "----------------------------------------------------------"


    let d1 = PUMPureICEngine

    let Just (conf1, cost1) = steadyPaceCost v1 d1 r1
    print conf1
    print cost1

    print $ fFuelFlowToFuelConsumption (let PUCPureICEngine _ icpuConf = conf1 in icpucMixture icpuConf) (ruSpeed r1)


    putStrLn "----------------------------------------------------------"


    let d2 = PUMPureEEngine
    --
    let Just (conf2, cost2) = steadyPaceCost v1 d2 r1
    print conf2
    print cost2

    putStrLn "----------------------------------------------------------"


    let d3 = getPUMonICPercentageUsage 30
    --
    let Just (conf3, cost3) = steadyPaceCost v1 d3 r1
    print conf3
    print cost3

    putStrLn "----------------------------------------------------------"


    let d4 = getPUMonICPercentageUsage 50
    --
    let Just (conf4, cost4) = steadyPaceCost v1 d4 r1
    print conf4
    print cost4

    putStrLn "----------------------------------------------------------"


    let d5 = getPUMonICPercentageUsage 80
    --
    let Just (conf5, cost5) = steadyPaceCost v1 d5 r1
    print conf5
    print cost5

    putStrLn "----------------------------------------------------------"



    return ()