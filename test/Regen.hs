module Regen where

import Commons.ISUnits
import Commons.Utilities
import PowerUnit.EPowerUnit.EEngine
import qualified Data.List as L
import System.Directory

import Vehicle.Vehicle
import Itinerary.Itinerary
import Vehicle.VehicleConsts.TestVehicles.TestVehicle
import PowerUnit.EPowerUnit.ETelInputNode

import GearboxDynamics.Gearbox
import GearboxDynamics.GearboxDynamics
import PowerUnit.ICPowerUnit.Fuel.Fuel
import VehicleDynamics.VehicleDynamics
import VehicleDynamics.VehiclePhysics

import Data.Csv

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.Vector as V

import qualified KNNRegression.KNN as KNN

import qualified MultivariateRegression.MultivariateRegression as MVR

import qualified PowerUnit.EPowerUnit.EPowerUnitEfficiencyModeller as EPUER

import PowerUnit.EPowerUnit.Regen.RTelInputNode
import PowerUnit.EPowerUnit.EPowerUnit
import qualified PowerUnit.EPowerUnit.Regen.RegenEfficiencyRetrieval as RER
import qualified PowerUnit.EPowerUnit.Regen.RegenEfficiencyModeller as REM

import MyChartMaker

import Debug.Trace

testRu = RoadUnit (KmHour 100.0) (Kmeter 5.0) (Degrees 0) (Meter 0) (AmbientCondition (Celsius 21) (KmHour 0) (KmHour 0) (KPascal 100) (Torr 23.8))

main :: IO ()
main =
    do
    --
    let v = updateVehicleCurrentSpeed testVehicle (KmHour 120)
    --
    let decGearOptions = decelerationGearing v (KmHour 0) _DECELERATION_RATE_LONG_G testRu
    --
    let decGearingWattHours = computeDecelerationRegenCurrentEnergy v decGearOptions
    --
    let plotDecGearOptions =
            [ MyScatter entries
            | (_, logs) <- decGearingWattHours
            , let entries =
                    [ (vSpeed, wh)
                    | (_vSpeed, _currEnergy, puConf) <- logs
                    , let KmHour vSpeed = fSpeedToKmHour _vSpeed
                    , let WattHour wh = currentEnergyToWattHour _currEnergy
                    ]
            ]
    --
    --
    let (gearChanges, accConfig) = decGearSeq decGearingWattHours
    print gearChanges
    print $ currentEnergyToKWattHour $ computeTotRegenInDeceleration accConfig
    --
    let plotRegOnGearing = MyLine
            [ (vSpeed, wh)
            | (_vSpeed, _currEnergy, _) <- accConfig
            , let KmHour vSpeed = fSpeedToKmHour _vSpeed
            , let WattHour wh = currentEnergyToWattHour _currEnergy
            ]
    --
    makeChart "HTMLS/DecelerationRegen.html" "Speed vs WattHour(charge)" $ plotDecGearOptions ++ [plotRegOnGearing]
    
    return ()
