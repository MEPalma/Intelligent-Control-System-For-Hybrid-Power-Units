{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RoadUnit where

import Commons.ISUnits
import Commons.Utilities
import Commons.Currency
import PowerUnit.ICPowerUnit.ICEngine
import PowerUnit.ICPowerUnit.Fuel.Fuel

import GearboxDynamics.Gearbox
import GearboxDynamics.GearboxDynamics

import qualified MultivariateRegression.MultivariateRegression as MVR

import VehicleDynamics.VehicleDynamics
import qualified PowerUnit.ICPowerUnit.ICPowerUnitEfficiencyModeller as ICPUEM
import Vehicle.Vehicle
import Itinerary.Itinerary
import PowerUnit.ICPowerUnit.ICPowerUnit
import PowerUnit.PowerUnitTypes
import PowerUnit.PowerUnit

import VehicleDynamics.RoadLoad
import VehicleDynamics.AeroDrag

import System.Directory

import Data.Vector
import Data.Csv

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.Vector as V

import MyChartMaker

import Debug.Trace

main :: IO ()
main =
    do

    let v = testVehicle

    let st1 = RoadUnit (KmHour 70) (Kmeter 5) (Degrees 0) (Meter 0) (AmbientCondition (Celsius 20) 0.0 (KmHour 0) (KmHour 0) (KPascal 100) (Torr 23.8))
    let time_st1 = timeOfRoadUnit st1

    let config_st1 = puConfigOf v st1 PUMPureICEngine

    let totalCost_st1 =
            case config_st1 of
                Just c -> steadyPaceCost v (trace (show c) c) time_st1
                Nothing -> error "invalid config for st1"
    putStrLn $ "TotalCost ST1 = " Prelude.++ show totalCost_st1

    return ()
    