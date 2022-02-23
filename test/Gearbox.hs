{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Gearbox where

import Commons.ISUnits
import Commons.Utilities
import Commons.Currency
import PowerUnit.ICPowerUnit.ICEngine
import PowerUnit.ICPowerUnit.Fuel.Fuel

import Vehicle.VehicleConsts.TestVehicles.TestVehicle
import GearboxDynamics.GearboxDynamics

import qualified MultivariateRegression.MultivariateRegression as MVR

import VehicleDynamics.VehicleDynamics
import qualified PowerUnit.ICPowerUnit.ICPowerUnitEfficiencyModeller as ICPUEM
import Vehicle.Vehicle
import Itinerary.Itinerary
import PowerUnit.ICPowerUnit.ICPowerUnit
import PowerUnit.PowerUnitTypes
import PowerUnit.PowerUnit
import PowerUnit.PowerUnitCostCalculator

import System.Directory

import Data.Vector
import Data.Csv

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.Vector as V

import MyChartMaker

testRu = RoadUnit (KmHour 100.0) (Kmeter 5.0) (Degrees 0) (Meter 0) (AmbientCondition (Celsius 21) (KmHour 0) (KmHour 0) (KPascal 100) (Torr 23.8))

main :: IO ()
main = do
        let startSpeed = KmHour 0
        let endSpeed = KmHour 130
        let preffPuMode = getPUMonICPercentageUsage 100
        let v = updateVehicleCurrentSpeed testVehicle startSpeed
        let ru = testRu{ruSpeed=endSpeed}
        let ambient = acTemp $ ruAmbient ru

        let testAllGearsTestVehicle = accelerationGearing v preffPuMode PUMPureICEngine startSpeed endSpeed _ACCELERATION_RATE_LONG_G _ACCELERATION_TOLL_RATE_LONG_G ru


        -- let allLinesAcceleration = [MyLine tmp | (_, tels) <- testAllGearsTestVehicle, let tmp = [(t, fromIntegral s') | ((_, _), (Nm t, s), _) <- tels, let RPM s' = fRevToRPM s]]
        -- makeChart "HTMLS/gearboxAcceleration.html" "EngineTorque vs EngineSpeed" allLinesAcceleration


        -- let allLinesAccelerationSpeedTime = [MyLine tmp | (_, tels) <- testAllGearsTestVehicle, let tmp = [(s', t) | ((s, Seconds t), (_, _), _) <- tels, let KmHour s' = fSpeedToKmHour s]]
        -- makeChart "HTMLS/gearboxAccelerationSpeedTime.html" "Speed vs Time" allLinesAccelerationSpeedTime



        -- Plots for all gears
        let allCostPerMeterVsSpeed =
                [ MyScatter tmp
                | (_, tels) <- testAllGearsTestVehicle
                , let tmp =
                        [ (vehicleSpeed, usdPerSecond)
                        | ((vSpeed, date), (eTorque, eSpeed), puMode) <- tels
                        , let KmHour vehicleSpeed = fSpeedToKmHour vSpeed
                        , let (propConsumption, _) = propulsionCostPerSecondOfConfig v vSpeed puMode (PowerUnitDeliveryRequirement eTorque eSpeed ambient)
                        , let usdPerSecond = fPropulsionCostPerSecondToUSD propConsumption
                        ]
                ]

        -- Compute optimal gearing for acceleration.
        --
        -- For each power requirement, compute its cost of the power unit operating in some mode.
        let grVsSpeedPropCostSubUnitConf = computeAccelerationSpeedVsCostPerSecond v ru testAllGearsTestVehicle
        --
        let grVsSpeedAndPropCost =
                [ (gr, speedsVsCosts)
                | (gr, speedVsCostVsConfig) <- grVsSpeedPropCostSubUnitConf
                , let speedsVsCosts = [(s, c) | (s, c, _) <- speedVsCostVsConfig]
                ]
        --
        -- Now compute optimal gearing.
        let myAccGearing = accGearSeq grVsSpeedAndPropCost
        --
        -- Reorganize for plotting.
        let myGearing =
                [ (vehicleSpeed, propCostValueInUDS)
                | (_, vSpeed, propCost) <- myAccGearing
                , let KmHour vehicleSpeed = fSpeedToKmHour vSpeed
                , let propCostValueInUDS = currValue $ currEx $ pcpsCost propCost
                ]
        --
        --
        makeChart "HTMLS/gearboxCostPerSecondInUSDVsSpeed.html" "Speed vs USD/Second" (allCostPerMeterVsSpeed Prelude.++ [MyLine myGearing])-- Prelude.++ [g1Plot])



        let Just accConf = accelerationConfigInRoadUnit v preffPuMode ru
        --
        let accSpecs = let (x, _, _) = accConf in x
        let gearChanges = let (_, x, _) = accConf in x
        let finalGearing = let (_, _, x) = accConf in x
        -- print finalGearing

        let allGearsSpeedVsTime =
                    [ MyScatter tmp
                    | (_, tels) <- testAllGearsTestVehicle
                    , let tmp =
                            [ (vehicleSpeed, secs)
                            | ((vSpeed, date), _, _) <- tels
                            , let KmHour vehicleSpeed = fSpeedToKmHour vSpeed
                            , let Seconds secs = fTimeToSeconds date
                            ]
                    ]
        let finalGearingSpeedVsZero = MyScatter
                    [ (speed, 0)
                    | (_s, _, _, _, _) <- finalGearing
                    , let KmHour speed = fSpeedToKmHour _s
                    ]
        let gearChangesSpeedVsZero = MyScatter
                    [ (speed, 0)
                    | (_, _s) <- gearChanges
                    , let KmHour speed = fSpeedToKmHour _s
                    ]

        makeChart "HTMLS/gearboxAccConfig.html" "Speed[kph] vs Time[Seconds]" (allGearsSpeedVsTime Prelude.++ [finalGearingSpeedVsZero] Prelude.++ [gearChangesSpeedVsZero])

        putStrLn "\nAcceletation specs:"
        print accSpecs

        putStrLn "\nGear changes:"
        print gearChanges


        let accTotalCost = accelerationCost finalGearing
        putStrLn "\nTotal cost of acceleration"
        print accTotalCost


        return ()