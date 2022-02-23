{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module EEngineBMWi3 where

import Commons.ISUnits
import Commons.Utilities
import PowerUnit.EPowerUnit.EEngine
import qualified Data.List as L
import System.Directory

import Vehicle.Vehicle
import qualified Vehicle.VehicleConsts.BMWi3Consts as BMWi3
import PowerUnit.EPowerUnit.ETelInputNode

import GearboxDynamics.Gearbox
import GearboxDynamics.GearboxDynamics
import PowerUnit.ICPowerUnit.Fuel.Fuel

import qualified Data.Vector as Vector
import Data.Csv

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.Vector as V

import qualified KNNRegression.KNN as KNN

import qualified MultivariateRegression.MultivariateRegression as MVR

import qualified PowerUnit.EPowerUnit.EPowerUnitEfficiencyModeller as EPUEM
import qualified PowerUnit.EPowerUnit.EPowerUnitEfficiencyRetrieval as EPUER


import qualified PowerUnit.EPowerUnit.Regen.RegenEfficiencyModeller as RPUEM

import qualified PowerUnit.EPowerUnit.Regen.RegenEfficiencyRetrieval as RER
import qualified PowerUnit.EPowerUnit.Regen.RegenEfficiencyModeller as REM


import PowerUnit.EPowerUnit.Regen.RTelInputNode

import MyChartMaker
import EEngine
import EEngineRegen

import Debug.Trace


data Telemetry =
    Telemetry
    { tTimestamp :: Double
    , tCellTemp :: Double
    , tCellRH :: Double
    , tDynoSpeed :: Double
    , tDynoTorque :: Double
    , tPhase :: Double
    , tAccPedPos :: Double
    , tHVBatSOC :: Double
    , tHVBatCurr :: Double
    , tHVBatVolt :: Double
    , tMotorSpeed :: Double
    } deriving (Show, Eq, Ord)

instance FromNamedRecord Telemetry where
    parseNamedRecord m =
        Telemetry
        <$> m .: C8.pack "Time[sec]"
        <*> m .: C8.pack "Test_Cell_Temp[C]"
        <*> m .: C8.pack "Test_Cell_RH[%]"
        <*> m .: C8.pack "Dyno_Speed[mph]"
        <*> m .: C8.pack "Dyno_Tractive_Effort[N]"
        <*> m .: C8.pack "Phase_#"
        <*> m .: C8.pack "Accelerator_Pedal_Position[%]"
        <*> m .: C8.pack "HV_Battery_SOC[%]"
        <*> m .: C8.pack "HV_Battery_Current[A]"
        <*> m .: C8.pack "HV_Battery_Voltage[V]"
        <*> m .: C8.pack "Motor_Speed[rpm]"


battVoltage = Volts 359.7693


tEngineTorque :: Telemetry -> Double
tEngineTorque t =
    let Meter wRad = fLengthToMeter BMWi3.wheelRadius
        --
        dTorque = Nm $ tDynoTorque t * wRad
        --
        Nm eTorque = fTorqueIn BMWi3.finalDrive dTorque
        in eTorque


tGetPowerInKWatt :: Telemetry -> Double
tGetPowerInKWatt t =
    let eTorque = Nm $tEngineTorque t
        --
        revs = RPM $ round $ tMotorSpeed t
        --
        KWatt pow = fTorqueToKWatt eTorque revs
        in pow


powerRawValue :: Torque -> Rev -> Double
powerRawValue t s = let KWatt p = fTorqueToKWatt t s in p



getPowerTelSupp :: IO EPUEM.ETelemetrySupply
getPowerTelSupp =
    getTelSupp filterForPower
    where
    --
    getTelSupp :: ([(Double, Double, Double, Double)] -> [(Double, Double, Double, Double)]) -> IO EPUEM.ETelemetrySupply
    getTelSupp filter = do
        -- Build full dataset from files.
        _torRevCurrTemp <- readAll (\t -> (tEngineTorque t, tMotorSpeed t, tHVBatCurr t, tCellTemp t))
        let torRevCurrTemp = filter _torRevCurrTemp
        putStrLn $! "[ LOADED: " Prelude.++ show (Prelude.length _torRevCurrTemp) Prelude.++ "]"
        --
        --
        let telEntries = 
                [ EPUEM.ETelemetryEntry (EPUEM.EQuery eT eS amb) curr
                | (t, s, c, temp) <- torRevCurrTemp
                , let eT = Nm t
                , let eS = RPM $ round s
                , let amb = Celsius temp
                , let curr = Ampere c
                ]
        putStrLn $! "[ ORGANIZED : " Prelude.++ show (Prelude.length telEntries) Prelude.++ "]"
        --
        return $ EPUEM.ETelemetrySupply battVoltage telEntries
    --
    filterForPower :: [(Double, Double, Double, Double)] -> [(Double, Double, Double, Double)]
    filterForPower xs = [(a, b, c, d) | (a, b, c, d) <- xs, a >= 10 && a <= 250 && b >= 1000 && c > 0, d >= -5 && d <= 40]


getRegenTelSupp :: IO RPUEM.RTelemetrySupply
getRegenTelSupp =
    getTelSupp filterForRegen
    where
    --
    getTelSupp :: ([(Double, Double, Double, Double)] -> [(Double, Double, Double, Double)]) -> IO RPUEM.RTelemetrySupply
    getTelSupp filter = do
        -- Build full dataset from files.
        _torRevCurrSoc <- readAll (\t -> (tEngineTorque t, tMotorSpeed t, tHVBatCurr t, tHVBatSOC t))
        let torRevCurrSoc = filter _torRevCurrSoc
        putStrLn $! "[ LOADED: " ++ show (length _torRevCurrSoc) ++ "]"
        --
        --
        let telEntries = 
                [ RPUEM.RTelemetryEntry (RPUEM.RQuery eT eS soc) curr
                | (t, s, c, _soc) <- torRevCurrSoc
                , let eT = Nm t
                , let eS = RPM $ round s
                , let soc = round _soc
                , let curr = Ampere c
                ]
        putStrLn $! "[ ORGANIZED : " ++ show (length telEntries) ++ "]"
        --
        return $ RPUEM.RTelemetrySupply battVoltage telEntries
    --
    filterForRegen :: [(Double, Double, Double, Double)] -> [(Double, Double, Double, Double)]
    filterForRegen xs = [(a, b, c, d) | (a, b, c, d) <- xs, a <= -5 && a >= -40 && b >= 1200 && c < 0]


readAll :: Show b => (Telemetry -> b) -> IO [b]
readAll f = do
    d1 <- getVals f "../data/BMWi3/1_Data.csv"
    d2 <- getVals f "../data/BMWi3/2_Data.csv"
    d3 <- getVals f "../data/BMWi3/3_Data.csv"
    d4 <- getVals f "../data/BMWi3/4_Data.csv"
    d5 <- getVals f "../data/BMWi3/5_Data.csv"
    d6 <- getVals f "../data/BMWi3/6_Data.csv"
    d7 <- getVals f "../data/BMWi3/7_Data.csv"
    d8 <- getVals f "../data/BMWi3/8_Data.csv"
    d9 <- getVals f "../data/BMWi3/9_Data.csv"
    d10 <- getVals f "../data/BMWi3/10_Data.csv"
    d11 <- getVals f "../data/BMWi3/11_Data.csv"
    d12 <- getVals f "../data/BMWi3/12_Data.csv"
    d13 <- getVals f "../data/BMWi3/13_Data.csv"
    d14 <- getVals f "../data/BMWi3/14_Data.csv"
    d15 <- getVals f "../data/BMWi3/15_Data.csv"
    d16 <- getVals f "../data/BMWi3/16_Data.csv"
    d17 <- getVals f "../data/BMWi3/17_Data.csv"
    d18 <- getVals f "../data/BMWi3/18_Data.csv"
    d19 <- getVals f "../data/BMWi3/19_Data.csv"
    --
    let dAll = L.concat [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19]
    --
    return dAll
    --
    where
    getVals :: (Telemetry -> b) -> FilePath -> IO [b]
    getVals f filePath = do
        s <- readFile filePath
        let x = decodeByName $ BLU.fromString s :: Either String (Header, Vector.Vector Telemetry)
        case x of
            Left err -> trace err $ return []
            Right (h, values) -> return $ Prelude.map f (V.toList values)