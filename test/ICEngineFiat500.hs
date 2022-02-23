{-# LANGUAGE OverloadedStrings #-}

module ICEngineFiat500 where

import Commons.ISUnits
import Commons.Utilities
import PowerUnit.ICPowerUnit.ICEngine
import PowerUnit.ICPowerUnit.Fuel.FuelConsts
import qualified Data.List as L
import System.Directory

import Vehicle.Vehicle
import qualified Vehicle.VehicleConsts.Fiat500Consts as Fiat500
import GearboxDynamics.Gearbox
import GearboxDynamics.GearboxDynamics
import PowerUnit.ICPowerUnit.Fuel.Fuel

import Data.Vector
import Data.Csv

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.Vector as V

import qualified KNNRegression.KNN as KNN

import qualified MultivariateRegression.MultivariateRegression as MR

import qualified PowerUnit.ICPowerUnit.ICPowerUnitEfficiencyModeller as ICPUEM

import PowerUnit.ICPowerUnit.ICPowerUnitEfficiencyRetrieval
import PowerUnit.ICPowerUnit.ICTelInputNode
import qualified Data.HashMap.Lazy as HashMap
import PowerUnit.ICPowerUnit.ICTel

import MyChartMaker

import Debug.Trace

import System.CPUTime
import Text.Printf (printf)
import System.Random
import Data.List
import Control.Applicative

import ICEngine
import KNNRegression.KNNConfig



data Telemetry =
    Telemetry
    { ttimestamp :: String
    , tdynoSpeed :: Double
    , tdynoTorque :: Double
    , tcellTemp :: Double
    , tcellRH :: Double
    , tphase :: Double
    , tengineSpeed :: Double
    , toilTemp :: Double
    , tfuelFlow :: Double
    } deriving (Show, Eq, Ord)

instance FromNamedRecord Telemetry where
    parseNamedRecord m =
        Telemetry
        <$> m .: C8.pack "Timestamp [sec]"
        <*> m .: C8.pack "Dyno Speed [mph]"
        <*> m .: C8.pack "Dyno Tractive Effort [N]"
        <*> m .: C8.pack "Cell Temp [C]"
        <*> m .: C8.pack "Cell RH[%]"
        <*> m .: C8.pack "Phase #"
        <*> m .: C8.pack "Engine Speed [rpm]"
        <*> m .: C8.pack "Engine Oil Temp [deg C]"
        <*> m .: C8.pack "Fuel flow from bench modal [cc/s]"

nCyls = 4
cr = Ratio 11.1

getGearRatioUsed :: Double -> Double -> GearRatio
getGearRatioUsed mph rpm =
    let gb = Fiat500.gearbox
        diff = Fiat500.diff
        --
        vSpeed = fSpeedToMeterSecond $ MileHour mph
        wRevs = wheelSpeed vSpeed Fiat500.wheelRadius
        --
        allGearsScore =
            [ (gr, score)
            | gr <- gb
            , let RPM eSpeed = fRevToRPM $ fRevsInSeq wRevs [gr, diff]
            , let score = abs $ eSpeed - round rpm
            ]
        --
        (gr, _) = L.minimumBy
                        (\(_, s) (_, s') -> compare s s') 
                        allGearsScore
        --
     in gr -- trace ((show mph) L.++ " " L.++ (show rpm) L.++ " " L.++ show allGearsScore) gr



tEngineTorque :: Telemetry -> Double
tEngineTorque t =
    let Meter wRad = fLengthToMeter Fiat500.wheelRadius
        dTorque = Nm $ tdynoTorque t * wRad
        gr = getGearRatioUsed (tdynoSpeed t) (tengineSpeed t)
        --
        transmission = [gr, Fiat500.diff]
        --
        Nm eTorque = fTorqueInSeq dTorque transmission
     in eTorque


tgetPowerInKWatt :: Telemetry -> Double
tgetPowerInKWatt t =
    let eTorque = Nm $ tEngineTorque t
        --
        revs = RPM $ round $ tengineSpeed t
        --
        KWatt pow = fTorqueToKWatt eTorque revs
     in pow



getTelSupp :: IO ICPUEM.ICTelemetrySupply
getTelSupp = do
    -- Build full dataset from files.
    _torRevFuelTempSpeed <- readAll (\t -> (tEngineTorque t, tengineSpeed t, tfuelFlow t, tcellTemp t, tdynoSpeed t))
    let torRevFuelTempSpeed = filterNeg5 _torRevFuelTempSpeed
    putStrLn $! "[ LOADED: " Prelude.++ show (Prelude.length _torRevFuelTempSpeed) Prelude.++ "]"
    --
    --
    let telEntries = 
            [ ICPUEM.ICTelemetryEntry (ICPUEM.ICQuery nCyls cr eT eS amb) amf
            | (t, s, ff, temp, dynoSpeed) <- torRevFuelTempSpeed
            , let eT = Nm t
            , let eS = RPM $ round s
            , let amf =
                    let Gram x = fMassToGram $ fStoichiometricAirForOctane (Gram (ff * _OCTANE_WEIGHT_1_CC_IN_GRAMS))
                     in GramPerSecond x
            , let amb = Celsius temp
            ]
    putStrLn $! "[ ORGANIZED : " Prelude.++ show (Prelude.length telEntries) Prelude.++ "]"
    --
    return $ ICPUEM.ICTelemetrySupply telEntries



testKNN :: IO ()
testKNN = do
    let knnConfig = KNNConfig {knnConfigNeighbors = id, knnConfigAvgCoeff = 3}
    -- Refactor Telemetry supply into KNN data points.
    ICPUEM.ICTelemetrySupply telEntries <- getTelSupp
    --
    let telInputNodes =
            [ ICTelInputNode torque rev temp e
            | ICPUEM.ICTelemetryEntry (ICPUEM.ICQuery nc cr torque rev temp) (GramPerSecond amf) <- telEntries
            --
            -- Eff
            , let teoAirFuelMixture = fOttoCycleAFMForTorque cr temp torque
            , let MixtureFlow (GramPerSecond teoAirMassFlow) _ = mixtureToFlow teoAirFuelMixture nCyls rev
            --
            , let e = amf / teoAirMassFlow
            ]
    --
    -- Store points.
    let knnData = putAll telInputNodes Nothing
    print knnData
    --
    -- Predict.
    let p1 =
            Prelude.map
            (\(ICTelInputNode torque rev temp e) ->
                let icTelQuery = ICTelQuery torque rev temp
                    --
                    e = effOf knnConfig icTelQuery knnData
                    --
                    afMix = fOttoCycleAFMForTorque cr temp torque
                    --
                    GramPerSecond af = airFlow $ mixtureToFlowCorrected afMix nCyls rev e
                    --
                    in (powerRawValue torque rev, af)
            )
            telInputNodes
    --
    -- Plot original values.
    let originalPlot =
            Prelude.map
            (\(ICPUEM.ICTelemetryEntry (ICPUEM.ICQuery _ _ torque rev _) (GramPerSecond amf)) ->
                (powerRawValue torque rev, amf)
            )
            telEntries
    --
    let myTorques = [20]
    let startRPM = 1300 
    let myRPMS = [startRPM, (startRPM+2)..6500]
    let myTorqueAndRPMS = [(t, s) | t <- myTorques, s <- myRPMS]
    let myps =
                Prelude.map
                (\(_torque, _rev) ->
                    let torque = Nm _torque
                        rev = RPM _rev
                        temp = Celsius 21
                        --
                        icTelQuery = ICTelQuery torque rev temp
                        --
                        e = effOf knnConfig icTelQuery knnData
                        --
                        afMix = fOttoCycleAFMForTorque cr temp torque
                        --
                        GramPerSecond af = airFlow $ mixtureToFlowCorrected afMix nCyls rev e
                        --
                     in (powerRawValue torque rev, af)
                )
                myTorqueAndRPMS
    --
    let tmpPlot =
            [ (powerRawValue _torque _rev, amf)
            | (ICPUEM.ICTelemetryEntry (ICPUEM.ICQuery _ _ _torque _rev _) (GramPerSecond amf)) <- telEntries
            , let Nm torque = _torque
            , let RPM revs = fRevToRPM _rev
            , abs torque - 20 < 5
            ]
    --
    makeChart "HTMLS/Fiat500_KNN.html" "FuelFlow vs Power" [MyScatter originalPlot, MyScatter p1, MyLine myps, MyScatter tmpPlot]

    -- Print MSEs.
    let diffs =
            [ diff
            | (ICPUEM.ICTelemetryEntry _ (GramPerSecond af), (_, myaf)) <- Prelude.zip telEntries p1
            , let diff = myaf - af
            ]
    let mse = (Prelude.sum [x^2 | x <- diffs]) / (fromIntegral $ Prelude.length diffs)
    putStrLn $ "MSE: " Prelude.++ show mse
    putStrLn $ "Error: " Prelude.++ show (Prelude.sum (Prelude.map abs diffs) / fromIntegral (Prelude.length p1))

    -- Residual Plots
    makeChart "HTMLS/Fiat500_KNN_Residuals.html" "Power vs Residuals" [MyScatter $ Prelude.zip (Prelude.map snd originalPlot) (standardizedResiduals diffs)]

    putStrLn $ "Number of points " Prelude.++ show (Prelude.length knnData)
    print knnConfig

    return ()



testRegression :: IO ()
testRegression = do
    telSup <- getTelSupp
    let ICPUEM.ICTelemetrySupply telEntries = telSup

    -- Plot real values.
    let fuelFlowVsPower =
            [ (powerRawValue (ICPUEM.icQueryTorque q) (ICPUEM.icQueryRev q), ff)
            | ICPUEM.ICTelemetryEntry q y <- telEntries
            , let ff =
                    let GramPerSecond x = y in x
            ]
    -- 
    -- Learn the telemetry.
    let c = ICPUEM.learn telSup 1 1
    print c
    --
    --
    -- Predict the values.
    let p1 =
            Prelude.map
                (\(ICPUEM.ICTelemetryEntry q y) -> 
                    case ICPUEM.predict c q of
                        --
                        (Just x) ->
                            let GramPerSecond af = airFlow x
                            in (powerRawValue (ICPUEM.icQueryTorque q) (ICPUEM.icQueryRev q), af)
                        --
                        Nothing -> (powerRawValue (ICPUEM.icQueryTorque q) (ICPUEM.icQueryRev q), -1)
                )
                telEntries
    --
    let myTorques = [20,25..120]
    let startRPM = 1000 
    let myRPMS = [startRPM, (startRPM+2)..6500]
    let myTorqueAndRPMS = [(t, s) | t <- myTorques, s <- myRPMS]
    let myps = Prelude.map
                (\(myT, myS) ->
                    let GramPerSecond af = 
                            airFlow (
                                case ICPUEM.predict c (ICPUEM.ICQuery nCyls cr (Nm myT) (RPM myS) (Celsius 21)) of
                                    (Just x) -> x
                                    Nothing  -> MixtureFlow (GramPerSecond $ -1) (GramPerSecond $ -1)
                            )
                      in (powerRawValue (Nm myT) (RPM myS), af)
                )
                --
                myTorqueAndRPMS
    -- Plot the two distributions.
    makeChart "HTMLS/Fiat500_ICPUEM.html" "Power vs AirFlow" [MyScatter fuelFlowVsPower, MyScatter p1, MyScatter myps]
    --
    -- Print MSEs.
    let diffs =
            [ diff
            | (ICPUEM.ICTelemetryEntry _ (GramPerSecond af), (_, myaf)) <- Prelude.zip telEntries p1
            , let diff = myaf - af
            ]
    let mse = (Prelude.sum [x^2 | x <- diffs]) / (fromIntegral $ Prelude.length diffs)
    putStrLn $ "MSE: " Prelude.++ show mse
    putStrLn $ "Error: " Prelude.++ show (Prelude.sum (Prelude.map abs diffs) / fromIntegral (Prelude.length p1))

    -- Residual Plots
    makeChart "HTMLS/Fiat500_ICPUEM_Residuals.html" "Power vs Residuals" [MyScatter $ Prelude.zip (Prelude.map snd fuelFlowVsPower) (standardizedResiduals diffs)]

    -- Write residuals to file.
    -- writeFile "Z_TestOutputs/ic_residuals_f500_2.txt" $ (show c) Prelude.++ "\n\n" Prelude.++ (show $ Prelude.zip (Prelude.map snd fuelFlowVsPower) (standardizedResiduals diffs))

    return ()



filterNeg5 :: [(Double, Double, Double, Double, Double)] -> [(Double, Double, Double, Double, Double)]
filterNeg5 xs = [(a, b, c, d, e) | (a, b, c, d, e) <- xs, a > 10 && a < 130 && b >= 1000 && c > 0 && d > 0 && e > 0]



powerRawValue :: Torque -> Rev -> Double
powerRawValue t s = let KWatt p = fTorqueToKWatt t s in p

readAll :: Show b => (Telemetry -> b) -> IO [b]
readAll f = do
            d1 <- getVals f "../data/Fiat500/1_Data.csv"
            d2 <- getVals f "../data/Fiat500/2_Data.csv"
            d3 <- getVals f "../data/Fiat500/3_Data.csv"
            d4 <- getVals f "../data/Fiat500/4_Data.csv"
            d5 <- getVals f "../data/Fiat500/5_Data.csv"
            --
            let dAll = d1 Prelude.++ d2 Prelude.++ d3 Prelude.++ d4 Prelude.++ d5
            --
            return dAll
        --
        where
        getVals :: (Telemetry -> b) -> FilePath -> IO [b]
        getVals f filePath = do s <- readFile filePath
                                let x = decodeByName $ BLU.fromString s :: Either String (Header, Vector Telemetry)
                                case x of
                                    Left err -> trace err $ return []
                                    Right (h, values) -> return $ Prelude.map f (V.toList values)