module EEngine where

import Commons.ISUnits
import Commons.Utilities
import PowerUnit.EPowerUnit.EEngine
import System.Directory

import Vehicle.Vehicle
import GearboxDynamics.Gearbox
import GearboxDynamics.GearboxDynamics

import qualified KNNRegression.KNN as KNN
import KNNRegression.KNNConfig

import MyChartMaker

import qualified MultivariateRegression.MultivariateRegression as MR

import qualified PowerUnit.EPowerUnit.EPowerUnitEfficiencyModeller as EPUEM

import qualified PowerUnit.EPowerUnit.EPowerUnitEfficiencyRetrieval as FER
import PowerUnit.EPowerUnit.ETelInputNode
import qualified Data.HashMap.Lazy as HashMap
import PowerUnit.EPowerUnit.ETel

import Debug.Trace

import System.CPUTime
import Text.Printf (printf)


{-# ANN module "HLint: ignore Redundant bracket" #-}


testPolysOnBiases :: IO EPUEM.ETelemetrySupply -> [Integer] -> [Double] -> IO ()
testPolysOnBiases ioTelSup ps bs = do
    (testSupp, trainSupp) <- splitSupp ioTelSup
    let lenBs = Prelude.length bs
    --
    let trainPlots = MyScatter
            [ (powerInHPValue torque rev, curr)
            | (EPUEM.ETelemetryEntry (EPUEM.EQuery torque rev temp) (Ampere curr)) <- let EPUEM.ETelemetrySupply _ tmp = trainSupp in tmp
            ]
    --
    let testPlots = MyScatter
            [ (powerInHPValue torque rev, curr)
            | (EPUEM.ETelemetryEntry (EPUEM.EQuery torque rev temp) (Ampere curr)) <- let EPUEM.ETelemetrySupply _ tmp = testSupp in tmp
            ]
    --
    testRegressionWithConfig testSupp trainSupp (Prelude.concat [Prelude.replicate lenBs p| p <- ps]) (Prelude.concat [bs | _ <- ps ]) (trainPlots, testPlots)
--
--
testRegressionWithConfig :: EPUEM.ETelemetrySupply -> EPUEM.ETelemetrySupply -> [Integer] -> [Double] -> (MyPlotData, MyPlotData) -> IO ()
testRegressionWithConfig _ _ [] _ _ = return ()
testRegressionWithConfig _ _ _ [] _ = return ()
testRegressionWithConfig trainSupp (EPUEM.ETelemetrySupply voltage testSupp) (p:ps) (b:bs) (trainPlot, testPlot) = do
    let c = EPUEM.learn trainSupp p b
    --
    let predictions =
            [ (pow, predictedCurr)
            | EPUEM.ETelemetryEntry qr (Ampere trueCurr) <- testSupp
            , let Ampere predictedCurr = EPUEM.predict c qr voltage
            , let pow = powerInHPValue (EPUEM.eQueryTorque qr) (EPUEM.eQueryRev qr)
            ]
    let diffs =
            [ trueCurr - predictCurr
            | (EPUEM.ETelemetryEntry _ (Ampere trueCurr), predictCurr) <- zip testSupp (map snd predictions)
            ]
    --
    let mse = (Prelude.sum [x^2 | x <- diffs]) / (fromIntegral (Prelude.length diffs))
    --
    putStrLn $! (show p) ++ ", " ++ (show b) ++ ", " ++ (show mse)
    --
    makeChart
        ("HTMLS/EENGINETESTS/REG_p" ++ (show p) ++ "_b" ++ (show b) ++ ".html")
        "Power vs Current"
        [trainPlot, testPlot, MyScatter predictions]
    --
    -- Residual Plots
    makeChart
        ("HTMLS/EENGINETESTS/REG_p" ++ (show p) ++ "_b" ++ (show b) ++ "RES_.html")
        "Power vs StdResiduals"
        [MyScatter $ Prelude.zip (Prelude.map fst predictions) (standardizedResiduals diffs)]
    --
    testRegressionWithConfig trainSupp (EPUEM.ETelemetrySupply voltage testSupp) ps bs (trainPlot, testPlot)


regConfigOf :: IO EPUEM.ETelemetrySupply -> Integer -> Double -> ((Double, Double), (Integer, Integer), (Power)) -> IO ()
regConfigOf _trainSupp p b ((startTorque, endTorque), (startRPM, endRPM), maxPower) = do
    trainSupp <- _trainSupp
    let EPUEM.ETelemetrySupply voltage trainSet = trainSupp
    --
    let trainPlots = MyScatter
            [ (powerInHPValue torque rev, curr)
            | (EPUEM.ETelemetryEntry (EPUEM.EQuery torque rev temp) (Ampere curr)) <- let EPUEM.ETelemetrySupply _ tmp = trainSupp in tmp
            ]
    --
    let c = EPUEM.learn trainSupp p b
    --
    --
    let refillPlots =
            [ (pow, predictedCurr)
            | EPUEM.ETelemetryEntry qr (Ampere trueCurr) <- trainSet
            , let Ampere predictedCurr = EPUEM.predict c qr voltage
            , let pow = powerInHPValue (EPUEM.eQueryTorque qr) (EPUEM.eQueryRev qr)
            ]
    --
    let queryPlots = MyScatter $
            Prelude.map
                (\(t, s) ->
                    let Ampere predictedCurr = EPUEM.predict c (EPUEM.EQuery (Nm t) (RPM s) (Celsius 20)) voltage
                        pow = powerInHPValue (Nm t) (RPM s)
                      in (pow, predictedCurr)
                )
                --
                [(t, s) | t <- [startTorque,(startTorque+5)..endTorque], s <- [startRPM, (startRPM+2)..endRPM], maxPower >= (fTorqueToKWatt (Nm t) (RPM s))]
    --
    makeChart "HTMLS/EENGINETESTS/REG_TEST.html" "Power vs Current" [trainPlots, MyScatter refillPlots, queryPlots]
    --
    --
    let diffs =
            [ trueCurr - predictCurr
            | (EPUEM.ETelemetryEntry _ (Ampere trueCurr), predictCurr) <- zip trainSet (map snd refillPlots)
            ]
    --
    let mse = (Prelude.sum [x^2 | x <- diffs]) / (fromIntegral (Prelude.length diffs))
    --
    putStrLn $! (show p) ++ ", " ++ (show b) ++ ", " ++ (show mse)
    -- Residual Plots
    makeChart
        ("HTMLS/EENGINETESTS/REG_TEST_RES.html")
        "Power vs StdResiduals"
        [MyScatter $ Prelude.zip (Prelude.map fst refillPlots) (standardizedResiduals diffs)]
    --
    -- Power feature plot.
    let powerFeaturesOriginal = MyScatter3d
            [ (t, fromIntegral s, trueCurr)
            | EPUEM.ETelemetryEntry qr (Ampere trueCurr) <- trainSet
            , let Nm t = EPUEM.eQueryTorque qr
            , let RPM s = fRevToRPM $ EPUEM.eQueryRev qr
            ]
    let powerFeaturesPredic = MyScatter3d
            [ (t, fromIntegral s, curr)
            | EPUEM.ETelemetryEntry qr (Ampere trueCurr) <- trainSet
            , let Nm t = EPUEM.eQueryTorque qr
            , let RPM s = fRevToRPM $ EPUEM.eQueryRev qr
            , let Ampere curr = EPUEM.predict c qr voltage
            ]
    let queryPlots = MyScatter3d $
            Prelude.map
            (\(t, s) ->
                let Ampere curr = EPUEM.predict c (EPUEM.EQuery (Nm t) (RPM s) (Celsius 21)) voltage
                 in (t, fromInteger s, curr)
            )
            [(t, s) | t <- [startTorque,(startTorque+5)..endTorque], s <- [startRPM, (startRPM+2)..endRPM], maxPower >= (fTorqueToKWatt (Nm t) (RPM s))]
    --
    makeChart
        ("HTMLS/EENGINETESTS/REG_TEST_PowerFeatures.html")
        "Torque(Nm) vs Motor-Speed(RPM) vs Current(Ampere)"
        [powerFeaturesOriginal, powerFeaturesPredic, queryPlots]
    --
    print c





testKNNKs :: IO EPUEM.ETelemetrySupply -> Voltage -> [(Int -> Int)] -> Integer -> IO ()
testKNNKs ioTelSupp voltage ks deg = do
    -- Split the dataset.
    (testSet, trainSet) <- (\tmp -> (telSuppToETelsInputNodes (fst tmp), telSuppToETelsInputNodes (snd tmp))) <$> splitSupp ioTelSupp
    --
    -- Store points.
    let knnData = FER.putAll trainSet Nothing
    print $ length $ HashMap.toList knnData
    --
    let learnedPlots = MyScatter
            [ (powerInHPValue torque rev, curr)
            | (ETelInputNode torque rev temp eff) <- trainSet
            , let Ampere curr = getCurrentWithEff torque rev voltage eff
            ]
    let testPlots = MyScatter
            [ (powerInHPValue torque rev, curr)
            | (ETelInputNode torque rev temp eff) <- testSet
            , let Ampere curr = getCurrentWithEff torque rev voltage eff
            ]
    --
    inner knnData testSet voltage ks deg (learnedPlots, testPlots)
    --
    where
    inner :: KNN.Telemetry ETel -> [ETelInputNode] -> Voltage -> [(Int -> Int)] -> Integer -> (MyPlotData, MyPlotData)  -> IO ()
    inner _ _ _ [] _ _ = return ()
    inner knnData testSet voltage (k:ks) deg (learnedPlots, testPlots) = do
        --
        let knnConfig = KNNConfig {knnConfigNeighbors = k, knnConfigAvgCoeff = deg}
        -- Predict.
        let predictions =
                map
                (\(ETelInputNode torque rev temp _) ->
                    let eff = FER.effOf2 knnConfig (torque, rev, temp) knnData
                        Ampere curr = getCurrentWithEff torque rev voltage eff
                        in (powerInHPValue torque rev, curr)
                )
                testSet
        --
        -- Results:
        -- Plot the two distributions.
        let strShowk = show (k $ length (HashMap.toList knnData))
        --
        makeChart
            (("HTMLS/EENGINETESTS/KNN_Ks_" ++ strShowk ++ "_" ++ show deg) ++ ".html")
            "Power vs Current"
            [learnedPlots, testPlots, MyScatter predictions]
        --
        -- MSE.
        let diffs =
                [ diff
                | ((ETelInputNode torque rev temp eff), (_, myCurr)) <- zip testSet predictions
                , let Ampere realCurr = getCurrentWithEff torque rev voltage eff
                , let diff = myCurr - realCurr
                ]
        let mse = (sum [x^2 | x <- diffs]) / (fromIntegral (length diffs))
        --
        putStrLn $ (show deg) ++ " " ++ strShowk ++ " -> MSE=" ++ show mse
        --
        -- Residual Plots
        makeChart
            ("HTMLS/EENGINETESTS/KNN_Ks_" ++ (strShowk ++"_" ++ show deg) ++ "_RES_.html")
            "Power vs StdResiduals"
            [MyScatter $ Prelude.zip (Prelude.map fst predictions) (standardizedResiduals diffs)]
        --
        --
        inner knnData testSet voltage ks deg (learnedPlots, testPlots)


knnConfigOf :: IO EPUEM.ETelemetrySupply -> Voltage -> (Int -> Int) -> Integer -> ((Double, Double), (Integer, Integer), Power) -> IO ()
knnConfigOf ioTelSupp voltage k deg ((startTorque, endTorque), (startRPM, endRPM), maxPower) = do
    let knnConfig = KNNConfig {knnConfigNeighbors = k, knnConfigAvgCoeff = deg}
    trainSet <- telSuppToETelsInputNodes <$> ioTelSupp
    let knnData = FER.putAll trainSet Nothing

    let trainPlots = MyScatter
            [ (powerInHPValue torque rev, curr)
            | (ETelInputNode torque rev soc eff) <- trainSet
            , let Ampere curr = getCurrentWithEff torque rev voltage eff
            ]
    --
    let refillPlots =
            [ (pow, predictedCurr)
            | (ETelInputNode torque rev soc _) <- trainSet
            , let eff = FER.effOf2 knnConfig (torque, rev, soc) knnData
            , let Ampere predictedCurr = getCurrentWithEff torque rev voltage eff
            , let pow = powerInHPValue torque rev
            ]
    --
    let queryPlots temp = MyScatter $
            Prelude.map
            (\(t, s) ->
                let eff = FER.effOf2 knnConfig (t, s, temp) knnData
                    Ampere predictedCurr = getCurrentWithEff t s voltage eff
                    --
                    pow = powerInHPValue t s
                    in (pow, predictedCurr)
            )
            [(Nm t, RPM s) | t <- [startTorque,(startTorque+5)..endTorque], s <- [startRPM, (startRPM+2)..endRPM], maxPower >= (fTorqueToKWatt (Nm t) (RPM s))]
    --
    makeChart "HTMLS/EENGINETESTS/KNN_TEST.html" "Power vs Current" [trainPlots, MyScatter refillPlots, queryPlots (Celsius 21)]
    --
    --
    let diffs =
            [ trueCurr - predictCurr
            | (ETelInputNode torque rev soc eff, predictCurr) <- zip trainSet (map snd refillPlots)
            , let Ampere trueCurr = getCurrentWithEff torque rev voltage eff
            ]
    --
    let mse = (Prelude.sum [x^2 | x <- diffs]) / (fromIntegral (Prelude.length diffs))
    --
    putStrLn $! (show knnConfig) ++ ", " ++ (show mse)
    --
    -- Residual Plots
    makeChart
        ("HTMLS/EENGINETESTS/KNN_TEST_RES.html")
        "Power vs StdResiduals"
        [MyScatter $ Prelude.zip (Prelude.map fst refillPlots) (standardizedResiduals diffs)]
    --
    -- Power feature plot.
    let powerFeaturesOriginal = MyScatter3d
            [ (t, fromIntegral s, trueCurr)
            | ETelInputNode torque rev soc eff <- trainSet
            , let Nm t = torque
            , let RPM s = fRevToRPM rev
            , let Ampere trueCurr = getCurrentWithEff torque rev voltage eff
            ]
    let powerFeaturesPredict = MyScatter3d
            [ (t, fromIntegral s, predictedCurr)
            | ETelInputNode torque rev soc _ <- trainSet
            , let Nm t = torque
            , let RPM s = fRevToRPM rev
            , let eff = FER.effOf2 knnConfig (torque, rev, soc) knnData
            , let Ampere predictedCurr = getCurrentWithEff torque rev voltage eff
            ]
    let queryPlots temp = MyScatter3d $
            Prelude.map
            (\(t, s) ->
                let eff = FER.effOf2 knnConfig (t, s, temp) knnData
                    Ampere predictedCurr = getCurrentWithEff t s voltage eff
                 in (let Nm x = t in x, let RPM x = s in fromIntegral x, predictedCurr)
            )
            [(Nm t, RPM s) | t <- [startTorque,(startTorque+5)..endTorque], s <- [startRPM, (startRPM+2)..endRPM], maxPower >= (fTorqueToKWatt (Nm t) (RPM s))]
    --
    makeChart
        ("HTMLS/EENGINETESTS/KNN_TEST_PowerFeatures.html")
        "Torque(Nm) vs Motor-Speed(RPM) vs Current(Ampere)"
        [powerFeaturesOriginal, powerFeaturesPredict, queryPlots (Celsius 21)]
    --
    --
    print $ length $ HashMap.toList knnData
    print knnData



powerInHPValue :: Torque -> Rev -> Double
powerInHPValue t s = let HP p = fPowerToHP $ fTorqueToKWatt t s in p


telSuppToETelsInputNodes :: EPUEM.ETelemetrySupply -> [ETelInputNode]
telSuppToETelsInputNodes (EPUEM.ETelemetrySupply voltage telEntries) =
    [ ETelInputNode torque rev temp e
    | EPUEM.ETelemetryEntry (EPUEM.EQuery torque rev temp) (Ampere curr) <- telEntries
    --
    -- Eff
    , let Ampere teoCurr = getCurrent torque rev voltage
    , let e = curr / teoCurr
    ]

splitSupp :: IO EPUEM.ETelemetrySupply -> IO (EPUEM.ETelemetrySupply, EPUEM.ETelemetrySupply)
splitSupp ioSupp = do
    EPUEM.ETelemetrySupply voltage telEntries <- ioSupp
    --
    let nTestSample = round $ 0.3 * fromIntegral (Prelude.length telEntries)
    --
    (testSupp, trainSupp) <- randomSubset nTestSample telEntries
    putStrLn $! "[ PREPARATION COMPLETED (TRAIN, TEST): " Prelude.++ show (Prelude.length trainSupp) Prelude.++ ", " Prelude.++ show (Prelude.length testSupp)
    --
    return (EPUEM.ETelemetrySupply voltage testSupp, EPUEM.ETelemetrySupply voltage trainSupp)

standardizedResiduals :: [Double] -> [Double]
standardizedResiduals diffs =
    let stdDiv = sqrt ((sum (map (^2) diffs)) / (fromIntegral (length diffs)))
     in [r / stdDiv | r <- diffs]