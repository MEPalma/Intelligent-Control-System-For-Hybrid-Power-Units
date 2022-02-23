module EEngineRegen where

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

import qualified PowerUnit.EPowerUnit.Regen.RegenEfficiencyModeller as RPUEM
import qualified PowerUnit.EPowerUnit.Regen.RegenEfficiencyRetrieval as RER

import PowerUnit.EPowerUnit.Regen.RTel
import PowerUnit.EPowerUnit.Regen.RTelInputNode

import Debug.Trace

import qualified Data.HashMap.Lazy as HashMap
import System.CPUTime
import Text.Printf (printf)


{-# ANN module "HLint: ignore Redundant bracket" #-}


testPolysOnBiases :: IO RPUEM.RTelemetrySupply -> [Integer] -> [Double] -> IO ()
testPolysOnBiases ioTelSup ps bs = do
    (testSupp, trainSupp) <- splitSupp ioTelSup
    let lenBs = Prelude.length bs
    --
    let trainPlots = MyScatter
            [ (powerInHPValue torque rev, curr)
            | (RPUEM.RTelemetryEntry (RPUEM.RQuery torque rev soc) (Ampere curr)) <- let RPUEM.RTelemetrySupply _ tmp = trainSupp in tmp
            ]
    --
    let testPlots = MyScatter
            [ (powerInHPValue torque rev, curr)
            | (RPUEM.RTelemetryEntry (RPUEM.RQuery torque rev soc) (Ampere curr)) <- let RPUEM.RTelemetrySupply _ tmp = testSupp in tmp
            ]
    --
    testRegressionWithConfig testSupp trainSupp (Prelude.concat [Prelude.replicate lenBs p| p <- ps]) (Prelude.concat [bs | _ <- ps ]) (trainPlots, testPlots)
--
--
testRegressionWithConfig :: RPUEM.RTelemetrySupply -> RPUEM.RTelemetrySupply -> [Integer] -> [Double] -> (MyPlotData, MyPlotData) -> IO ()
testRegressionWithConfig _ _ [] _ _ = return ()
testRegressionWithConfig _ _ _ [] _ = return ()
testRegressionWithConfig trainSupp (RPUEM.RTelemetrySupply voltage testSupp) (p:ps) (b:bs) (trainPlot, testPlot) = do
    start <- getCPUTime
    let c = RPUEM.learn trainSupp p b
    end <- getCPUTime
    let cmpTime = fromIntegral $ end - start
    --
    let predictions =
            [ (pow, predictedCurr)
            | RPUEM.RTelemetryEntry qr (Ampere trueCurr) <- testSupp
            , let Ampere predictedCurr = RPUEM.predict c qr voltage
            , let pow = powerInHPValue (RPUEM.rQueryTorque qr) (RPUEM.rQueryRev qr)
            ]
    let diffs =
            [ trueCurr - predictCurr
            | (RPUEM.RTelemetryEntry _ (Ampere trueCurr), predictCurr) <- zip testSupp (map snd predictions)
            ]
    --
    let mse = (Prelude.sum [x^2 | x <- diffs]) / (fromIntegral (Prelude.length diffs))
    --
    putStrLn $! (show p) ++ ", " ++ (show b) ++ ", " ++ (show mse)
    --
    makeChart
        ("HTMLS/RENGINETESTS/REG_p" ++ (show p) ++ "_b" ++ (show b) ++ ".html")
        "Power vs Current"
        [trainPlot, testPlot, MyScatter predictions]
    --
    -- Residual Plots
    makeChart
        ("HTMLS/RENGINETESTS/REG_p" ++ (show p) ++ "_b" ++ (show b) ++ "RES_.html")
        "Power vs StdResiduals"
        [MyScatter $ Prelude.zip (Prelude.map fst predictions) (standardizedResiduals diffs)]
    --
    testRegressionWithConfig trainSupp (RPUEM.RTelemetrySupply voltage testSupp) ps bs (trainPlot, testPlot)



regConfigOf :: IO RPUEM.RTelemetrySupply -> Integer -> Double -> ((Double, Double), (Integer, Integer), Power) -> IO ()
regConfigOf _trainSupp p b ((startTorque, endTorque), (startRPM, endRPM), minPower) = do
    trainSupp <- _trainSupp
    let RPUEM.RTelemetrySupply voltage trainSet = trainSupp
    --
    let trainPlots = MyScatter
            [ (powerInHPValue torque rev, curr)
            | (RPUEM.RTelemetryEntry (RPUEM.RQuery torque rev temp) (Ampere curr)) <- let RPUEM.RTelemetrySupply _ tmp = trainSupp in tmp
            ]
    --
    let c = RPUEM.learn trainSupp p b
    --
    --
    let refillPlots =
            [ (pow, predictedCurr)
            | RPUEM.RTelemetryEntry qr (Ampere trueCurr) <- trainSet
            , let Ampere predictedCurr = RPUEM.predict c qr voltage
            , let pow = powerInHPValue (RPUEM.rQueryTorque qr) (RPUEM.rQueryRev qr)
            ]
    --
    let queryPlots soc = MyScatter $
            Prelude.map
            (\(t, s) ->
                let Ampere predictedCurr = RPUEM.predict c (RPUEM.RQuery (Nm t) (RPM s) 10) voltage
                    pow = powerInHPValue (Nm t) (RPM s)
                    in (pow, predictedCurr)
            )
            --
            [(t, s) | t <- [startTorque,(startTorque-5)..endTorque], s <- [startRPM, (startRPM+2)..endRPM], minPower >= (fTorqueToKWatt (Nm t) (RPM s))]
    --
    makeChart "HTMLS/RENGINETESTS/REG_TEST.html" "Power vs Current" [trainPlots, MyScatter refillPlots, queryPlots 5, queryPlots 15, queryPlots 30,  queryPlots 50, queryPlots 80]
    --
    --
    let diffs =
            [ trueCurr - predictCurr
            | (RPUEM.RTelemetryEntry _ (Ampere trueCurr), predictCurr) <- zip trainSet (map snd refillPlots)
            ]
    --
    let mse = (Prelude.sum [x^2 | x <- diffs]) / (fromIntegral (Prelude.length diffs))
    --
    putStrLn $! (show p) ++ ", " ++ (show b) ++ ", " ++ (show mse)
    -- Residual Plots
    makeChart
        ("HTMLS/RENGINETESTS/REG_TEST_RES.html")
        "Power vs StdResiduals"
        [MyScatter $ Prelude.zip (Prelude.map fst refillPlots) (standardizedResiduals diffs)]
    --
    -- Power feature plot.
    let powerFeaturesOriginal = MyScatter3d
            [ (t, fromIntegral s, trueCurr)
            | RPUEM.RTelemetryEntry qr (Ampere trueCurr) <- trainSet
            , let Nm t = RPUEM.rQueryTorque qr
            , let RPM s = fRevToRPM $ RPUEM.rQueryRev qr
            ]
    let powerFeaturesPredic = MyScatter3d
            [ (t, fromIntegral s, curr)
            | RPUEM.RTelemetryEntry qr (Ampere trueCurr) <- trainSet
            , let Nm t = RPUEM.rQueryTorque qr
            , let RPM s = fRevToRPM $ RPUEM.rQueryRev qr
            , let Ampere curr = RPUEM.predict c qr voltage
            ]
    let queryPlots soc = MyScatter3d $
            Prelude.map
            (\(t, s) ->
                let Ampere curr = RPUEM.predict c (RPUEM.RQuery (Nm t) (RPM s) soc) voltage
                 in (t, fromInteger s, curr)
            )
            [(t, s) | t <- [startTorque,(startTorque-5)..endTorque], s <- [startRPM, (startRPM+2)..endRPM], minPower >= (fTorqueToKWatt (Nm t) (RPM s))]
    --
    makeChart
        ("HTMLS/RENGINETESTS/REG_TEST_PowerFeatures.html")
        "Torque(Nm) vs Motor-Speed(RPM) vs Current(Ampere)"
        [powerFeaturesOriginal, powerFeaturesPredic, queryPlots 5, queryPlots 15, queryPlots 30,  queryPlots 50, queryPlots 80]
    print c



testKNNKs :: IO RPUEM.RTelemetrySupply -> Voltage -> [(Int -> Int)] -> Integer -> IO ()
testKNNKs ioTelSupp voltage ks deg = do
    -- Split the dataset.
    (testSet, trainSet) <- (\tmp -> (telSuppToETelsInputNodes (fst tmp), telSuppToETelsInputNodes (snd tmp))) <$> splitSupp ioTelSupp
    --
    -- Store points.
    let knnData = RER.putAll trainSet Nothing
    print $ length $ HashMap.toList knnData
    --
    let learnedPlots = MyScatter
            [ (powerInHPValue torque rev, curr)
            | (RTelInputNode torque rev temp eff) <- trainSet
            , let Ampere curr = getCurrentWithEff torque rev voltage eff
            ]
    let testPlots = MyScatter
            [ (powerInHPValue torque rev, curr)
            | (RTelInputNode torque rev temp eff) <- testSet
            , let Ampere curr = getCurrentWithEff torque rev voltage eff
            ]
    --
    inner knnData testSet voltage ks deg (learnedPlots, testPlots)
    --
    where
    inner :: KNN.Telemetry RTel -> [RTelInputNode] -> Voltage -> [(Int -> Int)] -> Integer -> (MyPlotData, MyPlotData)  -> IO ()
    inner _ _ _ [] _ _ = return ()
    inner knnData testSet voltage (k:ks) deg (learnedPlots, testPlots) = do
        --
        let knnConfig = KNNConfig {knnConfigNeighbors = k, knnConfigAvgCoeff = deg}
        -- Predict.
        let predictions =
                map
                (\(RTelInputNode torque rev temp _) ->
                    let eff = RER.effOf2 knnConfig (torque, rev, temp) knnData
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
            (("HTMLS/RENGINETESTS/KNN_Ks_" ++ strShowk ++ "_" ++ show deg) ++ ".html")
            "Power vs Current"
            [learnedPlots, testPlots, MyScatter predictions]
        --
        -- MSE.
        let diffs =
                [ diff
                | ((RTelInputNode torque rev temp eff), (_, myCurr)) <- zip testSet predictions
                , let Ampere realCurr = getCurrentWithEff torque rev voltage eff
                , let diff = myCurr - realCurr
                ]
        let mse = (sum [x^2 | x <- diffs]) / (fromIntegral (length diffs))
        --
        putStrLn $ (show deg) ++ " " ++ strShowk ++ " -> MSE=" ++ show mse
        --
        -- Residual Plots
        makeChart
            ("HTMLS/RENGINETESTS/KNN_Ks_" ++ (strShowk ++"_" ++ show deg) ++ "_RES_.html")
            "Power vs StdResiduals"
            [MyScatter $ Prelude.zip (Prelude.map fst predictions) (standardizedResiduals diffs)]
        --
        inner knnData testSet voltage ks deg (learnedPlots, testPlots)


knnConfigOf :: IO RPUEM.RTelemetrySupply -> Voltage -> (Int -> Int) -> Integer -> ((Double, Double), (Integer, Integer), Power) -> IO ()
knnConfigOf ioTelSupp voltage k deg ((startTorque, endTorque), (startRPM, endRPM), minPower) = do
    let knnConfig = KNNConfig {knnConfigNeighbors = k, knnConfigAvgCoeff = deg}
    trainSet <- telSuppToETelsInputNodes <$> ioTelSupp
    let knnData = RER.putAll trainSet Nothing

    let trainPlots = MyScatter
            [ (powerInHPValue torque rev, curr)
            | (RTelInputNode torque rev soc eff) <- trainSet
            , let Ampere curr = getCurrentWithEff torque rev voltage eff
            ]
    --
    let refillPlots =
            [ (pow, predictedCurr)
            | (RTelInputNode torque rev soc _) <- trainSet
            , let eff = RER.effOf2 knnConfig (torque, rev, soc) knnData
            , let Ampere predictedCurr = getCurrentWithEff torque rev voltage eff
            , let pow = powerInHPValue torque rev
            ]
    --
    let queryPlots soc = MyScatter $
            Prelude.map
            (\(t, s) ->
                let eff = RER.effOf2 knnConfig (t, s, soc) knnData
                    Ampere predictedCurr = getCurrentWithEff t s voltage eff
                    --
                    pow = powerInHPValue t s
                    in (pow, predictedCurr)
            )
            [(Nm t, RPM s) | t <- [startTorque,(startTorque-5)..endTorque], s <- [startRPM, (startRPM+2)..endRPM], minPower >= (fTorqueToKWatt (Nm t) (RPM s))]
    --
    makeChart "HTMLS/RENGINETESTS/KNN_TEST.html" "Power vs Current" [trainPlots, MyScatter refillPlots, queryPlots 5, queryPlots 15, queryPlots 30,  queryPlots 50, queryPlots 80]
    --
    --
    let diffs =
            [ trueCurr - predictCurr
            | (RTelInputNode torque rev soc eff, predictCurr) <- zip trainSet (map snd refillPlots)
            , let Ampere trueCurr = getCurrentWithEff torque rev voltage eff
            ]
    --
    let mse = (Prelude.sum [x^2 | x <- diffs]) / (fromIntegral (Prelude.length diffs))
    --
    putStrLn $! (show knnConfig) ++ ", " ++ (show mse)
    --
    -- Residual Plots
    makeChart
        ("HTMLS/RENGINETESTS/KNN_TEST_RES.html")
        "Power vs StdResiduals"
        [MyScatter $ Prelude.zip (Prelude.map fst refillPlots) (standardizedResiduals diffs)]
    --
    -- Power feature plot.
    let powerFeaturesOriginal = MyScatter3d
            [ (t, fromIntegral s, trueCurr)
            | RTelInputNode torque rev soc eff <- trainSet
            , let Nm t = torque
            , let RPM s = fRevToRPM rev
            , let Ampere trueCurr = getCurrentWithEff torque rev voltage eff
            ]
    let powerFeaturesPredict = MyScatter3d
            [ (t, fromIntegral s, predictedCurr)
            | RTelInputNode torque rev soc _ <- trainSet
            , let Nm t = torque
            , let RPM s = fRevToRPM rev
            , let eff = RER.effOf2 knnConfig (torque, rev, soc) knnData
            , let Ampere predictedCurr = getCurrentWithEff torque rev voltage eff
            ]
    let queryPlots soc = MyScatter3d $
            Prelude.map
            (\(t, s) ->
                let eff = RER.effOf2 knnConfig (t, s, soc) knnData
                    Ampere predictedCurr = getCurrentWithEff t s voltage eff
                 in (let Nm x = t in x, let RPM x = s in fromIntegral x, predictedCurr)
            )
            [(Nm t, RPM s) | t <- [startTorque,(startTorque-5)..endTorque], s <- [startRPM, (startRPM+2)..endRPM], minPower >= (fTorqueToKWatt (Nm t) (RPM s))]
    --
    makeChart
        ("HTMLS/RENGINETESTS/KNN_TEST_PowerFeatures.html")
        "Torque(Nm) vs Motor-Speed(RPM) vs Current(Ampere)"
        [powerFeaturesOriginal, powerFeaturesPredict, queryPlots 5, queryPlots 15, queryPlots 30,  queryPlots 50, queryPlots 80]
    --
    --
    print $ length $ HashMap.toList knnData
    print knnData


powerInHPValue :: Torque -> Rev -> Double
powerInHPValue t s = let HP p = fPowerToHP $ fTorqueToKWatt t s in p


telSuppToETelsInputNodes :: RPUEM.RTelemetrySupply -> [RTelInputNode]
telSuppToETelsInputNodes (RPUEM.RTelemetrySupply voltage telEntries) =
    [ RTelInputNode torque rev soc e
    | RPUEM.RTelemetryEntry (RPUEM.RQuery torque rev soc) (Ampere curr) <- telEntries
    --
    -- Eff
    , let Ampere teoCurr = getCurrent torque rev voltage
    , let e = curr / teoCurr
    ]

splitSupp :: IO RPUEM.RTelemetrySupply -> IO (RPUEM.RTelemetrySupply, RPUEM.RTelemetrySupply)
splitSupp ioSupp = do
    RPUEM.RTelemetrySupply voltage telEntries <- ioSupp
    --
    let nTestSample = round $ 0.3 * fromIntegral (Prelude.length telEntries)
    --
    (testSupp, trainSupp) <- randomSubset nTestSample telEntries
    putStrLn $! "[ PREPARATION COMPLETED (TRAIN, TEST): " Prelude.++ show (Prelude.length trainSupp) Prelude.++ ", " Prelude.++ show (Prelude.length testSupp)
    --
    return (RPUEM.RTelemetrySupply voltage testSupp, RPUEM.RTelemetrySupply voltage trainSupp)

standardizedResiduals :: [Double] -> [Double]
standardizedResiduals diffs =
    let stdDiv = sqrt ((sum (map (^2) diffs)) / (fromIntegral (length diffs)))
     in [r / stdDiv | r <- diffs]