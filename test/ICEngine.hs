module ICEngine where

import Commons.ISUnits
import Commons.Utilities
import PowerUnit.ICPowerUnit.ICEngine
import PowerUnit.ICPowerUnit.Fuel.FuelConsts
import System.Directory

import Vehicle.Vehicle
import GearboxDynamics.Gearbox
import GearboxDynamics.GearboxDynamics
import PowerUnit.ICPowerUnit.Fuel.Fuel

import qualified KNNRegression.KNN as KNN
import KNNRegression.KNNConfig

import MyChartMaker

import qualified MultivariateRegression.MultivariateRegression as MR

import qualified PowerUnit.ICPowerUnit.ICPowerUnitEfficiencyModeller as ICPUEM

import qualified PowerUnit.ICPowerUnit.ICPowerUnitEfficiencyRetrieval as FER
import PowerUnit.ICPowerUnit.ICTelInputNode
import qualified Data.HashMap.Lazy as HashMap
import PowerUnit.ICPowerUnit.ICTel

import Debug.Trace

import System.CPUTime
import Text.Printf (printf)


{-# ANN module "HLint: ignore Redundant bracket" #-}


testPolysOnBiases :: IO ICPUEM.ICTelemetrySupply -> [Integer] -> [Double] -> IO ()
testPolysOnBiases ioTelSup ps bs = do
    (testSupp, trainSupp) <- splitSupp ioTelSup
    let lenBs = Prelude.length bs
    --
    let trainPlots = MyScatter
            [ (powerInHPValue torque rev, af)
            | (ICPUEM.ICTelemetryEntry (ICPUEM.ICQuery _ _ torque rev temp) (GramPerSecond af)) <- let ICPUEM.ICTelemetrySupply tmp = trainSupp in tmp
            ]
    --
    let testPlots = MyScatter
            [ (powerInHPValue torque rev, af)
            | (ICPUEM.ICTelemetryEntry (ICPUEM.ICQuery _ _ torque rev temp) (GramPerSecond af)) <- let ICPUEM.ICTelemetrySupply tmp = testSupp in tmp
            ]
    --
    testRegressionWithConfig testSupp trainSupp (Prelude.concat [Prelude.replicate lenBs p| p <- ps]) (Prelude.concat [bs | _ <- ps ]) (trainPlots, testPlots)
--
--
testRegressionWithConfig :: ICPUEM.ICTelemetrySupply -> ICPUEM.ICTelemetrySupply -> [Integer] -> [Double] -> (MyPlotData, MyPlotData) -> IO ()
testRegressionWithConfig _ _ [] _ _ = return ()
testRegressionWithConfig _ _ _ [] _ = return ()
testRegressionWithConfig trainSupp (ICPUEM.ICTelemetrySupply testSupp) (p:ps) (b:bs) (trainPlot, testPlot) = do
    let c = ICPUEM.learn trainSupp p b
    let predictions =
            [ (pow, predictAirFlow)
            | ICPUEM.ICTelemetryEntry qr (GramPerSecond trueAirFlow) <- testSupp
            --
            , let GramPerSecond predictAirFlow =
                    case ICPUEM.predict c qr of
                        (Just x) -> airFlow x
                        Nothing -> GramPerSecond (-1)
            --
            , let pow = powerInHPValue (ICPUEM.icQueryTorque qr) (ICPUEM.icQueryRev qr)
            ]
    let diffs =
            [ trueAirFlow - predictAirFlow
            | (ICPUEM.ICTelemetryEntry _ (GramPerSecond trueAirFlow), predictAirFlow) <- zip testSupp (map snd predictions)
            ]
    --
    let mse = (Prelude.sum [x^2 | x <- diffs]) / (fromIntegral (Prelude.length diffs))
    --
    putStrLn $! (show p) ++ ", " ++ (show b) ++ ", " ++ (show mse)
    --
    makeChart
            ("HTMLS/ICENGINETESTS/REG_p" ++ (show p) ++ "_b" ++ (show b) ++ ".html")
            "Power vs AirFlow"
            [trainPlot, testPlot, MyScatter predictions]
    --
    -- Residual Plots
    makeChart
        ("HTMLS/ICENGINETESTS/REG_p" ++ (show p) ++ "_b" ++ (show b) ++ "RES_.html")
        "Power vs StdResiduals"
        [MyScatter $ Prelude.zip (Prelude.map fst predictions) (standardizedResiduals diffs)]
    --
    testRegressionWithConfig trainSupp (ICPUEM.ICTelemetrySupply testSupp) ps bs (trainPlot, testPlot)


regConfigOf :: IO ICPUEM.ICTelemetrySupply -> Integer -> Double -> ((Integer, Ratio), (Double, Double), (Integer, Integer), (Power)) -> IO ()
regConfigOf _trainSupp p b ((nCyls, cr), (startTorque, endTorque), (startRPM, endRPM), maxPower) = do
    trainSupp <- _trainSupp
    let ICPUEM.ICTelemetrySupply trainSet = trainSupp
    --
    let trainPlots = MyScatter
            [ (powerInHPValue torque rev, af)
            | (ICPUEM.ICTelemetryEntry (ICPUEM.ICQuery _ _ torque rev temp) (GramPerSecond af)) <- trainSet
            ]
    --
    let c = ICPUEM.learn trainSupp p b
    --
    --
    let refillPlots =
            [ (pow, af)
            | (ICPUEM.ICTelemetryEntry (ICPUEM.ICQuery _ _ torque rev temp) (GramPerSecond af)) <- trainSet
            , let GramPerSecond af = 
                    airFlow (
                        case ICPUEM.predict c (ICPUEM.ICQuery nCyls cr torque rev temp) of
                            (Just x) -> x
                            Nothing  -> MixtureFlow (GramPerSecond $ -1) (GramPerSecond $ -1)
                    )
            , let pow = powerInHPValue torque rev
            ]
    --
    let queryPlots = MyScatter $
            Prelude.map
                (\(myT, myS) ->
                    let GramPerSecond af = 
                            airFlow (
                                case ICPUEM.predict c (ICPUEM.ICQuery nCyls cr (Nm myT) (RPM myS) (Celsius 21)) of
                                    (Just x) -> x
                                    Nothing  -> MixtureFlow (GramPerSecond $ -1) (GramPerSecond $ -1)
                            )
                      in (powerInHPValue (Nm myT) (RPM myS), af)
                )
                [(t, s) | t <- [startTorque,(startTorque+5)..endTorque], s <- [startRPM, (startRPM+2)..endRPM], maxPower >= (fTorqueToKWatt (Nm t) (RPM s))]
    --
    makeChart "HTMLS/ICENGINETESTS/REG_TEST.html" "Power vs AirFlow" [trainPlots, MyScatter refillPlots, queryPlots]
    --
    --
    let diffs =
            [ trueFlow - predictFlow
            | ((ICPUEM.ICTelemetryEntry _ (GramPerSecond trueFlow)), predictFlow) <- zip trainSet (map snd refillPlots)
            ]
    --
    let mse = (Prelude.sum [x^2 | x <- diffs]) / (fromIntegral (Prelude.length diffs))
    --
    putStrLn $! (show p) ++ ", " ++ (show b) ++ ", " ++ (show mse)
    -- Residual Plots
    makeChart
        ("HTMLS/ICENGINETESTS/REG_TEST_RES.html")
        "Power vs StdResiduals"
        [MyScatter $ Prelude.zip (Prelude.map fst refillPlots) (standardizedResiduals diffs)]
    --
    -- Power feature plot.
    let powerFeaturesOriginal = MyScatter3d
            [ (t, fromIntegral s, trueFlow)
            | (ICPUEM.ICTelemetryEntry (ICPUEM.ICQuery _ _ torque rev temp) (GramPerSecond trueFlow)) <- trainSet
            , let Nm t = torque
            , let RPM s = fRevToRPM rev
            ]
    let powerFeaturesPredic = MyScatter3d
            [ (t, fromIntegral s, af)
            | (ICPUEM.ICTelemetryEntry (ICPUEM.ICQuery _ _ torque rev temp) _) <- trainSet
            , let Nm t = torque
            , let RPM s = fRevToRPM rev
            , let GramPerSecond af = 
                    airFlow (
                        case ICPUEM.predict c (ICPUEM.ICQuery nCyls cr (Nm t) (RPM s) (Celsius 21)) of
                            (Just x) -> x
                            Nothing  -> MixtureFlow (GramPerSecond $ -1) (GramPerSecond $ -1)
                    )
            ]
    let queryPlots = MyScatter3d $
            Prelude.map
            (\(t, s) ->
                let GramPerSecond af = 
                        airFlow (
                            case ICPUEM.predict c (ICPUEM.ICQuery nCyls cr (Nm t) (RPM s) (Celsius 21)) of
                                (Just x) -> x
                                Nothing  -> MixtureFlow (GramPerSecond $ -1) (GramPerSecond $ -1)
                        )
                 in (t, fromIntegral s, af)
            )
            [(t, s) | t <- [startTorque,(startTorque+5)..endTorque], s <- [startRPM, (startRPM+2)..endRPM], maxPower >= (fTorqueToKWatt (Nm t) (RPM s))]
    --
    makeChart
        ("HTMLS/ICENGINETESTS/REG_TEST_PowerFeatures.html")
        "Torque(Nm) vs Motor-Speed(RPM) vs AirFlow(g/s)"
        [powerFeaturesOriginal, powerFeaturesPredic, queryPlots]
    print c




testKNNKs :: IO ICPUEM.ICTelemetrySupply -> (Integer, Ratio) -> [(Int -> Int)] -> Integer -> IO ()
testKNNKs ioTelSupp (nCyls, cr) ks deg = do
    -- Split the dataset.
    (testSet, trainSet) <- (\tmp -> (telSuppToICTelsInputNodes (fst tmp) (nCyls, cr), telSuppToICTelsInputNodes (snd tmp) (nCyls, cr))) <$> splitSupp ioTelSupp
    --
    -- Store points.
    let knnData = FER.putAll trainSet Nothing
    print $ length $ HashMap.toList knnData
    --
    let learnedPlots = MyScatter
            [ (powerInHPValue torque rev, airflow)
            | (ICTelInputNode torque rev temp eff) <- trainSet
            , let airflow =
                    let afMix = fOttoCycleAFMForTorque cr temp torque
                        GramPerSecond af = airFlow $ mixtureToFlowCorrected afMix nCyls rev eff
                    in af
            ]
    let testPlots = MyScatter
            [ (powerInHPValue torque rev, airflow)
            | (ICTelInputNode torque rev temp eff) <- testSet
            , let airflow =
                    let afMix = fOttoCycleAFMForTorque cr temp torque
                        GramPerSecond af = airFlow $ mixtureToFlowCorrected afMix nCyls rev eff
                    in af
            ]
    --
    inner knnData testSet (nCyls, cr) ks deg (learnedPlots, testPlots)
    --
    where
    inner :: KNN.Telemetry ICTel -> [ICTelInputNode] -> (Integer, Ratio) -> [(Int -> Int)] -> Integer -> (MyPlotData, MyPlotData)  -> IO ()
    inner _ _ _ [] _ _ = return ()
    inner knnData testSet (nCyls, cr) (k:ks) deg (learnedPlots, testPlots) = do
        --
        let knnConfig = KNNConfig {knnConfigNeighbors = k, knnConfigAvgCoeff = deg}
        -- Predict.
        let predictions =
                map
                (\(ICTelInputNode torque rev temp _) ->
                    let icTelQuery = ICTelQuery torque rev temp
                        --
                        e = FER.effOf2 knnConfig (torque, rev, temp) knnData
                        --
                        afMix = fOttoCycleAFMForTorque cr temp torque
                        --
                        GramPerSecond af = airFlow $ mixtureToFlowCorrected afMix nCyls rev e
                        --
                        in (powerInHPValue torque rev, af)
                )
                testSet
        --
        -- Results:
        -- Plot the two distributions.
        let strShowk = show (k $ length (HashMap.toList knnData))
        --
        makeChart
            (("HTMLS/ICENGINETESTS/KNN_Ks_" ++ strShowk ++ "_" ++ show deg) ++ ".html")
            "Power vs AirFlow"
            [learnedPlots, testPlots, MyScatter predictions]
        --
        -- MSE.
        let diffs =
                [ diff
                | ((ICTelInputNode torque rev temp eff), (_, myaf)) <- zip testSet predictions
                --
                , let realAirflow =
                        let afMix = fOttoCycleAFMForTorque cr temp torque
                            GramPerSecond af = airFlow $ mixtureToFlowCorrected afMix nCyls rev eff
                        in af
                --
                , let diff = myaf - realAirflow
                ]
        let mse = (sum [x^2 | x <- diffs]) / (fromIntegral (length diffs))
        --
        putStrLn $ (show deg) ++ " " ++ strShowk ++ " -> MSE=" ++ show mse
        --
        -- Residual Plots
        makeChart
            ("HTMLS/ICENGINETESTS/KNN_Ks_" ++ (strShowk ++"_" ++ show deg) ++ "_RES_.html")
            "Power vs StdResiduals"
            [MyScatter $ Prelude.zip (Prelude.map fst predictions) (standardizedResiduals diffs)]
        --
        --
        inner knnData testSet (nCyls, cr) ks deg (learnedPlots, testPlots)


-- knnConfigOf :: IO ICPUEM.ICTelemetrySupply -> (Integer, Ratio) -> (Int -> Int) -> Integer -> IO ()
-- knnConfigOf ioTelSupp (nCyls, cr) k deg = do
--     telSupp <- ioTelSupp
--     let trainSet = telSuppToICTelsInputNodes telSupp (nCyls, cr)
--     let knnData = FER.putAll trainSet Nothing
--     print $ length $ HashMap.toList knnData
--     print knnData


knnConfigOf :: IO ICPUEM.ICTelemetrySupply -> (Integer, Ratio) -> (Int -> Int) -> Integer -> ((Double, Double), (Integer, Integer), Power) -> IO ()
knnConfigOf ioTelSupp (nCyls, cr) k deg ((startTorque, endTorque), (startRPM, endRPM), maxPower) = do
    let knnConfig = KNNConfig {knnConfigNeighbors = k, knnConfigAvgCoeff = deg}
    telSupp <- ioTelSupp
    let trainSet = telSuppToICTelsInputNodes telSupp (nCyls, cr)
    let knnData = FER.putAll trainSet Nothing

    let trainPlots = MyScatter
            [ (powerInHPValue torque rev, airflow)
            | (ICTelInputNode torque rev temp eff) <- trainSet
            , let airflow =
                    let afMix = fOttoCycleAFMForTorque cr temp torque
                        GramPerSecond af = airFlow $ mixtureToFlowCorrected afMix nCyls rev eff
                    in af
            ]
    --
    let refillPlots =
            [ (powerInHPValue torque rev, predictedAf)
            | (ICTelInputNode torque rev temp _) <- trainSet
            , let eff = FER.effOf2 knnConfig (torque, rev, temp) knnData
            , let predictedAf =
                    let afMix = fOttoCycleAFMForTorque cr temp torque
                        GramPerSecond af = airFlow $ mixtureToFlowCorrected afMix nCyls rev eff
                    in af
            ]
    --
    let queryPlots temp = MyScatter $
            Prelude.map
            (\(torque, rev) ->
                let eff = FER.effOf2 knnConfig (torque, rev, temp) knnData
                    predictedAf =
                        let afMix = fOttoCycleAFMForTorque cr temp torque
                            GramPerSecond af = airFlow $ mixtureToFlowCorrected afMix nCyls rev eff
                         in af
                    --
                    pow = powerInHPValue torque rev
                    in (pow, predictedAf)
            )
            [(Nm t, RPM s) | t <- [startTorque,(startTorque+5)..endTorque], s <- [startRPM, (startRPM+2)..endRPM], maxPower >= (fTorqueToKWatt (Nm t) (RPM s))]
    --
    makeChart "HTMLS/ICENGINETESTS/KNN_TEST.html" "Power vs AirFlow" [trainPlots, MyScatter refillPlots, queryPlots (Celsius 21)]
    --
    --
    let diffs =
            [ trueAf - predictAf
            | (ICTelInputNode torque rev temp eff, predictAf) <- zip trainSet (map snd refillPlots)
            , let trueAf =
                    let afMix = fOttoCycleAFMForTorque cr temp torque
                        GramPerSecond af = airFlow $ mixtureToFlowCorrected afMix nCyls rev eff
                    in af

            ]
    --
    let mse = (Prelude.sum [x^2 | x <- diffs]) / (fromIntegral (Prelude.length diffs))
    --
    putStrLn $! (show knnConfig) ++ ", " ++ (show mse)
    --
    -- Residual Plots
    makeChart
        ("HTMLS/ICENGINETESTS/KNN_TEST_RES.html")
        "Power vs StdResiduals"
        [MyScatter $ Prelude.zip (Prelude.map fst refillPlots) (standardizedResiduals diffs)]
    --
    -- Power feature plot.
    let powerFeaturesOriginal = MyScatter3d
            [ (t, fromIntegral s, trueAf)
            | ICTelInputNode torque rev temp eff <- trainSet
            , let Nm t = torque
            , let RPM s = fRevToRPM rev
            , let trueAf =
                    let afMix = fOttoCycleAFMForTorque cr temp torque
                        GramPerSecond af = airFlow $ mixtureToFlowCorrected afMix nCyls rev eff
                     in af
            ]
    let powerFeaturesPredict = MyScatter3d
            [ (t, fromIntegral s, predictedAf)
            | ICTelInputNode torque rev temp _ <- trainSet
            , let Nm t = torque
            , let RPM s = fRevToRPM rev
            , let predictedAf =
                    let eff = FER.effOf2 knnConfig (torque, rev, temp) knnData
                        afMix = fOttoCycleAFMForTorque cr temp torque
                        GramPerSecond af = airFlow $ mixtureToFlowCorrected afMix nCyls rev eff
                     in af
            ]
    let queryPlots temp = MyScatter3d $
            Prelude.map
            (\(torque, rev) ->
                let eff = FER.effOf2 knnConfig (torque, rev, temp) knnData
                    afMix = fOttoCycleAFMForTorque cr temp torque
                    GramPerSecond predictedAf = airFlow $ mixtureToFlowCorrected afMix nCyls rev eff
                 in (let Nm x = torque in x, let RPM x = rev in fromIntegral x, predictedAf)
            )
            [(Nm t, RPM s) | t <- [startTorque,(startTorque+5)..endTorque], s <- [startRPM, (startRPM+2)..endRPM], maxPower >= (fTorqueToKWatt (Nm t) (RPM s))]
    --
    makeChart
        ("HTMLS/ICENGINETESTS/KNN_TEST_PowerFeatures.html")
        "Torque(Nm) vs Motor-Speed(RPM) vs AirFlow(g/s)"
        [powerFeaturesOriginal, powerFeaturesPredict, queryPlots (Celsius 21)]
    --
    --
    print $ length $ HashMap.toList knnData
    print knnData



powerInHPValue :: Torque -> Rev -> Double
powerInHPValue t s = let HP p = fPowerToHP $ fTorqueToKWatt t s in p


telSuppToICTelsInputNodes :: ICPUEM.ICTelemetrySupply -> (Integer, Ratio) -> [ICTelInputNode]
telSuppToICTelsInputNodes (ICPUEM.ICTelemetrySupply telEntries) (nCyls, cr) =
    [ ICTelInputNode torque rev temp e
    | ICPUEM.ICTelemetryEntry (ICPUEM.ICQuery nc cr torque rev temp) (GramPerSecond amf) <- telEntries
    --
    -- Eff
    , let teoAirFuelMixture = fOttoCycleAFMForTorque cr temp torque
    , let MixtureFlow (GramPerSecond teoAirMassFlow) _ = mixtureToFlow teoAirFuelMixture nCyls rev
    --
    , let e = amf / teoAirMassFlow
    ]

splitSupp :: IO ICPUEM.ICTelemetrySupply -> IO (ICPUEM.ICTelemetrySupply, ICPUEM.ICTelemetrySupply)
splitSupp ioSupp = do
    ICPUEM.ICTelemetrySupply telEntries <- ioSupp
    --
    let nTestSample = round $ 0.3 * fromIntegral (Prelude.length telEntries)
    --
    (testSupp, trainSupp) <- randomSubset nTestSample telEntries
    putStrLn $! "[ PREPARATION COMPLETED (TRAIN, TEST): " Prelude.++ show (Prelude.length trainSupp) Prelude.++ ", " Prelude.++ show (Prelude.length testSupp)
    --
    return (ICPUEM.ICTelemetrySupply testSupp, ICPUEM.ICTelemetrySupply trainSupp)

standardizedResiduals :: [Double] -> [Double]
standardizedResiduals diffs =
    let stdDiv = sqrt ((sum (map (^2) diffs)) / (fromIntegral (length diffs)))
     in [r / stdDiv | r <- diffs]