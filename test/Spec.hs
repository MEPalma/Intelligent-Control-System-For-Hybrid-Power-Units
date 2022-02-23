module Spec where

import qualified Gearbox
import qualified SteadyPace
import qualified Regen
--
import qualified ICEngine
import qualified ICEngineFiat500
import qualified ICEngineFordFusionV6
--
import qualified EEngine
import qualified EEngineRegen
import qualified EEngineBMWi3
import qualified EEngineNissanLeaf


{-# ANN module "HLint: ignore Redundant bracket" #-}

main :: IO ()
main = do
    -- let ks = [const 1, const 2, const 5, const 10, (\x -> round ((fromIntegral x * 0.3))), (\x -> round ((fromIntegral x * 0.5))), (\x -> round ((fromIntegral x * 0.8))), id]
    -- ICEngine.testKNNKs ICEngineFiat500.getTelSupp (ICEngineFiat500.nCyls, ICEngineFiat500.cr) ks 6
    -- EEngine.testKNNKs EEngineBMWi3.getPowerTelSupp EEngineBMWi3.battVoltage ks 6
    -- ICEngine.testPolysOnBiases ICEngineFiat500.getTelSupp [1..16] [0.000001, 0.001, 0.1, 0, 10, 20, 50, 100, 200]
    -- ICEngine.testPolysOnBiases ICEngineFordFusionV6.getTelSupp [1..16] [0.000001, 0.001, 0.1, 0, 10, 20, 50, 100, 200]
    -- EEngine.testPolysOnBiases EEngineBMWi3.getPowerTelSupp [1..16] [0.000001, 0.001, 0.1, 0, 10, 20, 50, 100, 200]
    -- EEngine.testPolysOnBiases EEngineNissanLeaf.getPowerTelSupp [1..16] [0.000001, 0.001, 0.1, 0, 10, 20, 50, 100, 200]
    -- EEngineRegen.testPolysOnBiases EEngineNissanLeaf.getRegenTelSupp [1..16] [0.000001, 0.001, 0.1, 0, 10, 20, 50, 100, 200]
    -- EEngineRegen.testKNNKs EEngineNissanLeaf.getRegenTelSupp EEngineNissanLeaf.battVoltage ks 6
    -- EEngineRegen.knnConfigOf EEngineNissanLeaf.getRegenTelSupp EEngineNissanLeaf.battVoltage  (\x -> round ((fromIntegral x * 0.5))) 3 ((-5, -30), (500, 7000), (HP (100)))
    -- EEngine.knnConfigOf EEngineBMWi3.getPowerTelSupp EEngineBMWi3.battVoltage (\x -> round ((fromIntegral x * 0.5))) 3 ((0, 180), (1200, 10000), (HP 140))
    -- EEngine.knnConfigOf EEngineNissanLeaf.getPowerTelSupp EEngineNissanLeaf.battVoltage (\x -> round ((fromIntegral x * 0.5))) 3 ((0, 200), (1200, 7000), (HP 100))
    -- EEngineRegen.knnConfigOf EEngineBMWi3.getRegenTelSupp EEngineBMWi3.battVoltage  (\x -> round ((fromIntegral x * 0.5))) 3 ((-5, -40), (1000, 10000), (HP (100)))
    -- ICEngine.knnConfigOf ICEngineFiat500.getTelSupp  (ICEngineFiat500.nCyls, ICEngineFiat500.cr) (\x -> round ((fromIntegral x * 0.3))) 3 ((1, 130), (1200, 5500), HP 100)
    -- ICEngine.knnConfigOf ICEngineFordFusionV6.getTelSupp  (ICEngineFordFusionV6.nCyls, ICEngineFordFusionV6.cr) (\x -> round ((fromIntegral x * 0.3))) 3 ((1, 200), (1200, 4500), HP 100)
    --
    return ()