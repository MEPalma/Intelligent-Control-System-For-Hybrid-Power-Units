module Start where

import Commons.ColoredOutput
import Searches.Solution
import System.Environment
import System.Timeout
import Data.Char
import Data.Maybe

import Commons.ISUnits

import Vehicle.Vehicle
--
import VehicleDynamics.VehicleDynamics

import Itinerary.Itinerary
import Itinerary.Parser.MainParser

import qualified Searches.BruteForceProgressive as BFP
import qualified Searches.Genetic as G
import qualified Searches.Greedy as Greedy
import qualified Searches.Simulated as Sim
import qualified Searches.NoHybridSimulated as NoHybrid

import Vehicle.VehicleConsts.TestVehicles.TestVehicle

{-# ANN module "HLint: ignore Redundant bracket" #-}

main :: IO ()
main =
    do
    let v = testVehicle
    --
    (itineraryFilePath, searchMode, timeoutMicroSeconds) <- handleInputArgs
    itinerary <- parseFile itineraryFilePath
    --
    putStrLn $ showInBox "INPUT FILE"
    print itinerary

    putStrLn $ showInBox "PROCESSED ITINERARY"
    let processedItinerary =
            fromMaybe
                (error $ "\n" ++ showInBoxColor "#COULD NOT PROCESS ITINERARY#" RED)
                (processItinerary v itinerary)
    print processedItinerary
    --
    timeout timeoutMicroSeconds $
     case searchMode of
        NOHYBRID -> do
            putStrLn $ showInBox "NO-HYBRID SEARCH"
            --
            let noHybridOutput =
                    fromMaybe
                        (error $ "\n" ++ showInBoxColor "#NO NO-HYBRID SOLUTION TO PROBLEM#" RED)
                        (NoHybrid.search v processedItinerary)
            putStrLn $ showSolution v noHybridOutput
            print $ solutionTotalCost (let Solution tmp = noHybridOutput in tmp)
        --
        SIMULATED -> do
            putStrLn $ showInBox "SIMULATED SEARCH"
            --
            let simOutput =
                    fromMaybe
                        (error $ "\n" ++ showInBoxColor "#NO SIMULATED SOLUTION TO PROBLEM#" RED)
                        (Sim.search v processedItinerary)
            putStrLn $ showSolution v simOutput
            print $ solutionTotalCost (let Solution tmp = simOutput in tmp)
        --
        GREEDY -> do
            putStrLn $ showInBox "GREEDY SEARCH"
            --
            let greedyOutput =
                    fromMaybe
                        (error $ "\n" ++ showInBoxColor "#NO GREEDY SOLUTION TO PROBLEM#" RED)
                        (Greedy.search v processedItinerary)
            putStrLn $ showSolution v greedyOutput
            print $ solutionTotalCost (let Solution tmp = greedyOutput in tmp)
        --
        FULL -> do
            putStrLn $ showInBox "FULL SEARCH"
            BFP.main v processedItinerary
            getChar
            return ()
        --
        GENETIC -> do
            putStrLn $ showInBox "GENETIC SEARCH"
            G.searchIO v processedItinerary
    --
    return ()


handleInputArgs :: IO (String, SearchMode, Int)
handleInputArgs =
    do
    [itineraryFilePath, _mode, _timeLimit] <- getArgs
    --
    let timeLimit =
            let tmp = read _timeLimit
             in if tmp == 0
                then -1
                else tmp * 1000000
    --
    let searchMode =
            fromMaybe
                (error $ "INVALID SEARCH MODE: " ++ _mode ++ ", ONLY ACCEPTING: " ++ show [NOHYBRID, SIMULATED, GREEDY, FULL, GENETIC])
                (searchModeFromStr _mode)
    --
    return (itineraryFilePath, searchMode, timeLimit)

data SearchMode = NOHYBRID | SIMULATED | GREEDY | FULL | GENETIC deriving (Show, Eq)
--
searchModeFromStr :: String -> Maybe SearchMode
searchModeFromStr _str
    | str == show NOHYBRID = Just NOHYBRID
    | str == show SIMULATED = Just SIMULATED
    | str == show GREEDY = Just GREEDY
    | str == show FULL = Just FULL
    | str == show GENETIC = Just GENETIC
    | otherwise = Nothing
    where
    str = map toUpper _str
