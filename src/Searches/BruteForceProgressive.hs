{-
    Encapsulates functions to perform a produces-consumer
    depth first search for the optimal sequence of power
    unit modes of a vehicle in an itinerary.
-}
module Searches.BruteForceProgressive where
--
import Commons.Utilities
import Commons.ColoredOutput
import Commons.ISUnits
import Commons.Currency
import Commons.Threading
--
import Vehicle.Vehicle
import Vehicle.VehicleState
import PowerUnit.PowerUnitTypes
import Itinerary.Itinerary
import Itinerary.ProcessedItinerary
--
import Searches.Solution
import Searches.Commons
--
import Debug.Trace
import Data.Maybe
import Data.List
import System.Timeout
import System.Console.ANSI
import qualified RIO
--
import Control.Concurrent
import Control.Monad
import Control.Concurrent.Chan


{-# ANN module "HLint: ignore Reduce duplication" #-}

-- Resolution of power unit modes.
_PUM_STEP :: Integer
_PUM_STEP = 2
--
_PUM_ALL :: [PowerUnitMode]
_PUM_ALL = getAllPUM _PUM_STEP

{-
    Performs on standard io a producer-consumer search
    for the optimal sequence of power  unit modes of a
    vehicle in an itinerary.
-}
main ::
       Vehicle
    -> ProcessedItinerary
    -> IO ()
--
main v it =
    do
    --
    -- Create a communication chanel for the solution.
    solChan <- newChan :: IO (Chan (Maybe Solution))
    -- Create a worker.
    let cache = buildCache v it _PUM_ALL
    cacheRef <- RIO.newMVar cache
    listenerID <- forkIO $ search cacheRef v it [] solChan
    --
    -- Override channel for force exit.
    exitOverrideChan <- newChan :: IO (Chan Bool)
    --
    -- Create a listener.
    workerID <- forkIO $ bestSolutionFilter solChan exitOverrideChan v (Solution [], fromIntegral (maxBound :: Int))
    --
    -- Crate listener for stop execution tuned into override channel.
    forkIO $ handleUserKey (== 'q') killAllAndMySelf exitOverrideChan [listenerID, workerID]
  
    return ()

{-
    Given a solution chanel and the previous best solution, decides whether to
    output to standard io the newly obtained solution in the channel. If uses
    the exit chanel to handle a force quit of the search.
-}
bestSolutionFilter :: Chan (Maybe Solution) -> Chan Bool -> Vehicle -> (Solution, Currency) -> IO ()
bestSolutionFilter chan exitChan v best =
  do
  mnewSol <- readChan chan
  case mnewSol of
      Nothing -> do putStrLn $! showInColor "FINISHED" MAGENTA
                    writeChan exitChan True
      --
      Just newSol ->
        --
        case isNewBest best newSol of
            Just newBest ->
                do
                -- Clear previous output.
                let prevOutputLen = fromIntegral $ let (Solution tmp, _) = best in length tmp
                clearConsoleLines (if prevOutputLen == 0 then 0 else prevOutputLen + 4)
                --
                -- Print new output.
                let (newBestSol, newBestCost) = newBest
                putStrLn "[NEW BEST SOLUTION]"
                print $ showSolution v newBestSol
                putStrLn "[NEW BEST PRICE]"
                print newBestCost
                --
                -- Restart Listening.
                bestSolutionFilter chan exitChan v newBest
                --
            Nothing -> bestSolutionFilter chan exitChan v best
            --
    where
    isNewBest :: (Solution, Currency) -> Solution -> Maybe (Solution, Currency)
    isNewBest (bestSol, bestSolCost) (Solution newSol) =
      let newSolCost = solutionTotalCost newSol
      in if newSolCost < bestSolCost
          then Just (Solution newSol, newSolCost)
          else Nothing

{-
    Performs a producer-consumer depth first search for the optimal solution.

    @MVar ExpCache: Cached physically available options per road unit.
    @Vehicle: the vehicle.
    @ProcessedItinerary: processed itinerary about the vehicle, to complete.
    @[Expansion] buffer of selected expansions of current subtree.
    @Chan (Maybe Solution): solution channel.
-}
search ::
       MVar ExpCache
    -> Vehicle
    -> ProcessedItinerary
    -> [Expansion]
    -> Chan (Maybe Solution)
    -> IO ()
--
-- Finished itinerary: output the solution to the channel.
search _ _ [] exps chan = writeAllToChan chan [Just (Solution $ reverse exps)]
--
-- Greedy step, for the last unit choose the best valid option.
search _cache _v [pru] exps chan =
    do
    -- compose the vehicle state on this level.
    let vState =
            case exps of
                [] -> vehicleState _v
                _  -> expFinalVehicleState $ head exps
    --
    let v = _v{vehicleState = vState}
    --
    --
    mallExpansions <- expandRoadUnitFromCache _cache v pru _PUM_ALL
    --
    -- Get the most efficient, feasible option.
    let bestOpt =
            minimumBy
                (cmpOn expTotalCostSafe)
                (filterJusts mallExpansions)
    --
    let chain = bestOpt : exps
    --
    search _cache v [] chain chan
--
-- Need to expand this level and for every option on this level
-- fully expand the chain.
search _cache _v (pru:xs) exps chan =
    do
    -- compose the vehicle state on this level.
    let vState =
            case exps of
                [] -> vehicleState _v
                _  -> expFinalVehicleState $ head exps
    --
    let v = _v{vehicleState = vState}
    --
    -- Get the options on this level
    mallExpansions <- expandRoadUnitFromCache _cache v pru _PUM_ALL
    let chains =
            [ opt:exps
            | mOpt <- mallExpansions
            , isJust mOpt
            , let Just opt = mOpt
            ]
    --
    -- Now for every option, carry on the chain.
    case chains of
        [] ->
            case exps of
                [] -> writeChan chan Nothing
                _  -> return () -- No other options on this branch.
        --
        _  ->
            case exps of
                [] -> 
                    do
                    evalChain _cache v xs chains chan
                    -- Close the channel, the top node is fully evaluated.
                    writeChan chan Nothing
                --
                _  -> evalChain _cache v xs chains chan
    --
    where
    evalChain :: MVar ExpCache -> Vehicle -> ProcessedItinerary -> [[Expansion]] -> Chan (Maybe Solution) -> IO ()
    evalChain _ _ _ [] _ = return ()
    evalChain cache v it (e:es) chan =
        do
        search cache v it e chan
        evalChain cache v it es chan
