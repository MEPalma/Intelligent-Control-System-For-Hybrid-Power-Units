module Searches.Genetic where
--
import Commons.ISUnits
import Commons.Utilities
import Commons.Currency
import Commons.ColoredOutput
--
import Searches.Commons
import Searches.Solution
--
import Vehicle.Vehicle
import Vehicle.VehicleState
import VehicleDynamics.VehicleDynamics
--
import GearboxDynamics.Gearbox
import GearboxDynamics.GearboxDynamics
--
import PowerUnit.PowerUnitTypes
import PowerUnit.PowerUnit
import PowerUnit.PowerUnitCostCalculator
--
import Itinerary.Itinerary
import Itinerary.ProcessedItinerary
--
import Data.List
import Data.Maybe
import qualified RIO
--
import Control.Parallel.Strategies
import Control.DeepSeq
import Control.Monad (when, unless)
import System.Directory
import Data.Time.Clock.POSIX
--
import Debug.Trace


{-# ANN module "HLint: ignore Redundant bracket" #-}


-- Debugging settings.
--
_DEBUG_TO_FILE :: Bool
_DEBUG_TO_FILE = False
--
_DEBUG_DESTINATION_FILE :: FilePath
_DEBUG_DESTINATION_FILE = "GADebug.csv"


-- Genetic search settings.
--
-- Resolution of power unit modes.
_PUM_STEP :: Integer
_PUM_STEP = 2
--
_PUM_ALL :: [PowerUnitMode]
_PUM_ALL = getAllPUM _PUM_STEP
--
--
_GEN_SIZE_PERCENT :: Rational
_GEN_SIZE_PERCENT = 200
--
_GEN_MAX_SIZE :: Int -> Int
_GEN_MAX_SIZE itLen = round ((_GEN_SIZE_PERCENT / 100) * fromIntegral itLen)
--
--
_MUT_CHANCE :: Rational
_MUT_CHANCE = 0.1
--
_CROSS_OVER_MUTATE_STRATEGY = singlePointCrossOverMutate


-- Intermediate function types (for syntactic advantage only).
type PartialSolutionNode = (Expansion, PowerUnitMode)
type PartialSolution = [PartialSolutionNode]
--
type Gene = (Solution, [PowerUnitMode], Currency)
type Generation = [Gene]
--
type GeneWeightedList = [(Gene, Rational)]


{-
    Performs a genetic search of a vehicle on its
    processed itinerary, outputting to stdout newly
    best solutions founds.
-}
searchIO :: Vehicle -> ProcessedItinerary -> IO ()
searchIO v it =
    do
    let cache = buildCache v it _PUM_ALL
    cacheRef <- RIO.newMVar cache
    --
    let genSize = _GEN_MAX_SIZE (length it)
    let firstGen = genGenesWithHeats cacheRef v it genSize
    inner 0 genSize cacheRef v it firstGen _MUT_CHANCE 0
    where
    inner :: Integer -> Int -> RIO.MVar ExpCache -> Vehicle -> ProcessedItinerary -> IO Generation -> Rational -> Currency -> IO ()
    inner genNum genSize ioCache v it iogen0 mut prevCost =
        do
        gen0 <- iogen0
        genWList <- sigmaScaling iogen0
        --
        (bestGen0, gen1) <- getNewGeneration genSize ioCache v it genWList mut
        let (_, _, newCost) = bestGen0
        --
        if newCost == prevCost
        then inner (genNum+1) genSize ioCache v it (return gen1) mut prevCost
        else do
            let prevOutputLen = fromIntegral $ let (Solution tmp, _, _) = bestGen0 in length tmp
            clearConsoleLines (if prevCost == 0 then 0 else prevOutputLen+2)
            --
            putStrLn $ let (sol, _, _) = (bestGen0) in showSolution v sol
            print newCost
            --
            when _DEBUG_TO_FILE $ outputDebug genNum (bestGen0, if genNum == 0 then (bestGen0:gen0) else gen0)
            --
            inner (genNum+1) genSize ioCache v it (return gen1) mut newCost

{-
    Creates a new first generation of valid genes.
-}
genGenesWithHeats :: RIO.MVar ExpCache -> Vehicle -> ProcessedItinerary -> Int -> IO Generation
genGenesWithHeats ioCache v it n =
    case n of
        0 -> return []
        _ ->
            do
            heat <- getRandomRationalInRange 0 (fromIntegral lenIt)
            mnewGene <- genGeneWithHeat ioCache v it heat
            --
            case mnewGene of
                --
                Nothing -> genGenesWithHeats ioCache v it n
                --
                Just newGene -> (:) newGene <$> genGenesWithHeats ioCache v it (n-1)
    --
    where
    lenIt = fromIntegral (length it) :: Integer
    --
    genGeneWithHeat :: RIO.MVar ExpCache -> Vehicle -> ProcessedItinerary -> Rational -> IO (Maybe Gene)
    genGeneWithHeat ioCache v it heat =
        inner ioCache v it heat (return [])
        where
        inner :: RIO.MVar ExpCache -> Vehicle -> ProcessedItinerary -> Rational -> IO PartialSolution -> IO (Maybe Gene)
        --
        inner _ _ [] _ iopSol =
            do
            gene <- partialSolutionToGene <$> iopSol
            return $ Just gene
        --
        inner ioCache _v (pru:prus) heat iopSol =
            do
            pSol <- iopSol
            --
            case pru of
                --
                DropRoadUnit{} ->
                    case getExpansionDropRoadUnit _v pru of
                        --
                        Nothing -> return Nothing
                        --
                        Just partSolNode ->
                            inner ioCache _v{vehicleState = expFinalVehicleState (fst partSolNode)} prus heat (return $ partSolNode:pSol)
                --
                PickPaceRoadUnit{} ->
                    do
                    _exps <- filterJusts <$> expandRoadUnitFromCache ioCache _v pru _PUM_ALL
                    case _exps of
                        --
                        -- No options.
                        [] -> return Nothing -- Failed: no option.
                        --
                        -- Just one option.
                        [onlyOpt] ->
                            let nextPartSol = expansionToPartialSolution onlyOpt in
                            inner ioCache _v{vehicleState = expFinalVehicleState onlyOpt} prus heat (return $ nextPartSol:pSol)
                        --
                        -- More than one option.
                        allValidOpts ->
                             -- If this is an acceleration and all options reverted to backup PUMPureICEngine, then don't filter.
                            if (pruUnitTask pru == PICK) && all (\x -> expPuMode x == PUMPureICEngine) allValidOpts
                            then
                                let onlyOpt = head allValidOpts
                                    nextPartSol = expansionToPartialSolution onlyOpt
                                 in inner ioCache _v{vehicleState = expFinalVehicleState onlyOpt} prus heat (return $ nextPartSol:pSol)
                            --
                            else do
                                -- Find the best option: which becomes the best option.
                                let defaultOpt = minimumBy (cmpOn expTotalCostSafe) allValidOpts
                                --
                                -- Now pick one a random from all but default.
                                let allBackupOpts = takeAllThat (\x -> (expPuMode x) /= (expPuMode defaultOpt)) allValidOpts -- Filter out options different from default.
                                --
                                backupOpt <- selectAtRandom allBackupOpts
                                --
                                -- Now decide which option to take.
                                nextOpt <- selectWeightedRandom [(defaultOpt, fromIntegral lenIt), (backupOpt, heat)]
                                let nextPartSol = expansionToPartialSolution nextOpt
                                --
                                -- If the backup is taken, then heat must decrease.
                                newHeat <- if expPuMode nextOpt == expPuMode defaultOpt
                                                then return heat
                                                else do val <- getRandomRationalInRange 0 1
                                                        return $ min 0 (heat - val)
                                --
                                inner ioCache _v{vehicleState = expFinalVehicleState nextOpt} prus newHeat (return $ nextPartSol:pSol)

{-
    Retrieves from a processed road unit with deceleration task, the deceleration
    configuration bounded to it.
-}
getExpansionDropRoadUnit :: Vehicle -> ProcessedRoadUnit -> Maybe PartialSolutionNode
getExpansionDropRoadUnit v pru =
    case pru of
        DropRoadUnit{} ->
            let mExp = expandRoadUnitSingle v pru PUMPureEEngine
            in if isJust mExp
                then let Just exp = mExp in Just (exp, PUMPureEEngine)
                else Nothing
        --
        _ -> Nothing

{-
    Converts a partial solution to a search gene.
-}
partialSolutionToGene :: PartialSolution -> Gene
partialSolutionToGene _pSol =
    let pSol = reverse _pSol
        --
        exps = map fst pSol
        puModes = map snd pSol
        --
        totCost = solutionTotalCost exps
        sol = Solution exps
     in (sol, puModes, totCost)

{-
    Converts an expansion to a partial solution.
-}
expansionToPartialSolution :: Expansion -> PartialSolutionNode
expansionToPartialSolution exp =
    case exp of
        DropExpansion{} -> (exp, PUMPureEEngine)
        _               -> (exp, expPuMode exp)

{-
    Given a sequence of power unit modes, attempts
    to build a new candidate solution, failing only if the
    minimum energy state of the vehicle is breached.
-}
buildSolution ::
       RIO.MVar ExpCache
    -> Vehicle
    -> ProcessedItinerary
    -> [PowerUnitMode]
    -> PartialSolution
    -> IO (Maybe Gene)
--
buildSolution _ _ [] [] buff = return $ Just $ partialSolutionToGene buff
--
buildSolution ioCache v (pru:prus) (puMode:puModes) buff =
    do
    mExp <- expandRoadUnitFromCacheSingle ioCache v pru puMode
    case mExp of
         Nothing -> return Nothing
         Just exp -> buildSolution ioCache v{vehicleState = expFinalVehicleState exp} prus puModes ((exp, puMode):buff)
--
buildSolution _ _ _ _ _ = return Nothing

{-
    Performs multi point crossover drawing genetic material
    randomly from a weighted list of genes.
    Performs gene repair before returning a new gene.
-}
multiPointCrossOverMutate ::
       RIO.MVar ExpCache
    -> Vehicle
    -> ProcessedItinerary
    -> GeneWeightedList
    -> Rational
    -> IO (Maybe Gene)
multiPointCrossOverMutate ioCache v it genWList mut =
    do
    let len = length it
    --
    selectLst <- take len <$> genRandomsFromWeightedList genWList
    --
    -- New Sequence of puModes.
    newSeq <- exportIO
            [ nextChoice
            | i <- reverse [0..(len - 1)]
            , let (_, puModes, _) = selectLst !! i
            , let hereditaryVal = puModes !! i
            , let nextChoice =
                    if mut <= 0
                        then return hereditaryVal
                        else
                            do
                            mutOpt <- selectAtRandom $ takeAllThat (/= hereditaryVal) _PUM_ALL
                            selectWeightedRandom [(hereditaryVal, 1 - mut), (mutOpt, mut)]
            ]
    --
    geneRepair ioCache v it newSeq (return [])

{-
    Performs single point crossover drawing genetic material
    from two randomly selected genes from a weighter list.
    Performs gene repair before returning a new gene.
-}
singlePointCrossOverMutate :: RIO.MVar ExpCache -> Vehicle -> ProcessedItinerary -> GeneWeightedList -> Rational -> IO (Maybe Gene)
singlePointCrossOverMutate ioCache v it genWList mut =
    do
    --
    -- Itinerary Length.
    let itLen = length it
    --
    -- Choose splitting point.
    changePortion <- fromIntegral <$> getRandomIntInRange 1 (fromIntegral itLen)
    --
    -- Choose which two Genes to copy from.
    [g0, g1] <- take 2 <$> genRandomsFromWeightedList genWList
    let (_, g0modes, _) = g0
    let (_, g1modes, _) = g1
    --
    -- Now concat partial solutions from gene 0 and 1.
    let g2modes = (take changePortion g0modes) ++ (take (itLen - changePortion) g1modes)
    --
    -- Mutation Step.
    g2MutatedModes <- exportIO
            [ newPuMode
            | puMode <- g2modes
            , let newPuMode =
                    if mut <= 0
                        then return puMode
                        else
                            do
                            mutOpt <- selectAtRandom $ takeAllThat (/= puMode) _PUM_ALL
                            selectWeightedRandom [(puMode, 1 - mut), (mutOpt, mut)]
            ]
    --
    geneRepair ioCache v it g2MutatedModes (return [])

{-
    Deterministically fixes broken genetic material
    by overriding power unit modes which breach the
    energy state of the vehicle. These are replaced
    by the most efficient option available.
-}
geneRepair ::
       RIO.MVar ExpCache
    -> Vehicle
    -> ProcessedItinerary
    -> [PowerUnitMode]
    -> IO PartialSolution
    -> IO (Maybe Gene)
--
geneRepair _ _ [] [] iobuff = Just . partialSolutionToGene <$> iobuff
--
geneRepair ioCache v (pru:prus) (puMode:puModes) iobuff =
    do
    choice <- expandRoadUnitFromCacheSingle ioCache v pru puMode
    -- Try applying the genetic choice.
    case choice of
        -- If this is a valid option -> continue.
        Just exp ->
            geneRepair ioCache v{vehicleState = expFinalVehicleState exp} prus puModes ((:) (exp, puMode) <$> iobuff)
        --
        -- If not applicably, attempt to select another option.
        Nothing ->
            do
            allChoices <- filterJusts <$> expandRoadUnitFromCache ioCache v pru _PUM_ALL
            case allChoices of
                --
                -- No options.
                [] -> return Nothing -- Failed: no option.
                --
                -- Just one option.
                [onlyOpt] ->
                    let nextPartSol = expansionToPartialSolution onlyOpt in
                    geneRepair ioCache v{vehicleState = expFinalVehicleState onlyOpt} prus puModes ((:) nextPartSol <$> iobuff)
                --
                -- More than one option.
                allValidOpts ->
                    -- Find the best option: which becomes the best option.
                    let defaultOpt = minimumBy (cmpOn expTotalCostSafe) allValidOpts
                        nextPartSol = expansionToPartialSolution defaultOpt
                     in geneRepair ioCache v{vehicleState = expFinalVehicleState defaultOpt} prus puModes ((:) nextPartSol <$> iobuff)

{-
    Creates a weighted list of all the genes in a generation,
    by scoring their fitness with a sigma scaling smoothing.
-}
sigmaScaling :: IO Generation -> IO GeneWeightedList
sigmaScaling ioGen = do
    gen <- ioGen
    --
    let invPrices =
            [ 1/p
            | g <- gen
            , let _p = applyCurrencyExchange $ currEx (let (_, _, curr) = g in curr)
            , let p = if _p <= 0 then fromIntegral (maxBound :: Int) else _p
            ]
    --
    let sigmaScores = (toSigmaScale invPrices)
    --
    let nSigmaScore =
            let worstScore = minimum sigmaScores
                bestScore = maximum sigmaScores
             in map (toRational . \x -> mapToRange (x, (worstScore, bestScore)) (0, 1)) sigmaScores
    --
    return $ zip gen nSigmaScore

{-
    It performs a generation evolution, leading to 
    a new generation of heathy/valid genes.
-}
getNewGeneration ::
       Int
    -> RIO.MVar ExpCache
    -> Vehicle
    -> ProcessedItinerary
    -> GeneWeightedList
    -> Rational
    -> IO (Gene, Generation)
getNewGeneration genSize ioCache v it genWList mut =
    do
    let (bestGene, _) = maximumBy (cmpOn (fromRational . snd)) genWList
    --
    let ioGen1 = getCrossOvers genWList genSize mut
    --
    gen1 <- ioGen1
    --
    return (bestGene, bestGene:gen1)
    --
    where
    getCrossOvers :: GeneWeightedList -> Int -> Rational -> IO [Gene]
    getCrossOvers _ 0 _ = return []
    getCrossOvers genWList n mut =
        do
        mGene <- _CROSS_OVER_MUTATE_STRATEGY ioCache v it genWList mut
        case mGene of
            --
            Nothing -> getCrossOvers genWList n mut
            --
            Just gene -> do
                gen <- getCrossOvers genWList (n-1) mut
                return (gene:gen)

{-
    Outputs to file, in csv form, the state of the generation.
-}
outputDebug :: Integer -> (Gene, Generation) -> IO ()
outputDebug gNumber (bestGene, generation) = do
    ts <- (show . round . (*1000)) <$> getPOSIXTime

    fileExists <- doesFileExist _DEBUG_DESTINATION_FILE
    unless fileExists $
        let headers = ["TimeStamp(ms)", "Generation", "Average", "StandardDeviation", "BestGene"] ++ [ "Gene_" ++ (show i) | i <- [1..(length generation)]]
         in appendFile _DEBUG_DESTINATION_FILE ((concatMap (++ ", ") headers)++"\n")

    let debugLine =
            let bestCost = applyCurrencyExchange $ currEx (let (_, _, curr) = bestGene in curr)
                costsUSD = [p | g <- generation, let p = applyCurrencyExchange $ currEx (let (_, _, curr) = g in curr)]
                --
                avgCost = show $ avg costsUSD
                stdCost = show $ stdDev costsUSD
                --
                costs = show bestCost : map show costsUSD
                --
                outputCols = ts:(show gNumber):avgCost:stdCost:costs
                --
             in concatMap (++ ", ") outputCols ++ "\n"

    appendFile _DEBUG_DESTINATION_FILE debugLine

    return ()