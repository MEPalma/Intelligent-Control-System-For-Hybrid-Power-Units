{-# LANGUAGE FlexibleContexts #-}
module KNNRegression.KNN where

import KNNRegression.KNNRegressionClass as KNNRegressionClass
import KNNRegression.KNNConfig

import Data.Hashable
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Heap as Heap


import Debug.Trace

type Telemetry a = HashMap.HashMap Int a


euclidean :: (KNNRegressionClass a) => [Double] -> a -> Double
euclidean fv a = sqrt $ Prelude.sum $ Prelude.map (\(x, y) -> (x - y)^2) (zip fv (toVector a))



weightedAverage :: (KNNRegressionClass a) => KNNConfig -> [(Double, a)] -> Double
weightedAverage _ [] = 0
weightedAverage c pts =
    let nex = knnConfigAvgCoeff c
        nnei = knnConfigNeighbors c (length pts)
        --
        i = sum 
            [ getValue t / (dist^nex)
            | (d, t) <- take nnei pts
            , let dist = if d /= 0 then d else 1
            ]
        --
        j = sum
            [ 1 / (dist^nex)
            | (d, _) <- take nnei pts
            , let dist = if d /= 0 then d else 1
            ]
        --
     in i / j



add :: (KNNRegressionClass a) => Maybe (Telemetry a) -> a -> Telemetry a
--
add (Just map) new =
    let key = hash new in
    case HashMap.lookup key map of
        --
        (Just a) ->
                let updated = KNNRegressionClass.updateOnMatch a (normalize new)
                in HashMap.insert key updated (HashMap.delete key map)
        --
        Nothing  -> HashMap.insert key (normalize new) map
--
add Nothing new = add (Just initCollection) new


addAll :: (KNNRegressionClass a) => Maybe (Telemetry a) -> [a] -> Telemetry a
addAll (Just map) [] = map
addAll Nothing    [] = initCollection
addAll mmap (n:ns) = addAll (Just (add mmap n)) ns


valueOfClosest :: (KNNRegressionClass a) => KNNConfig -> a -> Telemetry a -> Double
valueOfClosest c node map = 
    let fv = toVector node
        -- Extract all KNNRegressionClass in map :: [KNNRegressionClass].
        all = HashMap.elems map
        --
        -- For each Telemetry class, build a list of (euclideanDistance, KNNRegressionClass).
        allPairs = [(euclidean fv x, x) | x <- all]
        --
        -- Add all generated pairs in a new heap :: MinPrioHeap Double KNNRegressionClass.
        emptyHeap = Heap.empty :: (KNNRegressionClass a) => Heap.MinPrioHeap Double a
        heap = Prelude.foldl (flip Heap.insert) emptyHeap allPairs
        --
        -- Bests.
        bests = Heap.toAscList heap
        --
        avg = weightedAverage c bests
        --
     in avg
