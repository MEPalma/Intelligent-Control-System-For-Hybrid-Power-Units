module Itinerary.Parser.MainParser where

import Itinerary.Parser.Tokens
import Itinerary.Parser.Grammar
import Itinerary.Parser.UnexItinerary
import Itinerary.Itinerary
import Commons.ISUnits

import System.Environment

parseFile :: String -> IO Itinerary
parseFile f = do
    text <- readFile f
    let unexitinerary = parseCalc $ alexScanTokens text
    let itinerary = toItinerary unexitinerary
    return itinerary

parse :: String -> Itinerary
parse str = toItinerary $ parseCalc $ alexScanTokens str