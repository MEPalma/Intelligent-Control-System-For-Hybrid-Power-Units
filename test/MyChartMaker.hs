{-# LANGUAGE OverloadedStrings #-}

module MyChartMaker where
import Lucid
import Lucid.Html5
import Graphics.Plotly
import qualified Graphics.Plotly.Base as GPB
import Graphics.Plotly.Lucid
import Lens.Micro

import Data.Aeson.Types
import Data.Scientific

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Text.Internal (Text)
import Data.Text (pack)


data MyPlotData = MyScatter [(Double, Double)]
                | MyScatter3d [(Double, Double, Double)]
                | MyLine    [(Double, Double)]
                deriving (Show, Eq, Ord)


myPlotDataToCSV :: [MyPlotData] -> String
myPlotDataToCSV (x:xs) = show x ++ myPlotDataToCSV xs

genTraces :: [MyPlotData] -> [Trace]
genTraces [] = []
genTraces (m:ms) =
    case m of
        (MyScatter pts) -> points (aes & x .~ fst & y .~ snd) pts : genTraces ms
        (MyLine    pts) -> line   (aes & x .~ fst & y .~ snd) pts : genTraces ms
        (MyScatter3d pts) ->
            let tr = GPB.scatter3d & GPB.x ?~ map getx pts
                                   & GPB.y ?~ map gety pts
                                   & GPB.z ?~ map getz pts
                                   & mode ?~ [Markers]
             in tr : genTraces ms

getx (x, _, _) = Number $ read (show x)
gety (_, y, _) = Number $ read (show y)
getz (_, _, z) = Number $ read (show z)


makeChart :: FilePath -> String -> [MyPlotData] -> IO ()
makeChart _ _ [] = return ()
makeChart filePath chartName myPlotData = 
    T.writeFile filePath $ renderText $ doctypehtml_ $ do
    head_ $ do meta_ [charset_ "utf-8"]
               plotlyCDN
    body_ $ toHtml $ plotly "div" (genTraces myPlotData)
                                    & layout . height ?~ 1000
                                    & layout . title  ?~ pack chartName