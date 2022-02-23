{-
 Data type and conversion functions for currencies.
-}
module Commons.Currency where
--
import Commons.UnitShow
import Text.Printf (printf)


data CurrencyExchange =
        CurrencyExchange
            { currValue   :: Double
            , currExToUSD :: Double
            }
--
instance Show CurrencyExchange where
    show ce = printf "%s (%s)" (showUnit 8 (currValue ce) "") (showUnit4 (currExToUSD ce) (show USD))
--
instance Eq CurrencyExchange where
    (==) ce ce' = applyCurrencyExchange ce == applyCurrencyExchange ce'
    (/=) ce ce' = not $ (==) ce ce'
--
instance Ord CurrencyExchange where
    compare ce ce' = compare (applyCurrencyExchange ce) (applyCurrencyExchange ce')
--
instance Num CurrencyExchange where
    (+) = mathOperationOnCurrencyExchange (+)
    (*) = mathOperationOnCurrencyExchange (*)
    (-) = mathOperationOnCurrencyExchange (/)
    --
    negate = singleMathOperationOnCurrencyExchange negate
    abs = singleMathOperationOnCurrencyExchange abs
    signum = singleMathOperationOnCurrencyExchange signum
    --
    (fromInteger) int = CurrencyExchange (fromInteger int) 1 -- this is USD!
--
instance Fractional CurrencyExchange where
    (/) = mathOperationOnCurrencyExchange (/)
    --
    recip = singleMathOperationOnCurrencyExchange recip
    --
    fromRational rat = CurrencyExchange (fromRational rat) 1 -- this is USD!
--
applyCurrencyExchange :: CurrencyExchange -> Double
applyCurrencyExchange (CurrencyExchange val exc) = val * exc
--
mathOperationOnCurrencyExchange :: (Double -> Double -> Double) -> CurrencyExchange -> CurrencyExchange -> CurrencyExchange
mathOperationOnCurrencyExchange f ce ce' =
    let ceUSD  = applyCurrencyExchange ce  -- first value to usd
        ce'USD = applyCurrencyExchange ce' -- second value to usd
        --
        newUSD = f ceUSD ce'USD
        --
        newUSDinCeExc = newUSD / currExToUSD ce
     in ce {currValue = newUSDinCeExc}
--
singleMathOperationOnCurrencyExchange :: (Double -> Double) -> CurrencyExchange -> CurrencyExchange
singleMathOperationOnCurrencyExchange f ce = ce {currValue = f (currValue ce)}


data CurrencyName = EUR | GBP | USD deriving (Show, Eq)
--
data Currency =
        Currency
            { currName :: CurrencyName
            , currEx   :: CurrencyExchange
            }
--
instance Show Currency where
    show c = printf "%s %s" (show $ currName c) (show $ currEx c)
--
instance Eq Currency where
    (==) c c' = currEx c == currEx c'
    (/=) c c' = currEx c /= currEx c'
--
instance Ord Currency where
    compare c c' = compare (currEx c) (currEx c')
--
instance Num Currency where
   (+) c c' = c {currEx = (+) (currEx c) (currEx c')}
   (*) c c' = c {currEx = (*) (currEx c) (currEx c')}
   (-) c c' = c {currEx = (-) (currEx c) (currEx c')}
   --
   negate c = c {currEx = negate (currEx c)}
   (abs) c = c {currEx = abs (currEx c)}
   (signum) c = c {currEx = signum (currEx c)}
   --
   (fromInteger) int = Currency USD (fromInteger int) -- this is USD!
--
instance Fractional Currency where
    (/) c c' = c {currEx = (/) (currEx c) (currEx c')}
    --
    recip c = c {currEx = recip (currEx c)}
    --
    fromRational rat =  Currency USD (fromRational rat) -- this is USD!