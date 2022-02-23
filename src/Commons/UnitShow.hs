module Commons.UnitShow where

import Numeric

{-|
    Returns a string containing the given unit formatted using the information provided:
    (number of decimals) (real float number to include) (string containing the unit)
-}
showUnit :: RealFloat a => Int -> a -> String -> String
showUnit nDecimals = Numeric.showFFloat (Just nDecimals)

{-|
    Takes a RealFloat and a string -> returns a concatenation of the given number to 4 decimals
    followed by the given string.
-}
showUnit4 :: RealFloat a => a -> String -> String
showUnit4 = showUnit 4