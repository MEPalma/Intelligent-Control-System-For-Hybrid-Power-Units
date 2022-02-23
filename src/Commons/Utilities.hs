{-
    Encapsulates a miscellaneous of general purpose functions.
-}
module Commons.Utilities where
--
import Debug.Trace
import Data.Maybe
import System.Console.ANSI
import System.Random.Shuffle
import Control.Monad.Random
import System.Random
import Data.List
import Control.Applicative


{-
    Returns an infinite list of the same object.
-}
endlessStream :: a -> [a]
endlessStream x = x : endlessStream x

{-
    Maps a function only on Just objects in a list.
-}
mapOnJust :: (a -> Maybe b) -> [a] -> [b]
mapOnJust _ [] = []
mapOnJust f (x:xs) =
            case f x of
                Just val -> val : mapOnJust f xs
                Nothing  -> mapOnJust f xs

{-
    Remove all empty lists.
-}
rmEmpties :: [[a]] -> [[a]]
rmEmpties all = [x | x <- all, not $ null x]

{-
    Removes all empty io lists, and converts
    the list to an io list.
-}
rmEmptiesIO :: [IO [a]] -> IO [[a]]
rmEmptiesIO [] = return []
rmEmptiesIO (iox:xs) =
    do
    x <- iox
    case x of
        [] -> rmEmptiesIO xs
        _  ->
            do
            fll <- rmEmptiesIO xs
            return (x:fll)

{-
    Given a list, a object, and an index,
    puts the object at the lists' index,
    replacing if necessary.
-}
replaceAt :: [a] -> a -> Int -> [a]
replaceAt [] _ _ = []
replaceAt (x:xs) x' 0 = x':xs
replaceAt (x:xs) x' n = x : replaceAt xs x' (n-1)

{-
    Applies a comparison function on two inputs.
-}
cmpOn :: (Ord o) => (a -> o) -> a -> a -> Ordering
cmpOn f x y = compare (f x) (f y)

{-
    Returns a infinite stream of increasing numbers
    from a starting number.
-}
range :: Num a => a -> a -> [a]
range curr step = curr : range (curr + step) step

{-
    Takes a given number of numbers from an infinite
    range of values.
-}
takeFromRange :: Num a => Integer -> a -> a -> [a]
takeFromRange n curr step = take (fromIntegral n) $ range curr step

{-
    Returns true if the given number is within (including)
    and upper and lower bound.
-}
inRange :: Ord a => a -> a -> a -> Bool
inRange num lower upper = (lower <= num) && (num <= upper)


{-
    Computes the average of a list of numbers.
-}
avg :: Floating a => [a] -> a
avg [] = 0
avg xs = sum xs / fromIntegral (length xs)

{-
    Computes the variance of a list of numbers.
-}
variance :: Floating a => [a] -> a
variance [] = 0
variance xs =
    let u = avg xs
     in sum [(x - u)^2 | x <- xs] / (fromIntegral (length xs) - 1)

{-
    Computes the standard deviation of a list of
    numbers.
-}
stdDev :: Floating a => [a] -> a
stdDev [] = 0
stdDev xs = let var = variance xs in sqrt var

{-
    Coverts the items in a list to their respective
    values in a sigma scale.
-}
toSigmaScale :: Floating a => [a] -> [a]
toSigmaScale [] = []
toSigmaScale xs =
    let u = avg xs
        a = 2 * stdDev xs
     in [(x-u)/a | x <- xs]

{-
    Of a least of objects, filters those which satisfy a
    given condition function.
-}
takeAllThat :: (a -> Bool) -> [a] -> [a]
takeAllThat f [] = []
takeAllThat f (x:xs)
    | f x = x : takeAllThat f xs
    | otherwise = takeAllThat f xs

{-
    Of a list of maybe monads, returns a list of the Just
    constructed values.
-}
filterJusts :: [Maybe a] -> [a]
filterJusts xs = [x | mx <- xs, isJust mx, let Just x = mx] 

{-
    Returns the second to last item in a list.
-}
sndToLast :: [a] -> a
sndToLast x =
    let lastIndex = (-1) + length x
     in x !! (lastIndex - 1)

{-
    Rounds a number to the closest multiple.
-}
roundToClosestMultiple :: Double -> Double -> Double
roundToClosestMultiple x m =
    let lower = fromIntegral (floor (x / m)) * m
        upper = lower + m
        --
        closest = if (x - lower) > (upper - x) then upper else lower
     in closest

{-
    Maps a number to a new range.
-}
mapToRange :: (Double, (Double, Double)) -> (Double, Double) -> Double
mapToRange (x, (a_min, a_max)) (b_min, b_max) = b_min + (((x - a_min)*(b_max - b_min)) / (a_max - a_min))

{-
    Applies a function on a input n times, appending
    each application to a list.
-}
applyN :: (a -> b) -> a -> Integer -> [b]
applyN _ _ 0 = []
applyN f x n = f x : applyN f x (n - 1)

{-
    Clears n lines in stdout.
-}
clearConsoleLines :: Integer -> IO ()
clearConsoleLines 0 = return ()
clearConsoleLines n =
    do
    cursorUp 1
    clearLine
    clearConsoleLines (n - 1)

{-
    Returns the first object to satisfy a given
     condition function.
-}
takeFirstThat :: (a -> Bool) -> [a] -> Maybe a
takeFirstThat _ [] = Nothing
takeFirstThat f (x:xs)
    | f x = Just x
    | otherwise = takeFirstThat f xs

{-
    Returns the fist Just constructed object
    in a list of maybe monads, Nothing if no
    Just value exists.
-}
takeFirstJust :: [Maybe a] -> Maybe a
takeFirstJust [] = Nothing
takeFirstJust (x:xs)
    | isJust x = x
    | otherwise = takeFirstJust xs

{-
    Returns a random integer in a given range.
-}
getRandomIntInRange :: Integer -> Integer -> IO Integer
getRandomIntInRange lbound ubound = randomRIO (lbound, ubound) :: IO Integer

{-
    Returns a random rational in a given range.
-}
getRandomRationalInRange :: Double -> Double -> IO Rational
getRandomRationalInRange lbound ubound = toRational <$> (randomRIO (lbound, ubound) :: IO Double)

{-
    Returns a random item in a given list.
-}
selectAtRandom :: [a] -> IO a
selectAtRandom l =
    do
    let len = length l
    rn <- randomRIO (0, len - 1) :: IO Int
    return $ l !! rn

{-
    Shuffles the order in the list.
-}
mix :: [a] -> IO [a]
mix l =
    do
    ge <- getStdGen
    let len = fromIntegral $ length l
    return $ shuffle' l len ge

{-
    Converts a list of ios to an io list.
-}
exportIO :: [IO a] -> IO [a]
exportIO [] = return []
exportIO (iox:xs) =
    do
    x <- iox
    ls <- exportIO xs
    return $ x : ls

{-
    If a list of io maybe monads, returns an io
    list of Just constructed objects.
-}
exportJustIO :: [IO (Maybe a)] -> IO [a]
exportJustIO [] = return []
exportJustIO (iomx:xs) =
    do
    mx <- iomx
    case mx of
        Nothing -> exportJustIO xs
        Just x ->
            do
            following <- exportJustIO xs
            return $ x : following


{-
    Returns a random item from the list according to its overall weighting.
-}
selectWeightedRandom :: [(a, Rational)] -> IO a
selectWeightedRandom wlst = head <$> genRandomsFromWeightedList wlst

{-
    Generates a random sequence of selection from a weighter list.
-}
genRandomsFromWeightedList :: [(a, Rational)] -> IO [a]
genRandomsFromWeightedList wlst = evalRandIO (sequence . repeat . fromList $! wlst)

{-
    Splits a list into two by drawing n items at random.
-}
randomSubset :: (Eq a) => Integer -> [a] -> IO ([a], [a])
randomSubset n lst
    | n <= 0 = return ([], lst)
    | otherwise = inner n [] lst
    where
    inner 0 s1 xs = return (s1, xs)
    inner n s1 xs =
        do
        -- Random select.
        tmp <- selectAtRandom xs
        -- Remove from incoming list.
        let newxs = delete tmp xs
        -- Add to new subsable.
        let news1 = tmp : s1
        --
        inner (n - 1) news1 newxs