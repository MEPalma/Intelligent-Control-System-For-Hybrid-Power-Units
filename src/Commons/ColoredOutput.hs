module Commons.ColoredOutput where


data Color = BLACK | RED | GREEN | YELLOW | BLUE | MAGENTA | CYAN | WHITE deriving (Eq)
--
colorCode :: Color -> String
colorCode BLACK   = "\x1b[30m"
colorCode RED     = "\x1b[31m"
colorCode GREEN   = "\x1b[32m"
colorCode YELLOW  = "\x1b[33m"
colorCode BLUE    = "\x1b[34m"
colorCode MAGENTA = "\x1b[35m"
colorCode CYAN    = "\x1b[36m"
colorCode WHITE   = "\x1b[37m"
--
_CLOSE_COLOR = "\x1b[0m"


showInColor :: String -> Color -> String
showInColor x c = colorCode c ++ x ++ _CLOSE_COLOR

showItemInColor :: Show a => a -> Color -> String
showItemInColor x = showInColor (show x)

showInBox :: String -> String
showInBox x = sect ++ "\n| " ++ x ++ " |\n" ++ sect
    where
    len = length x
    sect = "+" ++ replicate (len + 2) '-' ++ "+"

showInBoxColor :: String -> Color -> String
showInBoxColor str = showInColor (showInBox str)

printInColor :: String -> Color -> IO ()
printInColor x c = putStr $! showInColor x c

printlnInColor :: String -> Color -> IO ()
printlnInColor x c = putStrLn $! showInColor x c

