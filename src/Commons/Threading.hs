module Commons.Threading where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.Chan

import System.Timeout
import Debug.Trace

{-
    Forwards a message to all channels.
-}
writeAllToChan :: Chan a -> [a] -> IO ()
writeAllToChan _ [] = return ()
writeAllToChan chan (s:ss) =
    do
    writeChan chan s
    writeAllToChan chan ss


{-
    Given an exit condition, an exit operation and a boolean channel, if in a timeout of 1 
    microsecond, a false is piped in the channel, it maps the exit operation on each thread,
    otherwise continues monitoring the channel.
-}
handleUserKey :: (Char -> Bool) -> ([ThreadId] -> IO ()) -> Chan Bool -> [ThreadId] -> IO ()
handleUserKey exitCondition exitOp overrideExit threadIds =
    do
    mcmd <- timeout 1 $ readChan overrideExit
    --
    case mcmd of
        Nothing -> 
            do
            muserInput <- timeout 1 getChar
            case muserInput of
                Nothing -> handleUserKey exitCondition exitOp overrideExit threadIds
                Just userInput ->
                    if exitCondition userInput
                        then exitOp threadIds
                        else handleUserKey exitCondition exitOp overrideExit threadIds
        --
        Just cmd -> if cmd
                    then exitOp threadIds
                    else handleUserKey exitCondition exitOp overrideExit threadIds


{-
    Given a condition, a function to invoke on a list of threads, and a list of
    threads, invokes this function whenever the condition is met on stdin,
    otherwise keeps monitoring the state of the function.
-}
handleUserLine :: (String -> Bool) -> ([ThreadId] -> IO ()) -> [ThreadId] -> IO ()
handleUserLine exitCondition exitOp threadIds =
  do
  userInput <- getLine
  if exitCondition userInput
    then exitOp threadIds
    else handleUserLine exitCondition exitOp threadIds



{-
    Kills all but invoking thread.
-}
killAll :: [ThreadId] -> IO ()
killAll [] = return ()
killAll(x:xs) =
  do
  killThread x
  killAll xs


{-
    Kills all threads, as well as invoking thread.
-}
killAllAndMySelf :: [ThreadId] -> IO ()
killAllAndMySelf threadIds =
    do
    myId <- myThreadId
    killAll (threadIds ++ [myId])
