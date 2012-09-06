module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Conduit
import qualified Data.Conduit.List as C
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO

import Manipulator


-- Commands
generalCommands :: Map String GCommand
generalCommands = Map.fromList [("resetpl", gcmdResetPipeline stringCommands)]

stringCommands :: Map String (CCommand String)
stringCommands = Map.fromList [("append", cmdAppend1), ("toint", cmdToInt intCommands)]

intCommands :: Map String (CCommand Int)
intCommands = Map.fromList [("double", cmdDouble), ("tostr", cmdToStr stringCommands)]

initState :: Manipulator String
initState = Manipulator
    { manipSource = C.sourceList $ map show ([1..10]::[Int])
    , manipPipe = awaitForever yield
    , manipCommands = generalCommands
    , manipCtxCommands = stringCommands
    }

main :: IO ()
main = do
    inp <- newTChanIO
    out <- newTChanIO

    _ <- forkIO $ forever $ do
        -- output the current state
        AnySource s <- atomically $ readTChan out
        output <- s $$ C.consume
        putStrLn $ "Output: " ++ show output

    _ <- forkIO $ process inp out initState

    forever $ do
        -- prompt
        putStr "> "
        hFlush stdout
        -- execute a command
        l <- getLine
        atomically $ writeTChan inp $ RunCommand l
