{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Conduit
import qualified Data.Conduit.List as C
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO


data Editor a = Editor
    { edSource :: Source IO String
    , edPipe :: Conduit String IO a
    , edCommands :: Map String GCommand
    , edCtxCommands :: Map String (CCommand a)
    }

data GCommand = forall b. Show b => GCommand (forall a. Editor a -> Editor b)
data CCommand a = forall b. Show b => CCommand (Editor a -> Editor b)

-- General commands
generalCommands :: Map String GCommand
generalCommands = Map.fromList [("resetpl", gcmdResetPipeline)]

gcmdResetPipeline :: GCommand
gcmdResetPipeline = GCommand $ \st -> st { edPipe = edPipe initState
                                         , edCtxCommands = edCtxCommands initState
                                         }

-- String commands
stringCommands :: Map String (CCommand String)
stringCommands = Map.fromList [("append", cmdAppend1), ("toint", cmdToInt)]

cmdAppend1 :: CCommand String
cmdAppend1 = CCommand $ \st -> st { edPipe = edPipe st =$= C.map (++ "1") }

cmdToInt :: CCommand String
cmdToInt = CCommand $ \st -> st { edPipe = edPipe st =$= C.map read
                               , edCtxCommands = intCommands
                               }

-- Int commands
intCommands :: Map String (CCommand Int)
intCommands = Map.fromList [("double", cmdDouble), ("tostr", cmdToStr)]

cmdDouble :: CCommand Int
cmdDouble = CCommand $ \st -> st { edPipe = edPipe st =$= C.map (2 *) }

cmdToStr :: CCommand Int
cmdToStr = CCommand $ \st -> st { edPipe = edPipe st =$= C.map show
                               , edCtxCommands = stringCommands
                               }

data Message = RunCommand String

data AnySource = forall a. Show a => AnySource (Source IO a)

process :: Show a => TChan Message -> TChan AnySource -> Editor a -> IO ()
process inp out st@(Editor s p gcs ccs) = do
    atomically $ writeTChan out $ AnySource (s $= p)
    msg <- atomically $ readTChan inp
    case msg of
        RunCommand c -> case Map.lookup c ccs of
            Just (CCommand f) -> process inp out $ f st
            Nothing -> case Map.lookup c gcs of
                Just (GCommand f) -> process inp out $ f st
                Nothing -> do
                    hPutStrLn stderr $ "Unknown command " ++ show c ++ "."
                    process inp out st

initState :: Editor String
initState = Editor
    { edSource = C.sourceList $ map show ([1..10]::[Int])
    , edPipe = awaitForever yield
    , edCommands = generalCommands
    , edCtxCommands = stringCommands
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
