{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Main where

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

process :: Show a => Editor a -> IO ()
process st@(Editor s p gcs ccs) = do
    -- output the current state
    input <- s $$ C.consume
    output <- s $= p $$ C.consume
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Output: " ++ show output
    -- prompt
    putStr "> "
    hFlush stdout
    -- execute a command
    l <- getLine
    case Map.lookup l ccs of
        Nothing -> case Map.lookup l gcs of
            Nothing -> do
                hPutStrLn stderr $ "Unknown command " ++ show l ++ "."
                process st
            Just (GCommand f) -> process $ f st
        Just (CCommand f) -> process $ f st

initState :: Editor String
initState = Editor
    { edSource = C.sourceList $ map show ([1..10]::[Int])
    , edPipe = awaitForever yield
    , edCommands = generalCommands
    , edCtxCommands = stringCommands
    }

main :: IO ()
main = process initState
