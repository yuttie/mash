{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad
import Data.Conduit
import qualified Data.Conduit.List as C
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO


data Editor a = Editor
    { edSource :: Source IO String
    , edPipe :: Conduit String IO a
    , edCommands :: Map String (Command a)
    }

data Command a = forall b. Show b => Command (Editor a -> Editor b)

-- String commands
stringCommands :: Map String (Command String)
stringCommands = Map.fromList [("append", cmdAppend1), ("toint", cmdToInt)]

cmdAppend1 :: Command String
cmdAppend1 = Command $ \st -> st { edPipe = edPipe st =$= C.map (++ "1") }

cmdToInt :: Command String
cmdToInt = Command $ \st -> st { edPipe = edPipe st =$= C.map read
                               , edCommands = intCommands
                               }

-- Int commands
intCommands :: Map String (Command Int)
intCommands = Map.fromList [("double", cmdDouble), ("tostr", cmdToStr)]

cmdDouble :: Command Int
cmdDouble = Command $ \st -> st { edPipe = edPipe st =$= C.map (2 *) }

cmdToStr :: Command Int
cmdToStr = Command $ \st -> st { edPipe = edPipe st =$= C.map show
                               , edCommands = stringCommands
                               }

process :: Show a => Editor a -> IO ()
process st@(Editor s p cs) = do
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
    case Map.lookup l cs of
        Nothing -> do
            hPutStrLn stderr $ "Unknown command " ++ show l ++ "."
            process st
        Just (Command f) -> process $ f st

initState :: Editor String
initState = Editor
    { edSource = C.sourceList $ map show ([1..10]::[Int])
    , edPipe = awaitForever yield
    , edCommands = stringCommands
    }

main :: IO ()
main = forever $ process initState
