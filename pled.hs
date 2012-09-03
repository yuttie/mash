{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad
import Data.Conduit
import qualified Data.Conduit.List as C
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO


data Editor a = Editor
    { source :: Source IO String
    , pipe :: Conduit String IO a
    , commands :: Map String (Command a)
    }

data Command a = forall b. Show b => Command (Editor a -> Editor b)

-- String commands
stringCommands :: Map String (Command String)
stringCommands = Map.fromList [("append", commandAppend1), ("toint", commandToInt)]

commandAppend1 :: Command String
commandAppend1 = Command $ \(Editor s p cs) -> Editor s (p =$= C.map (++ "1")) cs

commandToInt :: Command String
commandToInt = Command $ \(Editor s p _) -> Editor s (p =$= C.map read) intCommands

-- Int commands
intCommands :: Map String (Command Int)
intCommands = Map.fromList [("double", commandDouble), ("tostr", commandToStr)]

commandDouble :: Command Int
commandDouble = Command $ \(Editor s p cs) -> Editor s (p =$= C.map (2 *)) cs

commandToStr :: Command Int
commandToStr = Command $ \(Editor s p _) -> Editor s (p =$= C.map show) stringCommands

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
    { source = C.sourceList $ map show ([1..10]::[Int])
    , pipe = awaitForever yield
    , commands = stringCommands
    }

main :: IO ()
main = forever $ process initState