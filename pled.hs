{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO


data State a = State a

data Command a = forall b. Show b => Command (State a -> (State b, Map String (Command b)))

-- String commands
stringCommands :: Map String (Command String)
stringCommands = Map.fromList [("append", commandAppend1), ("toint", commandToInt)]

commandAppend1 :: Command String
commandAppend1 = Command $ \(State v) -> (State $ v ++ "1", stringCommands)

commandToInt :: Command String
commandToInt = Command $ \(State v) -> (State $ read v, intCommands)

-- Int commands
intCommands :: Map String (Command Int)
intCommands = Map.fromList [("double", commandDouble), ("tostr", commandToStr)]

commandDouble :: Command Int
commandDouble = Command $ \(State v) -> (State $ 2 * v, intCommands)

commandToStr :: Command Int
commandToStr = Command $ \(State v) -> (State $ show v, stringCommands)

process :: Show a => State a -> Map String (Command a) -> IO ()
process s@(State v) cmds = do
    putStrLn $ "State: " ++ show v
    putStr "> "
    hFlush stdout
    l <- getLine
    case Map.lookup l cmds of
        Nothing -> do
            hPutStrLn stderr $ "Unknown command " ++ show l ++ "."
            process s cmds
        Just (Command f) -> do
            let (s', cmds') = f s
            process s' cmds'

main :: IO ()
main = forever $ do
    process (State "1") stringCommands
    return ()