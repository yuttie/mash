{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Manipulator.Core
    ( Editor(..)
    , GCommand(..)
    , CCommand(..)
    , Message(..)
    , AnySource(..)
    , process
    ) where

import Control.Concurrent.STM
import Data.Conduit (Source, Conduit, ($=))
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
