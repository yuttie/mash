{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Manipulator.Core
    ( Manipulator(..)
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


data Manipulator a = Manipulator
    { manipSource :: Source IO String
    , manipPipe :: Conduit String IO a
    , manipCommands :: Map String GCommand
    , manipCtxCommands :: Map String (CCommand a)
    }

data GCommand = forall b. Show b => GCommand (forall a. Manipulator a -> Manipulator b)

data CCommand a = forall b. Show b => CCommand (Manipulator a -> Manipulator b)

data Message = RunCommand String

data AnySource = forall a. Show a => AnySource (Source IO a)

process :: Show a => TChan Message -> TChan AnySource -> Manipulator a -> IO ()
process inp out st@(Manipulator s p gcs ccs) = do
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
