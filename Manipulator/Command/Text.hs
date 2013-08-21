{-# LANGUAGE RankNTypes #-}
module Manipulator.Command.Text
    ( -- Text commands
      cmdAppendText
    ) where

import           Data.Conduit            (Conduit, awaitForever, yield, (=$=))
import           Data.Text               (Text, pack)

import           Manipulator.Core
import           Manipulator.Stream.Text ()


append :: Monad m => a -> Conduit a m a
append x = do
    r <- awaitForever yield
    yield x
    return r

cmdAppendText :: Monad m => Command m Text
cmdAppendText = Command $ \st args -> case args of
    [s] -> Right $ st { manipPipe = manipPipe st =$= append (pack s) }
    _ -> Left $ CommandArgumentError args
