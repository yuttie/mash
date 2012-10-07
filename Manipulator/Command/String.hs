module Manipulator.Command.String
    ( -- String commands
      cmdAppend
    , cmdToInt
    ) where

import Data.Conduit ((=$=))
import qualified Data.Conduit.List as C
import Data.Map (Map)

import Manipulator.Core
import Manipulator.Stream.Int ()
import Manipulator.Stream.String ()


cmdAppend :: Command String
cmdAppend = Command $ \st args -> case args of
    [s] -> Right $ st { manipPipe = manipPipe st =$= C.map (++ s) }
    _ -> Left $ CommandArgumentError args

cmdToInt :: Map String (Command Int) -> Command String
cmdToInt intCmds = Command $ \st args -> case args of
    [] -> Right $ st { manipPipe = manipPipe st =$= C.map read
                     , manipCtxCommands = intCmds
                     }
    _ -> Left $ CommandArgumentError args
