module Manipulator.Command.Int
    ( -- Int commands
      cmdDouble
    , cmdToStr
    ) where

import Data.Conduit ((=$=))
import qualified Data.Conduit.List as C
import Data.Map (Map)

import Manipulator.Core


cmdDouble :: Command Int
cmdDouble = Command $ \st args -> case args of
    [] -> Right $ st { manipPipe = manipPipe st =$= C.map (2 *) }
    _ -> Left $ CommandArgumentError args

cmdToStr :: Map String (Command String) -> Command Int
cmdToStr strCmds = Command $ \st args -> case args of
    [] -> Right $ st { manipPipe = manipPipe st =$= C.map show
                     , manipCtxCommands = strCmds
                     }
    _ -> Left $ CommandArgumentError args
