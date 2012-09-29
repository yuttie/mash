module Manipulator.Command.String
    ( -- String commands
      cmdAppend1
    , cmdToInt
    ) where

import Data.Conduit ((=$=))
import qualified Data.Conduit.List as C
import Data.Map (Map)

import Manipulator.Core


cmdAppend1 :: Command String
cmdAppend1 = Command $ \st _ -> st { manipPipe = manipPipe st =$= C.map (++ "1") }

cmdToInt :: Map String (Command Int) -> Command String
cmdToInt intCmds = Command $ \st _ -> st { manipPipe = manipPipe st =$= C.map read
                                         , manipCtxCommands = intCmds
                                         }
