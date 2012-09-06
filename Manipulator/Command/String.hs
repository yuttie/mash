module Manipulator.Command.String
    ( -- String commands
      cmdAppend1
    , cmdToInt
    ) where

import Data.Conduit ((=$=))
import qualified Data.Conduit.List as C
import Data.Map (Map)

import Manipulator.Core


cmdAppend1 :: CCommand String
cmdAppend1 = CCommand $ \st -> st { manipPipe = manipPipe st =$= C.map (++ "1") }

cmdToInt :: Map String (CCommand Int) -> CCommand String
cmdToInt intCmds = CCommand $ \st -> st { manipPipe = manipPipe st =$= C.map read
                                        , manipCtxCommands = intCmds
                                        }
