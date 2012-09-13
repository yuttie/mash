module Manipulator.Command.Int
    ( -- Int commands
      cmdDouble
    , cmdToStr
    ) where

import Data.Conduit ((=$=))
import qualified Data.Conduit.List as C
import Data.Map (Map)

import Manipulator.Core


cmdDouble :: CCommand Int
cmdDouble = CCommand $ \st -> st { manipPipe = manipPipe st =$= C.map (2 *) }

cmdToStr :: Map String (CCommand String) -> CCommand Int
cmdToStr strCmds = CCommand $ \st -> st { manipPipe = manipPipe st =$= C.map show
                                        , manipCtxCommands = strCmds
                                        }
