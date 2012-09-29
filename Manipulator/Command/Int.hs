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
cmdDouble = Command $ \st _ -> st { manipPipe = manipPipe st =$= C.map (2 *) }

cmdToStr :: Map String (Command String) -> Command Int
cmdToStr strCmds = Command $ \st _ -> st { manipPipe = manipPipe st =$= C.map show
                                         , manipCtxCommands = strCmds
                                         }
