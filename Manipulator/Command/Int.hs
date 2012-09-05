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
cmdDouble = CCommand $ \st -> st { edPipe = edPipe st =$= C.map (2 *) }

cmdToStr :: Map String (CCommand String) -> CCommand Int
cmdToStr strCmds = CCommand $ \st -> st { edPipe = edPipe st =$= C.map show
                                        , edCtxCommands = strCmds
                                        }
