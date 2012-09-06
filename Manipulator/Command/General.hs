module Manipulator.Command.General
    ( -- General commands
      gcmdResetPipeline
    ) where

import Data.Conduit (awaitForever, yield)
import Data.Map (Map)

import Manipulator.Core


gcmdResetPipeline :: Map String (CCommand String) -> GCommand
gcmdResetPipeline strCmds = GCommand $ \st -> st { manipPipe = awaitForever yield
                                                 , manipCtxCommands = strCmds
                                                 }
