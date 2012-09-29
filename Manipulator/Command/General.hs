module Manipulator.Command.General
    ( -- General commands
      gcmdResetPipeline
    ) where

import Data.Conduit (awaitForever, yield)
import Data.Map (Map)

import Manipulator.Core


gcmdResetPipeline :: Map String (Command String) -> GCommand
gcmdResetPipeline strCmds = GCommand $ \st _ -> st { manipPipe = awaitForever yield
                                                   , manipCtxCommands = strCmds
                                                   }
