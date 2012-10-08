module Manipulator.Command.General
    ( -- General commands
      gcmdResetPipeline
    ) where

import Data.Conduit (awaitForever, yield)
import Data.Map (Map)

import Manipulator.Core


gcmdResetPipeline :: Map String (Command Bytes) -> GCommand
gcmdResetPipeline cmds = GCommand $ Command $ \st args -> case args of
    [] -> Right $ st { manipPipe = awaitForever yield
                     , manipCtxCommands = cmds
                     }
    _ -> Left $ CommandArgumentError args
