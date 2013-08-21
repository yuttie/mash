module Manipulator.Command.General
    ( -- General commands
      gcmdResetPipeline
    , gcmdFileSource
    ) where

import Data.Conduit (MonadResource, (=$=), awaitForever, yield)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Map (Map)

import Manipulator.Core


gcmdResetPipeline :: Monad m => Map String (Command m Bytes) -> GCommand m
gcmdResetPipeline cmds = GCommand $ Command $ \st args -> case args of
    [] -> Right $ st { manipPipe = awaitForever yield
                     , manipCtxCommands = cmds
                     }
    _ -> Left $ CommandArgumentError args

gcmdFileSource :: MonadResource m => GCommand m
gcmdFileSource = GCommand $ Command $ \st args -> case args of
    [fp] -> Right $ st { manipSource = CB.sourceFile fp =$= CL.map Bytes }
    _ -> Left $ CommandArgumentError args
