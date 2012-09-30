module Manipulator.Command.Int
    ( -- Int commands
      cmdMult
    , cmdToStr
    ) where

import Data.Conduit ((=$=))
import qualified Data.Conduit.List as C
import Data.Map (Map)
import Data.Maybe (listToMaybe)

import Manipulator.Core


maybeRead :: Read a => String -> Maybe a
maybeRead s = case listToMaybe $ reads s of
    Just (x, []) -> Just x
    _ -> Nothing

cmdMult :: Command Int
cmdMult = Command $ \st args -> case args of
    [s] -> case (maybeRead s :: Maybe Int) of
        Just n -> Right $ st { manipPipe = manipPipe st =$= C.map (n *) }
        Nothing -> Left $ CommandArgumentError args
    _ -> Left $ CommandArgumentError args

cmdToStr :: Map String (Command String) -> Command Int
cmdToStr strCmds = Command $ \st args -> case args of
    [] -> Right $ st { manipPipe = manipPipe st =$= C.map show
                     , manipCtxCommands = strCmds
                     }
    _ -> Left $ CommandArgumentError args
