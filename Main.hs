module Main where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Map (Map)
import qualified Data.Map as Map

import Manipulator
import Shell.UI.Commandline


-- Commands
generalCommands :: Map String GCommand
generalCommands = Map.fromList [("resetpl", gcmdResetPipeline stringCommands)]

stringCommands :: Map String (CCommand String)
stringCommands = Map.fromList [("append", cmdAppend1), ("toint", cmdToInt intCommands)]

intCommands :: Map String (CCommand Int)
intCommands = Map.fromList [("double", cmdDouble), ("tostr", cmdToStr stringCommands)]

initState :: Manipulator String
initState = Manipulator
    { manipSource = CL.sourceList $ map show ([1..10]::[Int])
    , manipPipe = awaitForever yield
    , manipCommands = generalCommands
    , manipCtxCommands = stringCommands
    }

main :: IO ()
main = start initState
