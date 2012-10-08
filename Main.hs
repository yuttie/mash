module Main where

import qualified Data.ByteString as B
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Word (Word8)

import Manipulator
import Shell.UI.Commandline


-- Commands
generalCommands :: Map String GCommand
generalCommands = Map.fromList [("resetpl", gcmdResetPipeline bytesCommands)]

bytesCommands :: Map String (Command Bytes)
bytesCommands = Map.fromList [("appendB", cmdAppendBytes), ("decode", cmdDecodeUtf8 textCommands)]

textCommands :: Map String (Command Text)
textCommands = Map.fromList [("appendT", cmdAppendText)]

initState :: Manipulator Bytes
initState = Manipulator
    { manipSource = CL.sourceList [Bytes $ B.pack ([0..127]::[Word8])]
    , manipPipe = awaitForever yield
    , manipCommands = generalCommands
    , manipCtxCommands = bytesCommands
    }

main :: IO ()
main = start initState
