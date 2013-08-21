module Main where

import qualified Data.ByteString      as B
import           Data.Conduit
import qualified Data.Conduit.List    as CL
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           Data.Word            (Word8)

import           Manipulator
import           Shell.UI.Commandline


-- Commands
generalCommands :: MonadResource m => Map String (GCommand m)
generalCommands = Map.fromList [("resetpl", gcmdResetPipeline bytesCommands), ("file", gcmdFileSource)]

bytesCommands :: MonadThrow m => Map String (Command m Bytes)
bytesCommands = Map.fromList [("appendB", cmdAppendBytes), ("decode", cmdDecodeUtf8 textCommands)]

textCommands :: Monad m => Map String (Command m Text)
textCommands = Map.fromList [("appendT", cmdAppendText)]

initState :: MonadResource m => Manipulator m Bytes
initState = Manipulator
    { manipSource = CL.sourceList [Bytes $ B.pack ([0..127]::[Word8])]
    , manipPipe = awaitForever yield
    , manipCommands = generalCommands
    , manipCtxCommands = bytesCommands
    }

main :: IO ()
main = start initState
