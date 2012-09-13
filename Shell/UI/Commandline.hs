module Shell.UI.Commandline
    ( start
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTChanIO, writeTChan)
import Control.Monad (forever)
import Data.Conduit (GInfSink)
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.IO (hFlush, stdout)

import Manipulator
import Mash.Core
import Shell.Core


display :: GInfSink Text IO
display = CL.mapM_ T.putStr

start :: Manipulator a -> IO ()
start initState = do
    toShell <- newTChanIO

    _ <- forkIO $ runServer "/tmp/mash_test" $ manipulator initState
    _ <- forkIO $ runClient "/tmp/mash_test" $ shell toShell display

    forever $ do
        -- prompt
        putStr "> "
        hFlush stdout
        -- execute a command
        l <- getLine
        atomically $ writeTChan toShell $ CommandInput l
