module Shell.UI.Commandline
    ( start
    ) where

import           Control.Applicative          ((<$>))
import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM       (atomically, newTChanIO,
                                               readTChan, writeTChan)
import           Control.Monad                (forever)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.Text.IO                 as T
import           System.Exit                  (exitSuccess)
import           System.IO                    (hFlush, stdout)

import           Manipulator
import           Mash.Core
import           Shell.Core


start :: Render a => Manipulator (ResourceT IO) a -> IO ()
start initState = do
    fromShell <- newTChanIO
    toShell <- newTChanIO

    _ <- forkIO $ runResourceT $ runServer "/tmp/mash_test" $ manipulator initState
    _ <- forkIO $                runClient "/tmp/mash_test" $ shell toShell fromShell

    forever $ do
        -- prompt
        putStr "> "
        hFlush stdout
        -- execute a command
        (cmd:args) <- words <$> getLine
        atomically $ writeTChan toShell $ CommandInput cmd args
        u <- atomically $ readTChan fromShell
        case u of
            NoUpdate -> return ()
            ShowOutput t -> T.putStrLn t
            ShowError err -> putStrLn $ "Error: " ++ err
            Shutdown -> exitSuccess
