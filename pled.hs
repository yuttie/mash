{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception.Lifted (bracket, finally)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Network.Socket as NS
import System.Directory (doesFileExist, removeFile)
import System.IO

import Manipulator


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

deleteFile :: FilePath -> IO ()
deleteFile fp = do
    exist <- doesFileExist fp
    when exist $
        removeFile fp

runServer :: (MonadIO m, MonadBaseControl IO m) => FilePath -> CN.Application m -> m ()
runServer fp app = bracket
    (liftIO prepareSocket)
    (liftIO . NS.sClose)
    (forever . serve)
  where
    prepareSocket = do
        lsock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
        deleteFile fp
        NS.bindSocket lsock $ NS.SockAddrUnix fp
        NS.listen lsock NS.maxListenQueue
        return lsock
    serve lsock = do
        (sock, _) <- liftIO $ NS.accept lsock
        let src = CN.sourceSocket sock
        let sink = CN.sinkSocket sock
        control $ \run -> do
            _ <- forkIO $ void $ run (app src sink) `finally` NS.sClose sock
            run $ return ()

runClient :: (MonadIO m, MonadBaseControl IO m) => FilePath -> CN.Application m -> m ()
runClient fp app = bracket
    (liftIO prepareSocket)
    (liftIO . NS.sClose)
    (doSomething)
  where
    prepareSocket = do
        sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
        NS.connect sock $ NS.SockAddrUnix fp
        return sock
    doSomething sock = do
        let src = CN.sourceSocket sock
        let sink = CN.sinkSocket sock
        app src sink

main :: IO ()
main = do
    inp <- newTChanIO
    out <- newTChanIO

    _ <- forkIO $ forever $ do
        -- output the current state
        AnySource s <- atomically $ readTChan out
        output <- s $$ CL.consume
        putStrLn $ "Output: " ++ show output

    _ <- forkIO $ process inp out initState

    forever $ do
        -- prompt
        putStr "> "
        hFlush stdout
        -- execute a command
        l <- getLine
        atomically $ writeTChan inp $ RunCommand l
