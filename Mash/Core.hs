{-# LANGUAGE FlexibleContexts #-}
module Mash.Core
    ( runServer
    , runClient
    ) where

import Control.Concurrent (forkIO)
import Control.Exception.Lifted (bracket, finally)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.ByteString (ByteString)
import Data.Conduit (Source, Sink)
import qualified Data.Conduit.Network as CN
import qualified Network.Socket as NS
import System.Directory (doesFileExist, removeFile)


deleteFile :: FilePath -> IO ()
deleteFile fp = do
    exist <- doesFileExist fp
    when exist $
        removeFile fp

runServer :: (MonadIO m, MonadBaseControl IO m) => FilePath -> (Source m ByteString -> Sink ByteString m () -> m ()) -> m ()
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

runClient :: (MonadIO m, MonadBaseControl IO m) => FilePath -> (Source m ByteString -> Sink ByteString m () -> m ()) -> m ()
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
