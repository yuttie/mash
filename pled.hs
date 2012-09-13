{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception.Lifted (bracket, finally)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.Text as CT
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize, get, put, runGetPartial, runPut)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Network.Socket as NS
import System.Directory (doesFileExist, removeFile)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

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

data Event = CommandInput String

getResponse :: Monad m => GLSink ByteString m (Either String Response)
getResponse = go $ runGetPartial get
  where
    go p = do
        mbs <- await
        case p $ fromMaybe B.empty mbs of
            S.Fail err -> return $ Left err
            S.Partial p' -> go p'
            S.Done r lo -> leftover lo >> return (Right r)

decode :: MonadThrow m => Conduit ByteString m Text
decode = (CB.takeWhile (/= endOfStream) >> CB.drop 1) =$= CT.decode CT.utf8
  where
    endOfStream = 0

display :: GInfSink Text IO
display = CL.mapM_ T.putStr

shell :: TChan Event -> CN.Application IO
shell fromUI0 fromManipulator0 toManipulator0 = do
    (fromManipulator, ()) <- fromManipulator0 $$+ return ()
    go fromUI0 fromManipulator toManipulator0
  where
    go fromUI fromManipulator toManipulator = do
        e <- atomically $ readTChan fromUI
        case e of
            CommandInput c -> do
                yield (runPut $ put $ RunCommand c) $$ toManipulator
                (fromManipulator', Right res) <- fromManipulator $$++ getResponse
                case res of
                    Fail err -> do
                        hPutStrLn stderr err
                        go fromUI fromManipulator' toManipulator
                    Success -> do
                        (fromManipulator'', ()) <- fromManipulator'
                                                $$++ decode
                                                =$ display
                        go fromUI fromManipulator'' toManipulator

start :: Manipulator a -> IO ()
start initState = do
    toShell <- newTChanIO

    _ <- forkIO $ runServer "/tmp/mash_test" $ manipulator initState
    _ <- forkIO $ runClient "/tmp/mash_test" $ shell toShell

    forever $ do
        -- prompt
        putStr "> "
        hFlush stdout
        -- execute a command
        l <- getLine
        atomically $ writeTChan toShell $ CommandInput l

main :: IO ()
main = start initState
