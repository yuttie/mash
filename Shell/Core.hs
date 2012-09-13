module Shell.Core
    ( Event(..)
    , shell
    ) where

import Control.Concurrent.STM (TChan, atomically, readTChan)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Conduit (Conduit, Sink, GLSink, MonadThrow, ($$), ($$+), ($$++), (=$), (=$=), await, leftover, yield)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.Text as CT
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize, get, put, runGetPartial, runPut)
import qualified Data.Serialize as S
import Data.Text (Text)
import System.IO (hPutStrLn, stderr)

import Manipulator


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

shell :: TChan Event -> Sink Text IO () -> CN.Application IO
shell fromUI0 toUI0 fromManipulator0 toManipulator0 = do
    (fromManipulator, ()) <- fromManipulator0 $$+ return ()
    go fromUI0 toUI0 fromManipulator toManipulator0
  where
    go fromUI toUI fromManipulator toManipulator = do
        e <- atomically $ readTChan fromUI
        case e of
            CommandInput c -> do
                yield (runPut $ put $ RunCommand c) $$ toManipulator
                (fromManipulator', Right res) <- fromManipulator $$++ getResponse
                case res of
                    Fail err -> do
                        hPutStrLn stderr err
                        go fromUI toUI fromManipulator' toManipulator
                    Success -> do
                        (fromManipulator'', ()) <- fromManipulator'
                                                $$++ decode
                                                =$ toUI
                        go fromUI toUI fromManipulator'' toManipulator
