module Shell.Core
    ( Event(..)
    , UIUpdate(..)
    , shell
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM (TChan, atomically, readTChan, writeTChan)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Conduit (Conduit, GInfConduit, GSink, GLSink, MonadThrow, ($$), ($$+), ($$++), (=$), (=$=), await, awaitForever, leftover, yield)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.Text as CT
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize, get, put, runGetPartial, runPut)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (concat)

import Manipulator


data Event = CommandInput String

data UIUpdate = ShowOutput Text
              | ShowError String
              | Shutdown

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

window :: Monad m => GInfConduit Text m Text
window = awaitForever yield

concat :: Monad m => GSink Text m Text
concat = T.concat <$> CL.consume

shell :: TChan Event -> TChan UIUpdate -> CN.Application IO
shell fromUI0 toUI0 fromManipulator0 toManipulator0 = do
    (fromManipulator, ()) <- fromManipulator0 $$+ return ()
    go fromUI0 toUI0 fromManipulator toManipulator0
  where
    go fromUI toUI fromManipulator toManipulator = do
        e <- atomically $ readTChan fromUI
        case e of
            CommandInput "quit" -> atomically $ writeTChan toUI Shutdown
            CommandInput "show" -> do
                _ <- yield (runPut $ put Output) $$ toManipulator
                (fromManipulator', t) <- fromManipulator $$++ decode =$ window =$ concat
                atomically $ writeTChan toUI $ ShowOutput t
                go fromUI toUI fromManipulator' toManipulator
            CommandInput c -> do
                _ <- yield (runPut $ put $ RunCommand c) $$ toManipulator
                (fromManipulator', Right res) <- fromManipulator $$++ getResponse
                case res of
                    Fail err -> do
                        atomically $ writeTChan toUI $ ShowError err
                        go fromUI toUI fromManipulator' toManipulator
                    Success -> do
                        (fromManipulator'', t) <- fromManipulator' $$++ decode =$ window =$ concat
                        atomically $ writeTChan toUI $ ShowOutput t
                        go fromUI toUI fromManipulator'' toManipulator
