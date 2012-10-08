{-# LANGUAGE OverloadedStrings #-}
module Shell.Core
    ( Event(..)
    , UIUpdate(..)
    , shell
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM (TChan, atomically, readTChan, writeTChan)
import Data.Attoparsec.ByteString.Char8 (string)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Conduit (Sink, GLSink, MonadThrow, ($$), ($$+), ($$++), (=$), await, leftover, yield)
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.Text as CT
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize, get, put, runGetPartial, runPut)
import qualified Data.Serialize as S
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (concat)

import Manipulator


data Event = CommandInput String [String]

data UIUpdate = NoUpdate
              | ShowOutput Text
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

getStreamText :: MonadThrow m => Sink ByteString m Text
getStreamText = do
    _ <- CA.sinkParser $ string startTag
    go =$ CT.decode CT.utf8 =$ (T.concat <$> CL.consume)
  where
    startTag, endTag :: IsString a => a
    startTag = "<stream>"
    endTag = "</stream>"
    go = do
        mbs <- await
        case mbs of
            Nothing -> error $ "End tag \"" ++ endTag ++ "\" is missing."
            Just bs
                | B.null y -> do
                    yield x
                    go
                | otherwise -> do
                    leftover $ B.drop (B.length endTag) y
                    yield x
              where
                (x, y) = B.breakSubstring endTag bs

shell :: TChan Event -> TChan UIUpdate -> CN.Application IO
shell fromUI0 toUI0 fromManipulator0 toManipulator0 = do
    (fromManipulator, ()) <- fromManipulator0 $$+ return ()
    go fromUI0 toUI0 fromManipulator toManipulator0
  where
    go fromUI toUI fromManipulator toManipulator = do
        e <- atomically $ readTChan fromUI
        case e of
            CommandInput "quit" _ -> atomically $ writeTChan toUI Shutdown
            CommandInput "show" _ -> do
                _ <- yield (runPut $ put Output) $$ toManipulator
                (fromManipulator', t) <- fromManipulator $$++ getStreamText
                atomically $ writeTChan toUI $ ShowOutput t
                go fromUI toUI fromManipulator' toManipulator
            CommandInput "" _ -> do
                atomically $ writeTChan toUI NoUpdate
                go fromUI toUI fromManipulator toManipulator
            CommandInput c args -> do
                _ <- yield (runPut $ put $ RunCommand c args) $$ toManipulator
                (fromManipulator', Right res) <- fromManipulator $$++ getResponse
                case res of
                    Fail err -> do
                        atomically $ writeTChan toUI $ ShowError err
                        go fromUI toUI fromManipulator' toManipulator
                    Success -> do
                        (fromManipulator'', t) <- fromManipulator' $$++ getStreamText
                        atomically $ writeTChan toUI $ ShowOutput t
                        go fromUI toUI fromManipulator'' toManipulator
