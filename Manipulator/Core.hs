{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Manipulator.Core
    ( Manipulator(..)
    , GCommand(..)
    , CCommand(..)
    , Message(..)
    , Response(..)
    , AnySource(..)
    , manipulator
    ) where

import Control.Monad (unless)
import Control.Monad.Trans.Resource (MonadThrow)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Conduit (Source, Conduit, Sink, ResumableSource, GInfConduit, GLSink, ($$), ($$+), ($$++), ($=), (=$=), await, awaitE, leftover, yield)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.Text as CT
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize, get, put, runGetPartial, runPut)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Generics
import Prelude hiding (unlines)


data Manipulator a = Manipulator
    { manipSource :: Source IO String
    , manipPipe :: Conduit String IO a
    , manipCommands :: Map String GCommand
    , manipCtxCommands :: Map String (CCommand a)
    }

data GCommand = forall b. Show b => GCommand (forall a. Manipulator a -> Manipulator b)

data CCommand a = forall b. Show b => CCommand (Manipulator a -> Manipulator b)

data Message = Output
             | RunCommand String
             deriving (Generic)

instance Serialize Message

data Response = Success
              | Fail String
              deriving (Generic)

instance Serialize Response

data AnySource = forall a. Show a => AnySource (Source IO a)

getMessage :: Monad m => GLSink ByteString m (Either String Message)
getMessage = go $ runGetPartial get
  where
    go p = do
        mbs <- await
        case p $ fromMaybe B.empty mbs of
            S.Fail err -> return $ Left err
            S.Partial p' -> go p'
            S.Done r lo -> leftover lo >> return (Right r)

render :: (Monad m, Show a) => Conduit a m Text
render = CL.map (T.pack . show) =$= unlines

encode :: MonadThrow m => Conduit Text m ByteString
encode = CT.encode CT.utf8 =$= snoc endOfStream
  where
    endOfStream = 0

unlines :: Monad m => GInfConduit Text m Text
unlines = loop True
  where
    loop first = do
        et <- awaitE
        case et of
            Left r -> return r
            Right t -> do
                unless first $ yield $ T.singleton '\n'
                yield t
                loop False

snoc :: Monad m => Word8 -> GInfConduit ByteString m ByteString
snoc b = loop
  where
    loop = do
        ebs <- awaitE
        case ebs of
            Left r -> do
                yield $ B.singleton b
                return r
            Right bs -> do
                yield bs
                loop

manipulator :: Show a => Manipulator a -> CN.Application IO
manipulator st0 fromShell0 toShell0 = do
    (fromShell, ()) <- fromShell0 $$+ return ()
    go st0 fromShell toShell0
  where
    go :: Show a => Manipulator a -> ResumableSource IO ByteString -> Sink ByteString IO () -> IO ()
    go st@(Manipulator src pipe gcs ccs) fromShell toShell = do
        (fromShell', Right msg) <- fromShell $$++ getMessage
        case msg of
            Output -> do
                src $= pipe $= render $= encode $$ toShell
                go st fromShell' toShell
            RunCommand c -> case Map.lookup c ccs of
                Just (CCommand f) -> do
                    yield (runPut $ put Success) $$ toShell
                    let st'@(Manipulator src' pipe' _ _) = f st
                    src' $= pipe' $= render $= encode $$ toShell
                    go st' fromShell' toShell
                Nothing -> case Map.lookup c gcs of
                    Just (GCommand f) -> do
                        yield (runPut $ put Success) $$ toShell
                        let st'@(Manipulator src' pipe' _ _) = f st
                        src' $= pipe' $= render $= encode $$ toShell
                        go st' fromShell' toShell
                    Nothing -> do
                        yield (runPut $ put $ Fail $ "Unknown command " ++ show c ++ ".") $$ toShell
                        go st fromShell' toShell
