{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Manipulator.Core
    ( Manipulator(..)
    , GCommand(..)
    , Command(..)
    , Message(..)
    , Response(..)
    , ToMarkup(..)
    , manipulator
    ) where

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Char.Utf8 (fromChar, fromString)
import Control.Applicative ((<$>), (<|>))
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Conduit (Source, Conduit, Sink, ResumableSource, GInfConduit, GLSink, ($$), ($$+), ($$++), ($=), (=$=), await, awaitE, leftover, yield)
import Data.Conduit.Blaze (builderToByteString)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize, get, put, runGetPartial, runPut)
import qualified Data.Serialize as S
import GHC.Generics
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Renderer.Utf8 (renderMarkupBuilder)


data Manipulator a = Manipulator
    { manipSource :: Source IO String
    , manipPipe :: Conduit String IO a
    , manipCommands :: Map String GCommand
    , manipCtxCommands :: Map String (Command a)
    }

newtype GCommand = GCommand (forall a. Command a)

data Command a = forall b. ToMarkup b => Command (Manipulator a -> [String] -> Manipulator b)

data Message = Output
             | RunCommand String [String]
             deriving (Generic)

instance Serialize Message

data Response = Success
              | Fail String
              deriving (Generic)

instance Serialize Response

getMessage :: Monad m => GLSink ByteString m (Either String Message)
getMessage = go $ runGetPartial get
  where
    go p = do
        mbs <- await
        case p $ fromMaybe B.empty mbs of
            S.Fail err -> return $ Left err
            S.Partial p' -> go p'
            S.Done r lo -> leftover lo >> return (Right r)

render :: (Monad m, ToMarkup a) => Conduit a m Builder
render = do
    yield $ fromString "<stream>"
    CL.map (renderMarkupBuilder . toMarkup) =$= unlinesB
    yield $ fromString "</stream>"

unlinesB :: Monad m => GInfConduit Builder m Builder
unlinesB = loop True
  where
    loop first = do
        et <- awaitE
        case et of
            Left r -> return r
            Right b -> do
                unless first $ yield $ fromChar '\n'
                yield b
                loop False

lookupCommand :: String -> Manipulator a -> Maybe (Command a)
lookupCommand name (Manipulator _ _ gcs ccs) =
    Map.lookup name ccs <|> (asCommand <$> Map.lookup name gcs)
  where
    asCommand (GCommand c) = c

manipulator :: ToMarkup a => Manipulator a -> CN.Application IO
manipulator st0 fromShell0 toShell0 = do
    (fromShell, ()) <- fromShell0 $$+ return ()
    go st0 fromShell toShell0
  where
    go :: ToMarkup a => Manipulator a -> ResumableSource IO ByteString -> Sink ByteString IO () -> IO ()
    go st@(Manipulator src pipe _ _) fromShell toShell = do
        (fromShell', Right msg) <- fromShell $$++ getMessage
        case msg of
            Output -> do
                src $= pipe $= render $= builderToByteString $$ toShell
                go st fromShell' toShell
            RunCommand name args -> case lookupCommand name st of
                Just (Command f) -> do
                    yield (runPut $ put Success) $$ toShell
                    let st'@(Manipulator src' pipe' _ _) = f st args
                    src' $= pipe' $= render $= builderToByteString $$ toShell
                    go st' fromShell' toShell
                Nothing -> do
                    yield (runPut $ put $ Fail $ "Unknown command " ++ show name ++ ".") $$ toShell
                    go st fromShell' toShell
