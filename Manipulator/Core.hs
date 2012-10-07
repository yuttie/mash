{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Manipulator.Core
    ( Manipulator(..)
    , ManipulatorError(..)
    , GCommand(..)
    , Command(..)
    , Render(..)
    , Message(..)
    , Response(..)
    , manipulator
    ) where

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.Applicative ((<$>), (<|>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Conduit (Source, Conduit, Sink, ResumableSource, GInfConduit, GLSink, ($$), ($$+), ($$++), ($=), await, leftover, yield)
import Data.Conduit.Blaze (builderToByteString)
import qualified Data.Conduit.Network as CN
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize, get, put, runGetPartial, runPut)
import qualified Data.Serialize as S
import GHC.Generics


data Manipulator a = Manipulator
    { manipSource :: Source IO String
    , manipPipe :: Conduit String IO a
    , manipCommands :: Map String GCommand
    , manipCtxCommands :: Map String (Command a)
    }

data ManipulatorError = CommandArgumentError [String]
                      deriving (Show)

newtype GCommand = GCommand (forall a. Command a)

data Command a = forall b. Render b => Command (Manipulator a -> [String] -> Either ManipulatorError (Manipulator b))

class Render a where
    render :: Monad m => GInfConduit a m Builder

renderStream :: (Render a, Monad m) => GInfConduit a m Builder
renderStream = do
    yield $ fromString "<stream>"
    r <- render
    yield $ fromString "</stream>"
    return r

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

lookupCommand :: String -> Manipulator a -> Maybe (Command a)
lookupCommand name (Manipulator _ _ gcs ccs) =
    Map.lookup name ccs <|> (asCommand <$> Map.lookup name gcs)
  where
    asCommand (GCommand c) = c

manipulator :: Render a => Manipulator a -> CN.Application IO
manipulator st0 fromShell0 toShell0 = do
    (fromShell, ()) <- fromShell0 $$+ return ()
    go st0 fromShell toShell0
  where
    go :: Render a => Manipulator a -> ResumableSource IO ByteString -> Sink ByteString IO () -> IO ()
    go st@(Manipulator src pipe _ _) fromShell toShell = do
        (fromShell', Right msg) <- fromShell $$++ getMessage
        case msg of
            Output -> do
                src $= pipe $= renderStream $= builderToByteString $$ toShell
                go st fromShell' toShell
            RunCommand name args -> case lookupCommand name st of
                Just (Command f) -> case f st args of
                    Right st'@(Manipulator src' pipe' _ _) -> do
                        yield (runPut $ put Success) $$ toShell
                        src' $= pipe' $= renderStream $= builderToByteString $$ toShell
                        go st' fromShell' toShell
                    Left err -> do
                        yield (runPut $ put $ Fail $ show err) $$ toShell
                        go st fromShell' toShell
                Nothing -> do
                    yield (runPut $ put $ Fail $ "Unknown command " ++ show name ++ ".") $$ toShell
                    go st fromShell' toShell
