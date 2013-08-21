{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
module Manipulator.Core
    ( Manipulator(..)
    , ManipulatorError(..)
    , GCommand(..)
    , Command(..)
    , Render(..)
    , Bytes(..)
    , unbytes
    , Message(..)
    , Response(..)
    , manipulator
    ) where

import           Blaze.ByteString.Builder           (Builder)
import           Blaze.ByteString.Builder.Char.Utf8 (fromChar, fromString)
import           Control.Applicative                ((<$>), (<|>))
import           Control.Monad.Trans.Resource       (MonadUnsafeIO)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import           Data.Conduit                       (Conduit, Consumer,
                                                     ResumableSource, Sink,
                                                     Source, await, leftover,
                                                     yield, ($$), ($$+), ($$++),
                                                     ($=))
import           Data.Conduit.Blaze                 (builderToByteString)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid                        (mempty, (<>))
import           Data.Serialize                     (Serialize, get, put,
                                                     runGetPartial, runPut)
import qualified Data.Serialize                     as S
import           GHC.Generics
import           Numeric                            (showHex)


data Manipulator m a = Manipulator
    { manipSource      :: Source m Bytes
    , manipPipe        :: Conduit Bytes m a
    , manipCommands    :: Map String (GCommand m)
    , manipCtxCommands :: Map String (Command m a)
    }

data ManipulatorError = CommandArgumentError [String]
                      deriving (Show)

newtype GCommand m = GCommand (forall a. Render a => Command m a)

data Command m a = forall b. Render b => Command (Manipulator m a -> [String] -> Either ManipulatorError (Manipulator m b))

class Render a where
    render :: Monad m => Conduit a m Builder

renderStream :: (Render a, Monad m) => Conduit a m Builder
renderStream = do
    yield $ fromString "<stream>"
    r <- render
    yield $ fromString "</stream>"
    return r

newtype Bytes = Bytes ByteString

unbytes :: Bytes -> ByteString
unbytes (Bytes bs) = bs

instance Render Bytes where
    render = go mempty B.empty
      where
        go sep bs
            | B.length bs < 16 = await >>= maybe
                (close sep bs)
                (go sep . (bs <>) . unbytes)
            | otherwise = do
                let (x, y) = B.splitAt 16 bs
                yield $ sep <> fromBytes x
                go (fromChar '\n') y
        close sep bs
            | B.null bs = return ()
            | otherwise = yield (sep <> fromBytes bs) >> return ()
        fromBytes bs = B.foldl (\a b -> a <> fromChar ' ' <> fromByte b) (fromByte $ B.head bs) (B.tail bs)
        fromByte b
            | b < 0x10 = fromChar '0' <> fromString (showHex b "")
            | otherwise = fromString $ showHex b ""

data Message = Output
             | RunCommand String [String]
             deriving (Generic)

instance Serialize Message

data Response = Success
              | Fail String
              deriving (Generic)

instance Serialize Response

getMessage :: Monad m => Consumer ByteString m (Either String Message)
getMessage = go $ runGetPartial get
  where
    go p = do
        mbs <- await
        case p $ fromMaybe B.empty mbs of
            S.Fail err -> return $ Left err
            S.Partial p' -> go p'
            S.Done r lo -> leftover lo >> return (Right r)

lookupCommand :: Render a => String -> Manipulator m a -> Maybe (Command m a)
lookupCommand name (Manipulator _ _ gcs ccs) =
    Map.lookup name ccs <|> (asCommand <$> Map.lookup name gcs)
  where
    asCommand (GCommand c) = c

manipulator :: (MonadUnsafeIO m, Render a) => Manipulator m a -> Source m ByteString -> Sink ByteString m () -> m ()
manipulator st0 fromShell0 toShell0 = do
    (fromShell, ()) <- fromShell0 $$+ return ()
    go st0 fromShell toShell0
  where
    go :: (MonadUnsafeIO m, Render a) => Manipulator m a -> ResumableSource m ByteString -> Sink ByteString m () -> m ()
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
