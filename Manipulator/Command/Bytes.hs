{-# LANGUAGE RankNTypes #-}
module Manipulator.Command.Bytes
    ( -- Bytes commands
      cmdAppendBytes
    , cmdDecodeUtf8
    ) where

import qualified Data.ByteString as B
import Data.Conduit (GInfConduit, (=$=), awaitForever, yield)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Word (Word8)

import Manipulator.Core
import Manipulator.Stream.Text ()


append :: Monad m => a -> GInfConduit a m a
append x = do
    r <- awaitForever yield
    yield x
    return r

maybeRead :: Read a => String -> Maybe a
maybeRead s = case listToMaybe $ reads s of
    Just (x, []) -> Just x
    _ -> Nothing

cmdAppendBytes :: Command Bytes
cmdAppendBytes = Command $ \st args ->
    case mapM maybeRead args :: Maybe [Word8] of
        Just bs -> Right $ st { manipPipe = manipPipe st =$= append (Bytes $ B.pack bs) }
        Nothing -> Left $ CommandArgumentError args

cmdDecodeUtf8 :: Map String (Command Text) -> Command Bytes
cmdDecodeUtf8 textCmds = Command $ \st args -> case args of
    [] -> Right $ st { manipPipe = manipPipe st =$= CL.map unbytes =$= CT.decode CT.utf8
                     , manipCtxCommands = textCmds
                     }
    _ -> Left $ CommandArgumentError args
