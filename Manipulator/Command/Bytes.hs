module Manipulator.Command.Bytes
    ( -- Bytes commands
      cmdAppendBytes
    ) where

import qualified Data.ByteString as B
import Data.Conduit (GInfConduit, (=$=), awaitForever, yield)
import Data.Maybe (listToMaybe)
import Data.Word (Word8)

import Manipulator.Core


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
