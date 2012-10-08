module Manipulator.Stream.Text
    (
    ) where

import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Data.Conduit (awaitForever, yield)
import Data.Text (Text)

import Manipulator.Core


instance Render Text where
    render = awaitForever (yield . fromText)
