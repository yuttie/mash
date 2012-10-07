{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Manipulator.Stream.String
    (
    ) where

import Blaze.ByteString.Builder.Char.Utf8 (fromChar, fromShow)
import Data.Conduit (awaitE, awaitForever, yield)
import Data.Monoid ((<>))

import Manipulator.Core


instance Render String where
    render = awaitE >>= either return (\i0 -> do
        yield $ fromShow i0
        awaitForever $ \i -> do
            yield $ fromChar ' ' <> fromShow i)
