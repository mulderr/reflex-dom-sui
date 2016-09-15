{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.Sui.Icon
  ( icon
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom

icon :: DomBuilder t m => Text -> m ()
icon i = elClass "i" (T.unwords [i, "icon"]) blank
