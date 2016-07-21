module Reflex.Dom.Sui.Icon
  ( icon
  ) where

import Reflex.Dom

icon :: MonadWidget t m => String -> m ()
icon i = elClass "i" (unwords [i, "icon"]) blank
