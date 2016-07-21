module Reflex.Dom.Sui.Button
  ( uiButton
  , uiButton'
  ) where

import Control.Applicative
import Data.Monoid
import Reflex.Dom

uiButton :: MonadWidget t m => String -> m () -> m (Event t ())
uiButton klass child = snd <$> uiButton' klass child

uiButton' :: MonadWidget t m => String -> m () -> m (El t, Event t ())
uiButton' klass child = do
  (e, _) <- elAttr' "button" ("class" =: klass <> "type" =: "button") child
  return (e, domEvent Click e)
