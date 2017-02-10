{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Dom.Sui.Dropdown
  ( uiDropdown
  , uiDropdownStatic
  , uiDropdownMap
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom

import           Reflex.Dom.Sui.Icon

-- | Dropdown from dynamic list and a label function with an initial value.
--
-- This is slightly easier to implement the the Map version but requires the
-- label function to work for all possible as. That is fine for a common
-- case of small enum type. If you need the label function to change over time
-- use the Map version.
uiDropdown
    :: forall t m a. MonadWidget t m
    => Text
    -- ^ dropdown class
    -> Text
    -- ^ icon
    -> a
    -- ^ initial item
    -> Dynamic t [a]
    -- ^ list of items
    -> (a -> Text)
    -- ^ item to label function
    -> m (Event t a)
uiDropdown klass i a as f = do
    pb <- getPostBuild
    divClass klass $ do
      rec divClass "text" $ dynText selected
          icon i
          res <- divClass "menu" $
            widgetHold (return never) $
              (fmap leftmost . mapM (item f)) <$> leftmost [updated as, tagPromptlyDyn as pb]
          let ev = switch $ current res
          selected <- holdDyn (f a) (f <$> ev)
      return ev
  where
    item :: (a -> Text) -> a -> m (Event t a)
    item f a = do
      (e, _) <- elAttr' "div" ("class" =: "item") $
                  text $ f a
      return $ a <$ domEvent Click e

-- | Like 'uiDropdown' but showing static content instead of selected item.
uiDropdownStatic
    :: forall t m a. MonadWidget t m
    => Text
    -- ^ dropdown class
    -> Dynamic t [a]
    -- ^ list of items
    -> (a -> Text)
    -- ^ item to label function
    -> (m ())
    -- ^ static label
    -> m (Event t a)
uiDropdownStatic klass as f label = do
    pb <- getPostBuild
    divClass klass $ do
      label
      rec res <- divClass "menu" $
            widgetHold (return never) $
              (fmap leftmost . mapM (item f)) <$> leftmost [updated as, tagPromptlyDyn as pb]
          let ev = switch $ current res
      return ev
  where
    item :: (a -> Text) -> a -> m (Event t a)
    item f a = do
      (e, _) <- elAttr' "div" ("class" =: "item") $
                  text $ f a
      return $ a <$ domEvent Click e

-- | Like `uiDropdown` but takes a Map instead of a list.
uiDropdownMap
    :: forall t m k. (Ord k, MonadWidget t m)
    => Text
    -- ^ dropdown class
    -> Text
    -- ^ icon
    -> k
    -- ^ initial item
    -> Dynamic t (Map k Text)
    -- ^ list of items
    -> m (Event t k)
uiDropdownMap klass i initKey items = do
    divClass klass $ do
      rec divClass "text" $ dynText $ zipDynWith (\k -> fromMaybe "" . Map.lookup k) selectedKey items
          icon i
          res <- divClass "menu" $ listWithKey items $ \k dv -> item k dv
          let ev = switch $ current $ fmap (leftmost . Map.elems) res
          selectedKey <- holdDyn initKey ev
      return ev
  where
    item :: k -> Dynamic t Text -> m (Event t k)
    item k dv = do
      (e, _) <- elAttr' "div" ("class" =: "item") $ dynText dv
      return $ k <$ domEvent Click e
