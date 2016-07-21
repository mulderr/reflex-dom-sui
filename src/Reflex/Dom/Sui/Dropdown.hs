module Reflex.Dom.Sui.Dropdown
  ( uiDropdown
  , uiDropdownStatic
  , uiDropdownMap
  ) where

import Reflex.Dom

-- | Dropdown from dynamic list and a label function with an initial value.
--
-- This is slightly easier to implement the the Map version but requires the
-- label function to work for all possible as. That is fine for a common
-- case of small enum type. If you need the label function to change over time
-- use the Map version.
uiDropdown
    :: forall t m a. MonadWidget t m
    => String
    -- ^ dropdown class
    -> String
    -- ^ icon
    -> a
    -- ^ initial item
    -> Dynamic t [a]
    -- ^ list of items
    -> (a -> String)
    -- ^ item to label function
    -> m (Event t a)
uiDropdown klass i a as f = do
    pb <- getPostBuild
    divClass klass $ do
      rec divClass "text" $ dynText selected
          icon i
          res <- divClass "menu" $
            widgetHold (return never) $
              (fmap leftmost . mapM (item f)) <$> leftmost [updated as, tagDyn as pb]
          let ev = switch $ current res
          selected <- holdDyn (f a) (f <$> ev)
      return ev
  where
    item :: (a -> String) -> a -> m (Event t a)
    item f a = do
      (e, _) <- elAttr' "div" ("class" =: "item") $
                  text $ f a
      return $ a <$ domEvent Click e

-- | Like 'uiDropdown' but showing static content instead of selected item.
uiDropdownStatic
    :: forall t m a. MonadWidget t m
    => String
    -- ^ dropdown class
    -> Dynamic t [a]
    -- ^ list of items
    -> (a -> String)
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
              (fmap leftmost . mapM (item f)) <$> leftmost [updated as, tagDyn as pb]
          let ev = switch $ current res
      return ev
  where
    item :: (a -> String) -> a -> m (Event t a)
    item f a = do
      (e, _) <- elAttr' "div" ("class" =: "item") $
                  text $ f a
      return $ a <$ domEvent Click e

-- | Like `uiDropdown` but takes a Map instead of a list.
uiDropdownMap
    :: forall t m k. (Ord k, MonadWidget t m)
    => String
    -- ^ dropdown class
    -> String
    -- ^ icon
    -> k
    -- ^ initial item
    -> Dynamic t (Map k String)
    -- ^ list of items
    -> m (Event t k)
uiDropdownMap klass i initKey items = do
    divClass klass $ do
      rec divClass "text" $ dynText =<< combineDyn (\k -> fromMaybe "" . Map.lookup k) selectedKey items
          icon i
          res <- divClass "menu" $ listWithKey items $ \k dv -> item k dv
          dev <- mapDyn (leftmost . Map.elems) res
          let ev = switch $ current dev
          selectedKey <- holdDyn initKey ev
      return ev
  where
    item :: k -> Dynamic t String -> m (Event t k)
    item k dv = do
      (e, _) <- elAttr' "div" ("class" =: "item") $ dynText dv
      return $ k <$ domEvent Click e
