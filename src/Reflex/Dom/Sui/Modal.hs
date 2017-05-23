{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Dom.Sui.Modal
  ( ModalConfig (..)
  , Modal (..)
  , uiModal
  , uiRemovingModal
  , mkUiModalBody
  , mkUiModalHeader
  , mkUiModalFooter
  ) where

import           Control.Monad (mapM_, liftM2, void, when)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Bool (bool)
import           Data.Default
import           Data.Either (isRight)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHCJS.DOM (currentDocument, currentWindow)
import           GHCJS.DOM.CSSStyleDeclaration (getPropertyValue, setProperty)
import           GHCJS.DOM.Document (Document, createElement, getBody)
import qualified GHCJS.DOM.DOMTokenList as TL
import           GHCJS.DOM.HTMLElement (HTMLElement, focus)
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.Event as Event
import           GHCJS.DOM.EventM (EventM, target)
import           GHCJS.DOM.Node (appendChild, removeChild, isSameNode)
import           GHCJS.DOM.Types (MonadJSM)
import qualified GHCJS.DOM.Types as DOM
import           GHCJS.DOM.Window (getComputedStyle, getInnerWidth)
import           Reflex
import           Reflex.Dom
import           Text.Read (readMaybe)

import           Reflex.Dom.Sui.Button
import           Reflex.Dom.Sui.Icon


data ModalConfig
  = ModalConfig { _modalConfig_attributes :: Map Text Text
                , _modalConfig_dimmerAttrs :: Map Text Text
                , _modalConfig_dimmerClose :: Bool
                , _modalConfig_escClose :: Bool
                , _modalConfig_bodyOpenClasses :: [Text]
                }

instance Default ModalConfig where
  def = ModalConfig { _modalConfig_attributes = "class" =: "ui scrolling modal transition scale in"
                    , _modalConfig_dimmerAttrs = "class" =: "ui modals page dimmer transition fade in" <> "style" =: "overflow: auto;"
                    , _modalConfig_dimmerClose = True
                    , _modalConfig_escClose = True
                    , _modalConfig_bodyOpenClasses = ["dimmable", "dimmed", "scrolling"]
                    }

data Modal t a
  = Modal { _modal_close :: Event t ()
          , _modal_result :: a
          }

------------------------------------------------------------------------------
-- | Modal using semantic-ui css.
--
-- Modified from Reflex.Dom.Contrib.Widgets.Modal

uiModal
  :: MonadWidget t m
  => ModalConfig
  -> Event t Bool
  -- ^ Event to open and/or close the modal
  -> m (a, Event t ())
  -- ^ Widget rendering the body of the modal. Returns an event with a
  -- success value and an event triggering the close of the modal.
  -> m (Modal t a)
uiModal cfg@(ModalConfig attrs dimmerAttrs dimmerClose escClose bodyCls) showm body = do
    rec let visE = leftmost [showm, False <$ closem]
        modalHacks cfg visE
        (resE, closem) <- go =<< holdDyn False visE
    return $ Modal closem resE
  where
    go vis = do
        let mattrs = fmap (\b -> attrs <&> visibility b <> "tabindex" =: "-1") vis
            dattrs = fmap (\b -> dimmerAttrs <&> visibility b) vis
        (dimmer, (modal, (res, close))) <- elDynAttr' "div" dattrs $
            elDynAttr' "div" mattrs body
        setFocus modal $ ffilter (==True) $ updated vis
        setScrollTop dimmer 0 $ ffilter (==True) $ updated vis
        dimmerClick <- doDimmer dimmerClose dimmer
        return (res, close <> dimmerClick <> doEsc escClose dimmer)

    visibility True = "class" =: "visible active" <> "style" =: "outline: none;"
    visibility False = mempty

    doDimmer True e = domEventOwn Click e
    doDimmer False _ = return never

    doEsc True = fmap (const ()) . ffilter (== 27) . domEvent Keydown
    doEsc False = const never

uiRemovingModal
  :: MonadWidget t m
  => ModalConfig
  -> Event t a
  -> (a -> m (b, Event t ()))
  -> m (Dynamic t (Maybe b))
uiRemovingModal cfg@(ModalConfig attrs dimmerAttrs dimmerClose escClose bodyCls) showm body = do
  rec let visE = leftmost [Just <$> showm, Nothing <$ closem]
      (r, closem) <- do
        res <- widgetHold (domRemover Nothing) (fmap domRemover visE)
        let e = switch $ current $ fmap snd res
        return (fmap fst res, e)
  modalHacks cfg $ fmap isJust visE
  return r

  where
    domRemover Nothing = return (Nothing, never)
    domRemover (Just a) = do
      pb <- getPostBuild
      (dimmer, (modal, (res, closem))) <- elAttr' "div" (dimmerAttrs <&> ("class" =: "visible active")) $
        elAttr' "div" (attrs <&> ("class" =: "visible active" <> "style" =: "outline: none;" <> "tabindex" =: "-1")) $ do
          (r, closem) <- body a
          return (Just r, closem)
      setFocus modal pb
      setScrollTop dimmer 0 pb
      dimmerClick <- doDimmer dimmerClose dimmer
      return (res, closem <> dimmerClick <> doEsc escClose dimmer)

    doDimmer True e = domEventOwn Click e
    doDimmer False _ = return never

    doEsc True = fmap (const ()) . ffilter (== 27) . domEvent Keydown
    doEsc False = const never

infixr 6 <&>

(<&>) :: Map Text Text -> Map Text Text -> Map Text Text
x <&> y = Map.unionWith (\a b -> a <> " " <> b) x y
{-# INLINE (<&>) #-}

------------------------------------------------------------------------------
modalHacks :: MonadWidget t m => ModalConfig -> Event t Bool -> m ()
modalHacks cfg e = performEvent_ $ fmap runHacks e
  where
    runHacks True = jsOpen $ map T.unpack cs
    runHacks False = jsClose $ map T.unpack cs
    cs = _modalConfig_bodyOpenClasses cfg

jsOpen :: MonadJSM m => [String] -> m ()
jsOpen cl = withDocBody $ \doc body -> do
  -- if we don't compensate for missing scrollbar the content may move
  -- slightly to the right which creates an ugly effect, the idea is taken
  -- from bootstrap
  isOverflowing <- isBodyOverflowing
  when isOverflowing $ jsFixPadding body (+)
  mapM_ (addClass body) cl

jsClose :: MonadJSM m => [String] -> m ()
jsClose cl = withDocBody $ \doc body -> do
  mapM_ (removeClass body) cl
  isOverflowing <- isBodyOverflowing
  when isOverflowing $ jsFixPadding body (-)

jsFixPadding :: MonadJSM m => HTMLElement -> (Double -> Double -> Double) -> m ()
jsFixPadding e f = do
  scw <- measureScrollbar
  ops <- css e "padding-right"
  let op = fromMaybe 0 $ readMaybe $ T.unpack $ T.dropEnd 2 (T.pack ops)
  s <- E.getStyle e
  setProperty s ("padding-right" :: String) (Just $ show (op `f` scw) <> ("px" :: String)) (Nothing :: Maybe String)

------------------------------------------------------------------------------
-- Semantic-ui templates.

mkUiModalBody
  :: MonadWidget t m
  => m (Event t ())
  -- ^ A header widget returning an event that closes the modal.
  -> (Dynamic t (Either e a) -> m (Event t (), Event t ()))
  -- ^ Footer widget that takes the current state of the body and returns
  -- a pair of a cancel event and an ok event.
  -> m (Dynamic t (Either e a))
  -> m (Event t (Either e a), Event t ())
mkUiModalBody header footer body = do
  dismissE <- header
  bodyRes <- divClass "content" body
  (submitE, cancelE) <- footer bodyRes
  let resE = tagPromptlyDyn bodyRes submitE
      closem = leftmost [dismissE, cancelE, () <$ ffilter isRight resE]
  return (resE, closem)

-- | Template for a basic modal header with a given title.
mkUiModalHeader :: MonadWidget t m => Text -> m (Event t ())
mkUiModalHeader title = do
  divClass "header" $ text title
  return $ never

-- | Template for a basic footer with submit and cancel buttons.
mkUiModalFooter :: MonadWidget t m => Text -> Text -> Dynamic t (Either e a) -> m (Event t (), Event t ())
mkUiModalFooter submitText cancelText _ = divClass "actions" $ do
  cancelE <- uiButton "ui black deny button" $ text cancelText
  text " "
  submitE <- uiButton "ui positive right labeled icon button" $ do
    text submitText
    icon "checkmark"
  return (submitE, cancelE)

------------------------------------------------------------------------------
-- Low-level helpers.

-- | Wraps 'defaultDomEventHandler' adding event target.
eventWithTarget :: (DOM.IsEvent (EventType en), E.IsElement e, DOM.IsGObject t)
  => e
  -> EventName en
  -> EventM e (EventType en) (Maybe (t, (EventResultType en)))
eventWithTarget e en = do
  t <- target
  mr <- fmap unEventResult <$> defaultDomEventHandler e en
  return $ (,) <$> Just t <*> mr

-- | Only occurs if target of the event is the given element. Ignores bubbles from children.
domEventOwn :: forall t m en . (MonadWidget t m, DOM.IsEvent (EventType en)) => EventName en -> El t -> m (Event t (EventResultType en))
domEventOwn en e = do
  ev <- wrapDomEvent (DOM.uncheckedCastTo DOM.HTMLElement el) (onEventName en) $ eventWithTarget el en
  res <- performEvent $ ffor (ev :: Event t (Maybe (DOM.HTMLElement, EventResultType en))) $ \mx ->
    case mx of
      Nothing -> return Nothing
      Just (t, r) -> bool Nothing (Just r) <$> isSameNode el (Just t)
  return $ fmapMaybe id res
  where
    el = _element_raw e

setFocus :: (MonadWidget t m) => El t -> Event t a -> m ()
setFocus e ev = do
  dev <- delay 0.01 ev
  performEvent_ $ (focus $ DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw e) <$ dev

setScrollTop :: (MonadWidget t m) => El t -> Int -> Event t a -> m ()
setScrollTop e v ev = do
  dev <- delay 0.01 ev
  performEvent_ $ (E.setScrollTop (_element_raw e) v) <$ dev

------------------------------------------------------------------------------

-- | Add css class to element.
addClass :: (MonadJSM m, E.IsElement self) => self -> String -> m ()
addClass el klass = withCL el $ \cl -> TL.add cl [klass]

-- | Remove css class from element.
removeClass :: (MonadJSM m, E.IsElement self) => self -> String -> m ()
removeClass el klass = withCL el $ \cl -> TL.remove cl [klass]

-- | Do stuff to element's class list.
withCL :: (MonadJSM m, E.IsElement self) => self -> (TL.DOMTokenList -> m ()) -> m ()
withCL el f = E.getClassList el >>= f

-- | Do stuff with document and body.
withDocBody :: (MonadJSM m) => (Document -> HTMLElement -> m a) -> m a
withDocBody f = do
  Just doc <- currentDocument
  Just body <- getBody doc
  f doc body

-- | Gets the computed style.
-- window.getComputedStyle(elem, null).getPropertyValue(prop)
css :: (MonadJSM m) => HTMLElement -> String -> m String
css e prop = do
  Just w <- currentWindow
  s <- getComputedStyle w e (Nothing :: Maybe String)
  getPropertyValue s prop

-- | Measure scrollbar width. Adapted from bootstrap's modal.js.
-- This is ugly. Ideally the browser would somehow expose scrollbar width.
measureScrollbar :: MonadJSM m => m Double
measureScrollbar = withDocBody $ \doc body -> do
  scrollDiv <- createElement doc ("div" :: String)
  E.setAttribute scrollDiv ("style" :: String) measureStyle
  appendChild body scrollDiv
  w <- liftM2 (-) (E.getOffsetWidth scrollDiv) (E.getClientWidth scrollDiv)
  removeChild body scrollDiv
  return w
  where
    measureStyle = "position: absolute; top: -9999px; width: 50px; height: 50px; overflow: scroll;" :: String

-- document.body.clientWidth < window.innerWidth
isBodyOverflowing :: MonadJSM m => m Bool
isBodyOverflowing = withDocBody $ \_ body -> do
  Just w <- currentWindow
  liftM2 (<) (E.getClientWidth body) (fromIntegral <$> getInnerWidth w)
