module Keyboard (
  Key(..),

  keyboard
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Char
import qualified Data.Map as M
import Data.Semigroup
import qualified Data.Text as T
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.KeyboardEvent as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Window as DOM
import Reflex
import Reflex.Dom.Core hiding (Key)

import Grid

topRow, middleRow, bottomRow :: [Char]
topRow    = "qwertyuiop"
middleRow = "asdfghjkl"
bottomRow = "zxcvbnm"


data Key = KeyChar Char
  | KeyEnter
  | KeyBackspace
 deriving (Eq, Ord, Show)


keyboard :: MonadWidget t m
  => Dynamic t [[(Char, Grade)]]
  -> EventWriterT t (First Key) m ()
keyboard gradedGuessesDyn = do
  let letterGradeDyn =
        fmap (fmap getMax . M.fromListWith (<>) . fmap (second Max) . concat)
             gradedGuessesDyn
  -- User key press events
  (userKeyboardE, userKeyboardTrigger) <- newTriggerEvent
  window <- DOM.currentWindowUnchecked
  document <- DOM.getDocument window
  _ <- DOM.liftJSM . DOM.on document DOM.keyUp $ do
    jsKey <- DOM.getKey =<< DOM.event
    mapM_ (liftIO . userKeyboardTrigger . First) . jsKeyToKey $ jsKey
  tellEvent $ userKeyboardE

  -- On-screen keyboard
  divClass "keyboard" $ do
    divClass "keyboard-row" $
      forM_ topRow $ \c -> charKey c . fmap (M.!? c) $ letterGradeDyn
    divClass "keyboard-row" $
      forM_ middleRow $ \c -> charKey c . fmap (M.!? c) $ letterGradeDyn
    divClass "keyboard-row" $ do
      enterE <- button "ENTER"
      tellEvent . fmap (const . First $ KeyEnter) $ enterE
      forM_ bottomRow $ \c -> charKey c . fmap (M.!? c) $ letterGradeDyn
      backspaceE <- button "⌫"
      tellEvent . fmap (const . First $ KeyBackspace) $ backspaceE

charKey :: MonadWidget t m
  => Char -> Dynamic t (Maybe Grade) -> EventWriterT t (First Key) m ()
charKey c gradeDyn = do
  let attrDyn = fmap (("class" =:) . maybe "" gradeCssClass) $ gradeDyn
  (buttonEl, _) <- elDynAttr' "button" attrDyn . text . T.pack . pure $ c
  tellEvent . fmap (const . First . KeyChar $ c) . domEvent Click $ buttonEl

jsKeyToKey :: String -> Maybe Key
jsKeyToKey key = do
  case key of
    "Enter"                -> Just KeyEnter
    "Backspace"            -> Just KeyBackspace
    (c:[])      | isChar c -> Just $ KeyChar c
    _                      -> Nothing
 where
  isChar c = 65 <= ord c && ord c <= 90 || 97 <= ord c && ord c <= 122
