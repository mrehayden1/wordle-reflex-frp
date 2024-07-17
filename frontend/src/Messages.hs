module Messages (
  messages
) where

import Control.Monad.IO.Class
import Data.Char
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Clock
import qualified Data.Text as T
import Reflex
import Reflex.Dom.Core hiding (Key)

displayDuration :: NominalDiffTime
displayDuration = 2.25

messages :: MonadWidget t m
  => Set String
  -> String
  -> Dynamic t [String]
  -> Event t String
  -> m ()
messages dictionary dailyWord guessesDyn submitE = do
  messageE <- fmap updated . widgetHold (return undefined)
    . fmap (\msg -> fmap (, msg) (liftIO getCurrentTime))
    . attachWithMaybe message (current guessesDyn)
    $ submitE
  let insertMsgE = uncurry M.insert <$> messageE
  removeMsgE <- fmap (fmap (M.delete . fst)) . delay displayDuration $ messageE
  let messageUpdateE = mergeWith (.) [insertMsgE, removeMsgE]
  messageDyn <- foldDyn ($) mempty messageUpdateE
  divClass "message-wrapper" $ do
    _ <- list messageDyn $ \guessDyn ->
      divClass "message" . dynText $
        fmap T.pack guessDyn
    return ()
 where
  message :: [String] -> String -> Maybe String
  message guesses guess
    | length guess < 5               = Just "Your guess is too short."
    | guess `S.notMember` dictionary = Just "Word not in list."
    | guess == dailyWord             = Just . winMessage . (+ 1)
                                         . length
                                         $ guesses
    -- Valid guess but not equal to the daily word
    | length guesses < 5             = Nothing
    | (c:cs) <- dailyWord            = Just $ "Bad luck! The answer was \""
                                          ++ (toUpper c : cs) ++ "\"."

  winMessage :: Int -> String
  winMessage i =
    case i of
      1 -> "Perfect!"
      2 -> "Fantastic!"
      3 -> "Superb!"
      4 -> "Excellent!"
      5 -> "Great!"
      6 -> "Phew!"
      _ -> "winMessage: no win message for " ++ show i ++ " guesses"
