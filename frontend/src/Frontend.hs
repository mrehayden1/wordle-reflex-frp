module Frontend where

import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock
import Data.List
import Data.Maybe
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Calendar
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static
import Reflex
import Reflex.Dom.Core hiding (Key)

import Common.Route

import Grid
import Keyboard
import Messages
import Words.Daily

initDay :: Day
initDay = read "2024-07-11"

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend {
  _frontend_head = do
    el "title" $ text "Wordle Reflex"
    elAttr "link"
      ("href" =: $(static "main.css") <> "type" =: "text/css"
        <> "rel" =: "stylesheet")
      blank,
  _frontend_body =
    prerender_ blank $ do
      e <- getPostBuild
      dictionaryE <- return . fmap (S.fromList . fromJust)
        <=< getAndDecode
        $ $(static "dictionary.json") <$ e
      elAttr "div" ("id" =: "app") $ do
        _ <- widgetHold blank (fmap app dictionaryE)
        return ()
  }

app :: MonadWidget t m => Set String -> m ()
app dictionary = do
  el "header" $ text "Wordle"
  -- Get today's word and the dictionary
  day <- liftIO . fmap utctDay $ getCurrentTime
  let dailyWord = dailyWords !! fromIntegral (day `diffDays` initDay)
  do rec let inputDyn' = fmap (fmap reverse)
               . foldDyn (collectInput . getFirst) ""
               . gate (fmap not finished)
               $ keyboardE
         inputDyn <- fmap join . widgetHold inputDyn' . fmap (const inputDyn')
           . updated $ guessesDyn
         let enterE = ffilter ((== KeyEnter) . getFirst) $ keyboardE
             submitE = tag (current inputDyn) enterE
         -- Collect guesses in reverse order of display
         guessesDyn <- foldDyn (:) [] . ffilter (isValidGuess dictionary)
                         $ submitE
         let gradedGuessesDyn = fmap (fmap (gradeGuess dailyWord)) guessesDyn
         let finished = current . fmap (isGameFinished dailyWord) $ guessesDyn
         keyboardE <- el "main" $ do
           grid inputDyn gradedGuessesDyn
           (_, keyboardE') <- runEventWriterT . keyboard $ gradedGuessesDyn
           return keyboardE'
         messages dictionary dailyWord guessesDyn submitE
     return ()

-- NB: Collects input in reverse order to how they are displayed.
collectInput :: Key -> String -> String
collectInput KeyBackspace []                    = []
collectInput KeyBackspace (_:s)                 = s
collectInput (KeyChar c)  s     | length s < 5  = c:s
                                | otherwise     = s
collectInput _            s                     = s

isValidGuess :: Set String -> String -> Bool
isValidGuess dictionary = (&&) <$> (>= 5) . length <*> (`S.member` dictionary)

gradeGuess :: String -> String -> [(Char, Grade)]
gradeGuess dailyWord guess =
  reverse . fst . foldl grade ([], unguessedLetters) $ guessRightOrWrong
 where
  grade :: ([(Char, Grade)], [Char]) -> (Char, Bool) -> ([(Char, Grade)], [Char])
  grade (gs, unguessed) (c, True ) = ((c, GradeRight) : gs, unguessed )
  grade (gs, unguessed) (c, False) =
    if c `elem` unguessed
      then ((c, GradeAlmost) : gs, delete c unguessed)
      else ((c, GradeWrong ) : gs, unguessed         )

  guessRightOrWrong = zipWith (ap (,) . (==)) dailyWord guess

  unguessedLetters =
    let lettersGuessedRight = fmap fst . filter snd $ guessRightOrWrong
    in dailyWord \\ lettersGuessedRight

isGameFinished :: String -> [String] -> Bool
isGameFinished dailyWord gs@(g:_) | length gs >= 6 = True
                                  | g == dailyWord = True
                                  | otherwise      = False
isGameFinished _         []                        = False
