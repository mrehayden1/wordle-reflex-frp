module Grid (
  Grade(..),
  gradeCssClass,

  grid
) where

import Control.Monad
import Data.Bifunctor
import Data.Char
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Reflex
import Reflex.Dom.Core

animationDuration :: NominalDiffTime
animationDuration = 1

-- This has to live somewhere accesible, so it might as well be here
data Grade = GradeWrong | GradeAlmost | GradeRight
 deriving (Eq, Ord, Show)

gradeCssClass :: Grade -> T.Text
gradeCssClass = T.pack . drop 5 . fmap toLower . show


grid :: forall m t. MonadWidget t m
  => Dynamic t String
  -> Dynamic t [[(Char, Grade)]]
  -> m ()
grid inputDyn gradedGuessesDyn =
  divClass "grid" $ do
    let numGuessesDyn = fmap length gradedGuessesDyn
    _ <- flip listWithKey (flip row numGuessesDyn)
           . fmap (M.fromList . zip [1..] . pad (Left $ replicate 5 ' ') 6 . reverse)
           . zipDynWith (:) (fmap (Left . pad ' ' 5) inputDyn)
           . fmap (fmap Right)
           $ gradedGuessesDyn
    return ()
 where
  row :: Int -> Dynamic t Int -> Dynamic t (Either String [(Char, Grade)]) -> m ()
  row j numGuessesDyn rowDyn =
    divClass "guess" $ do
      let boxesDyn = fmap (either (flip zip (repeat Nothing)) (fmap (second Just)))
                          rowDyn
      _ <- flip listWithKey (box numGuessesDyn j)
             . fmap (M.fromList . zip [1..])
             $ boxesDyn
      return ()

  box :: Dynamic t Int -> Int -> Int -> Dynamic t (Char, Maybe Grade) -> m ()
  box numGuessesDyn j i charGradeDyn = do
    animClassDyn <- holdDyn ""
          <=< delay ((fromIntegral i - 1) * animationDuration / 2)
          . updated . fmap (\n -> if j <= n then "animate" else "")
          $ numGuessesDyn
    let gradeClassDyn = zipDynWith (\g n -> if j <= n then g else "")
                                   (fmap (gradeClass . snd) charGradeDyn)
                                   numGuessesDyn
        classesDyn = do
          a <- animClassDyn
          g <- gradeClassDyn
          return ["box", a, g]
        attrsDyn = fmap (("class" =:) . T.intercalate " ") classesDyn
    elDynAttr "div" attrsDyn . dynText . fmap (T.pack . pure . fst)
      $ charGradeDyn
   where
    gradeClass :: Maybe Grade -> Text
    gradeClass (Just GradeRight)  = "right"
    gradeClass (Just GradeAlmost) = "almost"
    gradeClass (Just GradeWrong)  = ""
    gradeClass Nothing            = ""

  pad :: a -> Int -> [a] -> [a]
  pad a i = take i . (++ replicate i a)
