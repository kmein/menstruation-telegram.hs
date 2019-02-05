{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Menstruation.Pretty where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Printf (printf)

import Menstruation.Internal.Emoji
import Menstruation.Menu

class Pretty a where
  pretty :: a -> Text

instance Pretty Allergen where
  pretty Allergen {..} = Text.pack (show allergenNumber) <> fromMaybe mempty allergenTag

instance Pretty Cents where
  pretty x =
    let (e, c) = unCents x `divMod` 100
     in Text.pack (printf "%d,%02d €" e c)

instance Pretty Meal where
  pretty m =
    Text.pack $
    printf
      "%s %s _%s_ %s"
      (pretty $ mealColor m)
      (maybe mempty pretty $ mealPrice m)
      (mealName m)
      (Text.concat . Set.toList . Set.map pretty $ mealTags m)

instance Pretty Color where
  pretty =
    Text.singleton . \case
      Green -> greenHeart
      Yellow -> yellowHeart
      Red -> redHeart

instance Pretty Tag where
  pretty =
    Text.singleton . \case
      Climate -> globeShowingAmericas
      Organic -> smilingFaceWithHalo
      SustainableFishing -> fish
      Vegan -> seedling
      Vegetarian -> carrot

instance Pretty Price where
  pretty = pretty . student
