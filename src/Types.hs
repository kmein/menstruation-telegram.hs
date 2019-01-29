{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
module Types where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural
import Text.Printf

import Emoji

class Pretty a where
  pretty :: a -> Text

class FromEmoji a where
  fromEmoji :: Char -> Maybe a

newtype Cents = Cents
  { unCents :: Natural
  } deriving (Show, Num)

instance Pretty Cents where
  pretty x =
    let (e, c) = unCents x `divMod` 100
     in Text.pack (printf "%d,%02d €" e c)

data Meal = Meal
  { name :: Text
  , color :: Color
  , tags :: Set Tag
  , price :: Maybe Price
  , allergens :: Set Text
  } deriving (Show)

instance Pretty Meal where
  pretty m =
    Text.pack $
    printf
      "%s %s _%s_ %s"
      (pretty $ color m)
      (maybe mempty pretty $ price m)
      (name m)
      (Text.concat . Set.toList . Set.map pretty $ tags m)

data Color
  = Green
  | Yellow
  | Red
  deriving (Show, Eq, Ord)

instance FromEmoji Color where
  fromEmoji e
    | e == greenHeart = Just Green
    | e == yellowHeart = Just Yellow
    | e == redHeart = Just Red
    | otherwise = Nothing

instance Pretty Color where
  pretty =
    Text.singleton . \case
      Green -> greenHeart
      Yellow -> yellowHeart
      Red -> redHeart

data Tag
  = Vegetarian
  | Vegan
  | Organic
  | SustainableFishing
  | Climate
  deriving (Show, Eq, Ord)

instance FromEmoji Tag where
  fromEmoji e
    | e == globeShowingAmericas = Just Climate
    | e == smilingFaceWithHalo = Just Organic
    | e == fish = Just SustainableFishing
    | e == seedling = Just Vegan
    | e == carrot = Just Vegetarian
    | otherwise = Nothing

instance Pretty Tag where
  pretty =
    Text.singleton . \case
      Climate -> globeShowingAmericas
      Organic -> smilingFaceWithHalo
      SustainableFishing -> fish
      Vegan -> seedling
      Vegetarian -> carrot

data Price = Price
  { student :: Cents
  , employee :: Cents
  , guest :: Cents
  } deriving (Show)

instance Pretty Price where
  pretty = pretty . student
