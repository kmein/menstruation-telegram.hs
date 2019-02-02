{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, LambdaCase
  #-}

module Menstruation.Menu where

import Data.Aeson
import Data.Char
import qualified Data.Scientific as Scientfic
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Numeric.Natural
import Text.Printf

import Emoji

tagOptions :: Options
tagOptions = defaultOptions {constructorTagModifier = camelTo2 ' ', sumEncoding = UntaggedValue}

class Pretty a where
  pretty :: a -> Text

class FromEmoji a where
  fromEmoji :: Char -> Maybe a

newtype Cents = Cents
  { unCents :: Natural
  } deriving (Show, Num)

instance FromJSON Cents where
  parseJSON =
    withScientific "numbers of cents expected" $ \scientific ->
      if Scientfic.isInteger scientific
        then pure $ Cents (truncate scientific)
        else fail "whole number expected"

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
  } deriving (Show, Generic)

instance FromJSON Meal

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
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Color where
  parseJSON = genericParseJSON tagOptions

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
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Tag where
  parseJSON = genericParseJSON tagOptions

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
  } deriving (Show, Generic)

instance FromJSON Price

instance Pretty Price where
  pretty = pretty . student
