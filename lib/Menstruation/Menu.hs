{-# LANGUAGE RecordWildCards, DeriveGeneric, ViewPatterns,
  GeneralizedNewtypeDeriving, TypeApplications, LambdaCase,
  OverloadedStrings #-}

module Menstruation.Menu where

import Control.Monad (replicateM)
import Data.Aeson
import Data.Char
import Data.Maybe (fromMaybe)
import qualified Data.Scientific as Scientific
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Numeric.Natural
import Test.QuickCheck (Arbitrary(..), choose, oneof)
import Text.Printf

import Menstruation.Internal.Emoji

tagOptions :: Options
tagOptions = defaultOptions {constructorTagModifier = camelTo2 ' ', sumEncoding = UntaggedValue}

class Pretty a where
  pretty :: a -> Text

class FromEmoji a where
  fromEmoji :: Char -> Maybe a

data Allergen = Allergen
  { allergenNumber :: Natural
  , allergenTag :: Maybe Text
  } deriving (Eq, Ord, Show)

instance FromJSON Allergen where
  parseJSON =
    withText "allergen string expected" $ \string ->
      case reads (Text.unpack string) of
        [(number, "")] -> pure Allergen {allergenNumber = number, allergenTag = Nothing}
        [(number, Text.pack -> tag)] ->
          pure Allergen {allergenNumber = number, allergenTag = Just tag}
        _ -> fail "allergen must be a number optionally followed by a string"

instance Pretty Allergen where
  pretty Allergen {..} = Text.pack (show allergenNumber) <> fromMaybe mempty allergenTag

instance Arbitrary Allergen where
  arbitrary = do
    allergenNumber <- fromIntegral <$> choose (1 :: Int, 99)
    allergenTag <- oneof [pure Nothing, Just . Text.singleton <$> choose ('a', 'z')]
    pure Allergen {..}

newtype Cents = Cents
  { unCents :: Natural
  } deriving (Eq, Ord, Show, Num)

instance Arbitrary Cents where
  arbitrary = fromIntegral <$> choose @Int (1, 1000)

instance FromJSON Cents where
  parseJSON =
    withScientific "numbers of cents expected" $ \scientific ->
      if Scientific.isInteger scientific
        then pure $ Cents (truncate scientific)
        else fail "whole number expected"

instance Pretty Cents where
  pretty x =
    let (e, c) = unCents x `divMod` 100
     in Text.pack (printf "%d,%02d €" e c)

data Meal = Meal
  { mealName :: Text
  , mealColor :: Color
  , mealTags :: Set Tag
  , mealPrice :: Maybe Price
  , mealAllergens :: Set Allergen
  } deriving (Eq, Show, Generic)

instance Arbitrary Meal where
  arbitrary = do
    mealName <- oneof $ map pure ["Schlonze", "Pampe", "Schnitzel", "Spaghetti Bolognese"]
    mealColor <- arbitrary
    mealTags <- arbitrary
    mealPrice <- arbitrary
    mealAllergens <- arbitrary
    pure Meal {..}

instance FromJSON Meal where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = map toLower . dropWhile isLower}

instance Pretty Meal where
  pretty m =
    Text.pack $
    printf
      "%s %s _%s_ %s"
      (pretty $ mealColor m)
      (maybe mempty pretty $ mealPrice m)
      (mealName m)
      (Text.concat . Set.toList . Set.map pretty $ mealTags m)

data Color
  = Green
  | Yellow
  | Red
  deriving (Show, Eq, Ord, Enum, Generic)

instance Arbitrary Color where
  arbitrary = oneof $ map pure [Green .. Red]

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
  = Climate
  | Vegan
  | Vegetarian
  | Organic
  | SustainableFishing
  deriving (Show, Eq, Ord, Enum, Generic)

instance Arbitrary Tag where
  arbitrary = oneof $ map pure [Climate .. SustainableFishing]

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
  } deriving (Eq, Show, Generic)

instance Arbitrary Price where
  arbitrary = do
    student <- arbitrary
    employee <- arbitrary
    guest <- arbitrary
    pure Price {..}

instance FromJSON Price

instance Pretty Price where
  pretty = pretty . student
