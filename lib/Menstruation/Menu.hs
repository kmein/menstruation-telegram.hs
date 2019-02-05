{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Menstruation.Menu
  ( Allergen(..)
  , Cents(..)
  , Meal(..)
  , Color(..)
  , Tag(..)
  , Price(..)
  ) where

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
import Text.Printf

data Allergen = Allergen
  { allergenNumber :: Natural
  , allergenTag :: Maybe Text
  } deriving (Eq, Ord, Show)

newtype Cents = Cents
  { unCents :: Natural
  } deriving (Eq, Ord, Show, Num)

data Meal = Meal
  { mealName :: Text
  , mealColor :: Color
  , mealTags :: Set Tag
  , mealPrice :: Maybe Price
  , mealAllergens :: Set Allergen
  } deriving (Eq, Show, Generic)

data Color
  = Green
  | Yellow
  | Red
  deriving (Show, Eq, Ord, Enum, Generic)

data Tag
  = Climate
  | Vegan
  | Vegetarian
  | Organic
  | SustainableFishing
  deriving (Show, Eq, Ord, Enum, Generic)

data Price = Price
  { student :: Cents
  , employee :: Cents
  , guest :: Cents
  } deriving (Eq, Show, Generic)

tagOptions :: Options
tagOptions = defaultOptions {constructorTagModifier = camelTo2 ' ', sumEncoding = UntaggedValue}

instance FromJSON Allergen where
  parseJSON =
    withText "allergen string expected" $ \string ->
      case reads (Text.unpack string) of
        [(number, "")] -> pure Allergen {allergenNumber = number, allergenTag = Nothing}
        [(number, Text.pack -> tag)] ->
          pure Allergen {allergenNumber = number, allergenTag = Just tag}
        _ -> fail "allergen must be a number optionally followed by a string"

instance FromJSON Cents where
  parseJSON =
    withScientific "numbers of cents expected" $ \scientific ->
      if Scientific.isInteger scientific
        then pure $ Cents (truncate scientific)
        else fail "whole number expected"

instance FromJSON Meal where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = map toLower . dropWhile isLower}

instance FromJSON Color where
  parseJSON = genericParseJSON tagOptions

instance FromJSON Tag where
  parseJSON = genericParseJSON tagOptions

instance FromJSON Price
