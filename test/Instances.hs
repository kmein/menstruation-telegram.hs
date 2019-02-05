{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Instances where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as Text
import Test.QuickCheck

import Menstruation.Response

instance Arbitrary a => Arbitrary (Group a) where
  arbitrary = do
    name <- oneof $ map pure ["Vorspeisen", "Angebote", "Essen", "Salate", "Desserts", "Suppen"]
    items <- arbitrary
    pure Group {..}

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary

instance Arbitrary Meal where
  arbitrary = do
    mealName <- oneof $ map pure ["Schlonze", "Pampe", "Schnitzel", "Spaghetti Bolognese"]
    mealColor <- arbitrary
    mealTags <- arbitrary
    mealPrice <- arbitrary
    mealAllergens <- arbitrary
    pure Meal {..}

instance Arbitrary Color where
  arbitrary = oneof $ map pure [Green .. Red]

instance Arbitrary Tag where
  arbitrary = oneof $ map pure [Climate .. SustainableFishing]

instance Arbitrary Price where
  arbitrary = do
    student <- arbitrary
    employee <- arbitrary
    guest <- arbitrary
    pure Price {..}

instance Arbitrary Allergen where
  arbitrary = do
    allergenNumber <- fromIntegral <$> choose (1 :: Int, 99)
    allergenTag <- oneof [pure Nothing, Just . Text.singleton <$> choose ('a', 'z')]
    pure Allergen {..}

instance Arbitrary Cents where
  arbitrary = fromIntegral <$> choose @Int (1, 1000)
