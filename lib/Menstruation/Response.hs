{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Menstruation.Response
  ( Response
  , Group(..)
  , module Menstruation.Menu
  , module Menstruation.Mensa
  ) where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary(..), oneof)

import Menstruation.Mensa
import Menstruation.Menu

type Response a = [Group a]

data Group a = Group
  { name :: Text
  , items :: NonEmpty a
  } deriving (Show, Eq, Generic, Functor, Foldable)

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Group a) where
  arbitrary = do
    name <- oneof $ map pure ["Vorspeisen", "Angebote", "Essen", "Salate", "Desserts", "Suppen"]
    items <- arbitrary
    pure Group {..}

instance FromJSON a => FromJSON (Group a)
