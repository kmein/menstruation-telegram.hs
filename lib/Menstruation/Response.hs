{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Menstruation.Response
  ( Response
  , Group(..)
  , prettyResponse
  , module Menstruation.Menu
  , module Menstruation.Mensa
  ) where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics (Generic)

import Menstruation.Mensa
import Menstruation.Menu
import Menstruation.Pretty

type Response a = [Group a]

prettyResponse :: (Pretty a) => Response a -> Text
prettyResponse response = Text.unlines (map pretty response)

data Group a = Group
  { name :: Text
  , items :: NonEmpty a
  } deriving (Show, Eq, Generic, Functor, Foldable)

instance Pretty a => Pretty (Group a) where
  pretty Group {..} = Text.unlines (Text.toUpper name : NonEmpty.toList (fmap pretty items))

instance FromJSON a => FromJSON (Group a)
