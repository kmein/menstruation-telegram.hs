{-# LANGUAGE DeriveGeneric #-}

module Menstruation.Response
  ( Response
  , Group
  , module Menstruation.Menu
  , module Menstruation.Mensa
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import Menstruation.Mensa
import Menstruation.Menu

type Response a = [Group a]

data Group a = Group
  { name :: Text
  , items :: [a]
  } deriving (Generic)

instance FromJSON a => FromJSON (Group a)
