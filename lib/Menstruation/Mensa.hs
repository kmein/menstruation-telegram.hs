{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Menstruation.Mensa
  ( Code(..)
  , Mensa(..)
  ) where

import Data.Aeson
import Data.Char
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

newtype Code = Code
  { unCode :: Natural
  } deriving (Eq, Show, Num)

instance FromJSON Code where
  parseJSON =
    withScientific "mensa code expected" $ \scientific ->
      if Scientific.isInteger scientific
        then pure $ Code (truncate scientific)
        else fail "whole number expected"

data Mensa = Mensa
  { mensaCode :: Code
  , mensaName :: Text
  , mensaAddress :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Mensa where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = map toLower . dropWhile isLower}
