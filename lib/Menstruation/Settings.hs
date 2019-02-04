{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Menstruation.Settings where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Semigroup (Max(..), Semigroup(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Safe (readMay)
import Text.Regex.TDFA ((=~~))

import Menstruation.Response

data Date
  = Tomorrow
  | Selected Day
  deriving (Show, Eq)

data Filter = Filter
  { maximumPrice :: Maybe (Max Cents)
  , allowedColors :: Maybe (Set Color)
  , allowedTags :: Maybe (Set Tag)
  } deriving (Show, Eq)

instance Semigroup Filter where
  f <> g =
    Filter
      { maximumPrice = maximumPrice f <> maximumPrice g
      , allowedColors = allowedColors f <> allowedColors g
      , allowedTags = allowedTags f <> allowedTags g
      }

instance Monoid Filter where
  mempty = Filter {maximumPrice = mempty, allowedColors = mempty, allowedTags = mempty}
  mappend = (<>)

applyFilter :: Filter -> Response Meal -> Response Meal
applyFilter f =
  mapMaybe $ \group ->
    case NonEmpty.filter matches (items group) of
      [] -> Nothing
      (i:is) -> Just group {items = i :| is}
  where
    (<?>) = maybe True
    matches m =
      (\mp -> ((<= getMax mp) . student) <?> mealPrice m) <?> maximumPrice f &&
      (mealColor m `Set.member`) <?> allowedColors f &&
      (not . Set.null . Set.intersection (mealTags m)) <?> allowedTags f

extractFilter :: Text -> Filter
extractFilter text =
  let p =
        fmap (Max . Cents . truncate . (* 100)) . readMay @Double . replace ',' '.' =<<
        (Text.unpack text =~~ Text.unpack "[0-9]+(,[0-9][0-9]?)?")
      cs = fromEmojis text
      ts = fromEmojis text
   in Filter {allowedColors = unlessEmpty cs, allowedTags = unlessEmpty ts, maximumPrice = p}
  where
    replace x y =
      map
        (\c ->
           if c == x
             then y
             else c)
    unlessEmpty xs =
      if Set.null xs
        then Nothing
        else Just xs
    fromEmojis :: (FromEmoji a, Ord a) => Text -> Set a
    fromEmojis = Set.fromList . mapMaybe fromEmoji . Text.unpack

extractDate :: Text -> Maybe Date
extractDate text
  | "tomorrow" `Text.isInfixOf` text = Just Tomorrow
  | otherwise =
    fmap Selected . parseTimeM True defaultTimeLocale "%-Y-%-m-%-d" =<<
    Text.unpack text =~~ Text.unpack "[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}"
