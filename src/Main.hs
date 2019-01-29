{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad ((>=>), replicateM)
import Control.Monad.Trans (liftIO)
import Data.ConfigFile
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Numeric.Natural
import Safe (readMay)
import System.Environment (lookupEnv)
import System.FilePath
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug
import Telegram.Bot.Simple.UpdateParser
import Text.Regex.TDFA

import Types
import Emoji

apiEndpoint :: IO String
apiEndpoint =
  fromMaybe "http://127.0.0.1:80" <$> lookupEnv "MENSTRUATION_ENDPOINT"

configurationFile :: IO FilePath
configurationFile = (</> "config.ini") <$> configurationDirectory
  where
    configurationDirectory =
      fromMaybe
        (fail
           "Please specify configuration directory in variable MENSTRUATION_DIR.") <$>
      lookupEnv "MENSTRUATION_DIR"

configuration :: IO (Either CPError ConfigParser)
configuration = readfile emptyCP =<< configurationFile

data Action
  = Help
  | Menu Filter
         (Maybe Date)
  | Mensa Text
  | None
  deriving (Show)

data Date
  = Tomorrow
  | Selected Day
  deriving (Show)

data Filter = Filter
  { maximumPrice :: Maybe Cents
  , allowedColors :: Maybe (Set Color)
  , allowedTags :: Maybe (Set Tag)
  }
  deriving (Show)

bot :: a -> BotApp a Action
bot a =
  BotApp
    { botInitialModel = a
    , botAction = const . handleUpdate
    , botHandler = handleAction
    , botJobs = []
    }

handleUpdate :: Telegram.Update -> Maybe Action
handleUpdate =
  parseUpdate $
  Help <$ command "help" <|> Mensa <$> command "mensa" <|>
  Help <$ command "start" <|>
  menuCommand
  where
    menuCommand = do
      t <- text
      case Text.words t of
        ("/menu":_) -> pure (Menu (extractFilter t) (extractDate t))
        _ -> fail "not that command"

handleAction :: Action -> a -> Eff Action a
handleAction action conf =
  case action of
    None -> pure conf
    Help ->
      conf <#
      (None <$
       reply
         (toReplyMessage helpMessage)
           {replyMessageParseMode = Just Telegram.Markdown})
    _ ->
      conf <#
      (None <$
       reply
         (toReplyMessage (Text.pack (show action)))
           {replyMessageParseMode = Just Telegram.Markdown})

helpMessage :: Text
helpMessage =
  Text.unlines $
  ["*BEFEHLE*"] <> map explain commandDescription
  <> [mempty, "*LEBENSMITTELAMPEL*"] <> map explain colorDescription
  <> [mempty, "*KENNZEICHEN*"] <> map explain tagDescription
  where
    explain (x, y) = "`" <> x <> "` – " <> y
    commandDescription =
      [ ( "/menu " <> Text.singleton seedling <> " 3"
        , "heutige Speiseangebote (vegan bis 3€)")
      , ("/menu tomorrow", "morgige Speiseangebote")
      , ("/menu 2018-10-22", "Speiseangebote für den 22.10.2018")
      , ("/help", "dieser Hilfetext")
      , ("/mensa beuth", "Auswahlmenü für die Mensen der Beuth-Hochschule")
      ]
    colorDescription =
      [ (Text.singleton greenHeart, "grün")
      , (Text.singleton yellowHeart, "gelb")
      , (Text.singleton redHeart, "rot")
      ]
    tagDescription =
      [ (Text.singleton carrot, "vegetarisch")
      , (Text.singleton seedling, "vegan")
      , (Text.singleton smilingFaceWithHalo, "bio")
      , (Text.singleton fish, "nachhaltig gefischt")
      , (Text.singleton globeShowingAmericas, "klimafreundlich")
      ]

extractFilter :: Text -> Filter
extractFilter text =
  let p =
        fmap (Cents . truncate . (* 100)) . readMay @Double =<<
        (replace ',' '.' <$>
         Text.unpack text =~~ Text.unpack "[0-9]+(,[0-9][0-9]?)?")
      cs = fromEmojis text
      ts = fromEmojis text
   in Filter
        { allowedColors = unlessEmpty cs
        , allowedTags = unlessEmpty ts
        , maximumPrice = p
        }
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

main :: IO ()
main =
  getEnvToken "MENSTRUATION_TOKEN" >>=
  (Telegram.defaultTelegramClientEnv >=>
   startBot_ (traceBotDefault (conversationBot Telegram.updateChatId (bot ()))))