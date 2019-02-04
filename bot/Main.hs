{-# LANGUAGE OverloadedStrings #-}

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
import System.Environment (lookupEnv)
import System.FilePath
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug
import Telegram.Bot.Simple.UpdateParser

import Client
import Menstruation.Response
import Menstruation.Settings

configurationFile :: IO FilePath
configurationFile = (</> "config.ini") <$> configurationDirectory
  where
    configurationDirectory =
      fromMaybe (fail "Please specify configuration directory in variable MENSTRUATION_DIR.") <$>
      lookupEnv "MENSTRUATION_DIR"

configuration :: IO (Either CPError ConfigParser)
configuration = readfile emptyCP =<< configurationFile

data Action
  = GetHelp
  | GetMenu Filter
            (Maybe Date)
  | SetMensa Text
  | None
  deriving (Show)

bot :: a -> BotApp a Action
bot a =
  BotApp
    {botInitialModel = a, botAction = const . handleUpdate, botHandler = handleAction, botJobs = []}

handleUpdate :: Telegram.Update -> Maybe Action
handleUpdate =
  parseUpdate $
  GetHelp <$ command "help" <|> SetMensa <$> command "mensa" <|> GetHelp <$ command "start" <|>
  menuCommand
  where
    menuCommand = do
      t <- text
      case Text.words t of
        ("/menu":_) -> pure (GetMenu (extractFilter t) (extractDate t))
        _ -> fail "not that command"

handleAction :: Action -> a -> Eff Action a
handleAction action conf =
  case action of
    None -> pure conf
    GetHelp ->
      conf <#
      (None <$ reply (toReplyMessage helpMessage) {replyMessageParseMode = Just Telegram.Markdown})
    _ ->
      conf <#
      (None <$
       reply
         (toReplyMessage (Text.pack (show action))) {replyMessageParseMode = Just Telegram.Markdown})

helpMessage :: Text
helpMessage =
  Text.unlines $
  ["*BEFEHLE*"] <> map explain commandDescription <> [mempty, "*LEBENSMITTELAMPEL*"] <>
  map explain colorDescription <>
  [mempty, "*KENNZEICHEN*"] <>
  map explain tagDescription
  where
    explain (x, y) = "`" <> x <> "` – " <> y
    commandDescription =
      [ ("/menu " <> pretty Vegan <> " 3", "heutige Speiseangebote (vegan bis 3€)")
      , ("/menu tomorrow", "morgige Speiseangebote")
      , ("/menu 2018-10-22", "Speiseangebote für den 22.10.2018")
      , ("/help", "dieser Hilfetext")
      , ("/mensa beuth", "Auswahlmenü für die Mensen der Beuth-Hochschule")
      ]
    colorDescription = [(pretty Green, "grün"), (pretty Yellow, "gelb"), (pretty Red, "rot")]
    tagDescription =
      [ (pretty Vegetarian, "vegetarisch")
      , (pretty Vegan, "vegan")
      , (pretty Organic, "bio")
      , (pretty SustainableFishing, "nachhaltig gefischt")
      , (pretty Climate, "klimafreundlich")
      ]

main :: IO ()
main =
  getEnvToken "MENSTRUATION_TOKEN" >>=
  (Telegram.defaultTelegramClientEnv >=>
   startBot_ (traceBotDefault (conversationBot Telegram.updateChatId (bot ()))))
