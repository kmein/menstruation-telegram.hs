{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Control.Monad ((>=>), replicateM)
import Control.Monad.Trans (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
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
import Menstruation.Pretty
import Menstruation.Response
import Menstruation.Settings

type Model = Maybe Code

data Action
  = GetHelp
  | GetMenu Filter
            (Maybe Date)
  | SetMensa Text
  | None
  deriving (Show)

bot :: Model -> BotApp Model Action
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

handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
  case action of
    None -> pure model
    GetHelp ->
      model <#
      (None <$ reply (toReplyMessage helpMessage) {replyMessageParseMode = Just Telegram.Markdown})
    GetMenu f date ->
      model <#
      case model of
        Just code -> do
          response <- liftIO $ getMenu code date
          let menu = applyFilter f response
          reply
            (toReplyMessage (prettyResponse menu)) {replyMessageParseMode = Just Telegram.Markdown}
          pure None
        Nothing -> pure (SetMensa mempty)
    _ ->
      model <#
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
   startBot_ (traceBotDefault (conversationBot Telegram.updateChatId (bot Nothing))))
