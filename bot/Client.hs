{-# LANGUAGE OverloadedStrings #-}

module Client
  ( getMenu
  , getMensas
  ) where

import Data.Maybe (fromMaybe)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)
import System.Environment (lookupEnv)

import Menstruation.Response

apiEndpoint :: IO String
apiEndpoint = fromMaybe "http://127.0.0.1:80" <$> lookupEnv "MENSTRUATION_ENDPOINT"

getMenu :: Code -> Day -> IO (Response Meal)
getMenu code day = do
  endpoint <- apiEndpoint
  getResponseBody <$>
    (httpJSON =<<
     parseRequest
       (endpoint <> "/menu/" <> show (unCode code) <> formatTime defaultTimeLocale "%Y-%m-%d" day))

getMensas :: IO (Response Mensa)
getMensas = do
  endpoint <- apiEndpoint
  getResponseBody <$> (httpJSON =<< parseRequest (endpoint <> "/codes"))
