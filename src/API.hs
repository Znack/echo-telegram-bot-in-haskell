{-# LANGUAGE OverloadedStrings #-}

module API
  ( Token
  , getUpdates
  , getMe
  , sendMessage
  ) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Text (Text, pack)
import Network.HTTP.Req
import ResponseTypes

instance MonadHttp IO where
  handleHttpException = throwIO

type Token = String

makeRequest ::
     FromJSON a => Token -> Text -> [(Text, Text)] -> IO (Either String a)
makeRequest token url params = do
  response <- responseBody <$> sendRequest
  return $ parseResult response
  where
    sendRequest =
      req
        GET
        builtUrl
        NoReqBody
        jsonResponse
        queryParams
    builtUrl = https "api.telegram.org" /: pack ("bot" ++ token) /: url
    queryParams = buildRequestParams params
    parseResult response =
      case parseEither parseJSON response of
        Right (TelegramResponse True _ (Just result)) -> Right result
        Right (TelegramResponse False (Just errorMessage) _) ->
          Left errorMessage
        Right (TelegramResponse True errorMessage Nothing) ->
          Left $ "Response was got, but body is empty, error: " ++ show errorMessage
        Left errorMessage -> Left errorMessage

buildRequestParams :: (QueryParam p, Monoid p) => [(Text, Text)] -> p
buildRequestParams [] = mempty
buildRequestParams params = mconcat $ fmap (uncurry (=:)) params

getUpdates :: Token -> Maybe Int -> IO (Either String [TelegramUpdateResponse])
getUpdates token Nothing = getUpdates token (Just 0)
getUpdates token (Just lastUpdate) =
  makeRequest token "getUpdates" [("offset", pack $ show lastUpdate), ("timeout", pack $ show 10)]

getMe :: Token -> IO (Either String User)
getMe token = makeRequest token "getMe" []

sendMessage :: Token -> Int -> Text -> IO (Either String TelegramMessage)
sendMessage token chat message =
  makeRequest token "sendMessage" [("chat_id", pack $ show chat), ("text", pack $ show message)]
  