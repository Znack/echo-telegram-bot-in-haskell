{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ResponseTypes where

import Control.Monad
import Data.Aeson
import GHC.Generics

data TelegramChat = TelegramChat
  { chatId :: Int
  , chatType :: String
  , chatTitle :: Maybe String
  , chatUsername :: Maybe String
  , chatFirstName :: Maybe String
  , chatLastName :: Maybe String
  } deriving (Show, Generic)

instance FromJSON TelegramChat where
  parseJSON (Object v) =
    TelegramChat <$> v .: "id" <*> v .: "type" <*> v .:? "title" <*>
    v .:? "username" <*>
    v .:? "first_name" <*>
    v .:? "last_name"
  parseJSON _ = mzero

data TelegramMessage = TelegramMessage
  { messageId :: Int
  , messageFrom :: Maybe User
  , messageDate :: Int
  , messageChat :: TelegramChat
  , messageForwardFrom :: Maybe User
  , messageReplyToMessage :: Maybe TelegramMessage
  , messageText :: Maybe String
  , messageNewChatParticipant :: Maybe User
  , messageLeftChatParticipant :: Maybe User
  , messageNewChatTitle :: Maybe String
  , messageGroupChatCreated :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON TelegramMessage where
  parseJSON (Object v) =
    TelegramMessage <$> v .: "message_id" <*> v .:? "from" <*> v .: "date" <*>
    v .: "chat" <*>
    v .:? "forward_from" <*>
    v .:? "reply_to_message" <*>
    v .:? "text" <*>
    v .:? "new_chat_members" <*>
    v .:? "left_chat_member" <*>
    v .:? "new_chat_title" <*>
    v .:? "group_chat_created"
  parseJSON _ = mzero

data TelegramUpdateResponse = TelegramUpdateResponse
  { telegramResponseUpdateId :: Int
  , telegramResponseUpdateMessage :: Maybe TelegramMessage
  , telegramResponseUpdateEditedMessage :: Maybe TelegramMessage
  , telegramResponseUpdateChannelPost :: Maybe TelegramMessage
  , telegramResponseUpdateEditedChannelPost :: Maybe TelegramMessage
  } deriving (Show, Generic)

instance FromJSON TelegramUpdateResponse where
  parseJSON (Object v) =
    TelegramUpdateResponse <$> v .: "update_id" <*> v .:? "message" <*>
    v .:? "edited_message" <*>
    v .:? "channel_post" <*>
    v .:? "edited_channel_post"
  parseJSON _ = mzero

data TelegramResponse a = TelegramResponse
  { telegramResponseOk :: Bool
  , telegramResponseDescription :: Maybe String
  , telegramResponseResult :: Maybe a
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (TelegramResponse a) where
  parseJSON (Object v) =
    TelegramResponse <$> v .: "ok" <*> v .:? "description" <*> v .:? "result"
  parseJSON _ = mzero

data User = User
  { userId :: Int
  , userIsBot :: Bool
  , userFirstName :: String
  , userLastName :: Maybe String
  , userUsername :: Maybe String
  , userLanguageCode :: Maybe String
  } deriving (Show, Generic)

instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .: "id" <*> v .: "is_bot" <*> v .: "first_name" <*>
    v .:? "last_name" <*>
    v .:? "username" <*>
    v .:? "language_code"
  parseJSON _ = mzero
