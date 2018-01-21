module Server where

import API
import Data.Maybe
import Data.Text (pack)
import ResponseTypes


startServer :: Token -> IO ()
startServer token = do
  bot <- getMe token
  putStrLn $ "-> Running bot " ++ show bot
  mainLoop Nothing
  where
    mainLoop :: Maybe Int -> IO a
    mainLoop updateId = do
      updates <- getUpdates token updateId
      handleUpdates updates
      nextUpdateId <- getLastUpdateId updates
      mainLoop nextUpdateId
    handleUpdates (Right updates) = mapM_ (handleUpdate token) updates
    handleUpdates (Left errorMessage) =
      putStrLn $ "-> Bot got the error while updating: " ++ show errorMessage

getLastUpdateId :: Either String [TelegramUpdateResponse] -> IO (Maybe Int)
getLastUpdateId updates =
  case updates of
    Left errorMessage -> do
      putStrLn $ "-> ! Error while getting updates: " ++ show errorMessage
      return Nothing
    Right [] -> do
      putStrLn "-> Empty updates."
      return Nothing
    Right xs -> return $ Just $ (+ 1) $ telegramResponseUpdateId $ last xs

handleUpdate :: Token -> TelegramUpdateResponse -> IO ()
handleUpdate token (TelegramUpdateResponse _ (Just message) _ _ _) = do
  putStrLn $ "-> Bot got the message: " ++ show message
  sendMessage token (chatId $ messageChat message) (pack responseText)
  putStrLn $ "-> Bot has responded with message " ++ show responseText
  where
    responseText = fromMaybe "Hey there!" (messageText message)
handleUpdate _ update =
  putStrLn $ "-> Bot got the unrecognized update: " ++ show update
