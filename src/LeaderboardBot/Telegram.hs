{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module LeaderboardBot.Telegram where

import           LeaderboardBot.Database
import           LeaderboardBot.Internal

import           Control.Applicative              ((<|>))
import           Data.Monoid                      ((<>))

import           Control.Monad.Reader             (liftIO)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.API.Methods         (ParseMode (..))
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

data TodoItem
  = AddItem
  | StatsItem
  | EmptyItem
  deriving (Show, Eq)

-- | Bot conversation state model
data Model
  = Model !TodoItem
  deriving (Show, Eq)

-- | Actions bot can perform
data Action
  = NoAction
  | Start
  | AddRepo
  | UpdateRepo
  | Stats
  | Reset
  | UserUpdate !Text
  deriving Show

-- | Bot application
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = initModel
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

initModel :: Model
initModel = Model EmptyItem

addModel :: Model
addModel = Model AddItem

statsModel :: Model
statsModel = Model StatsItem

replyWithMarkdown :: Text -> BotM ()
replyWithMarkdown !txt =
  reply $! ReplyMessage txt (Just Markdown) Nothing Nothing Nothing Nothing

startMessage :: Text
startMessage = Text.unlines
  [ " ``` This is a SICP LeaderBoard Bot ``` "
  , ""
  , "Use /add to add your repo"
  , ""
  , "Use /stats to get stats"
  , ""
  , "Use /update to update stats"
  ]

-- | Process incoming Updates and turn them into Actions
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate
  $ Start <$ command "start"
  <|> AddRepo <$ command "add"
  <|> Stats <$ (command "stats" <|> command "show_board")
  <|> UpdateRepo <$ (command "update")
  <|> UserUpdate <$> text

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction -> pure model
  Reset    -> statsModel <# pure NoAction
  Start    -> initModel <# do
    replyWithMarkdown startMessage
    pure NoAction
  AddRepo  -> addModel <# do
    replyWithMarkdown "Which repository you want to add?\n\
      \Please send in format\n`username repository_name`"
    pure NoAction
  UserUpdate txt -> model <# do

    if model == addModel then do
      let nameAndRepo_ = case Text.words txt of
                           (x:xs:[]) -> Just (x, xs)
                           _         -> Nothing
      case nameAndRepo_ of
        Just (name_, repo_) -> do
          let !userConf = BotConfig (name_, repo_, "/")
          !num_ <- liftIO $ runLeaderBoardM defaultLB userConf
          case num_ of
            Left err_ -> do
              replyWithMarkdown $ "*Error*\n" <> err_
              liftIO $ putLogStrLn err_
            Right num -> do
              !fullName_ <- liftIO $ fullUserName name_
              case fullName_ of
                Left fname_  -> replyText fname_
                Right fname_ -> liftIO (do
                    firstDbConnection (BotField name_ fname_ repo_ num)
                    putLogStrLn $
                      "Added '" <> name_ <> "/" <> repo_ <> "'")
                  >> replyWithMarkdown "*Ok.*"

        _                   -> replyWithMarkdown "*Incorrect input*"

    else
      liftIO $ putLogStrLn $! "Nothing added -> " <> txt

    pure Reset
  Stats      -> model <#
    if model == statsModel then do
      !stats <- liftIO $ fetchStats
      replyWithMarkdown $ ppStats $ stats
      pure Reset
    else do
      replyWithMarkdown "*There is no added repos.*"
      pure NoAction
  UpdateRepo -> model <#
    if model == statsModel then do
      liftIO $ updateStats
      pure Stats
    else do
      replyWithMarkdown "*There is no added repos.*"
      pure NoAction

run :: Telegram.Token -> IO ()
run !token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env
