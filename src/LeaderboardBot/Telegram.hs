{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module LeaderboardBot.Telegram where

import           LeaderboardBot.Internal

import           Control.Applicative              ((<|>))
import           Data.Monoid                      ((<>))

import           Control.Monad.Reader             (liftIO)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

data TodoItem
  = AddItem
  | ShowItem
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
  | ShowBoard
  | Reset
  | Reg
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

startMessage :: Text
startMessage = Text.unlines
  [ "This is a LeaderBoard Bot"
  , ""
  , "Use /reg to sign up"
  , ""
  ]

-- | Process incoming Updates and turn them into Actions
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate
  $ Start <$ command "start"
  <|> AddRepo <$ command "add"
  <|> ShowBoard <$ (command "show" <|> command "show_board")
  <|> UpdateRepo <$ (command "update")
  <|> Reg <$ (command "reg")
  <|> UserUpdate <$> text

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction -> pure model
  Reset    -> initModel <# pure NoAction
  Start    -> initModel <# do
    replyText startMessage
    pure NoAction
  AddRepo  -> addModel <# do
    replyText "Which repository you want to add?\n\
      \Please give in format \"username repository_name\""
    pure NoAction
  UserUpdate txt -> model <# do
    if model == addModel then do
      -- replyText (txt <> " !!")
      let nameAndRepo_ = case Text.words txt of
                           (x:xs:[]) -> Just (x, xs)
                           _         -> Nothing
      case nameAndRepo_ of
        Just (name_, repo_) -> do
          let !userConf = BotConfig (name_, repo_, "/")
          num <- liftIO $ runLeaderBoardM defaultLB userConf
          replyText $ Text.pack $ show num
        _                   -> replyText "Incorrect input"
    else
      replyText "Nothing added | UserUpdate"
    pure Reset
  Reg  -> addModel <# pure NoAction
  _    -> initModel <# pure NoAction

run :: Telegram.Token -> IO ()
run !token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env