{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import           LeaderboardBot.Internal
import           LeaderboardBot.Telegram

import           Telegram.Bot.Simple

-- | Run bot using 'Telegram.Token' from @TG_BOT_TOKEN@ environment.
main :: IO ()
main = do
--  let !conf = BotConfig ("librerush", "sicp", "/")
--  num <- runLeaderBoardM defaultLB conf
--  print num
  getEnvToken "TG_BOT_TOKEN" >>= run
