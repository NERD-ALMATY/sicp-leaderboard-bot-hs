{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import           LeaderboardBot.Internal
import           LeaderboardBot.Telegram

import           Telegram.Bot.Simple

-- | Run bot using 'Telegram.Token' from @TG_BOT_TOKEN@ environment.
main :: IO ()
main = getEnvToken "TG_BOT_TOKEN" >>= run
