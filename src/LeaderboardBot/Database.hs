{-# LANGUAGE OverloadedStrings #-}

module LeaderboardBot.Database where

import           LeaderboardBot.Internal

import           Data.Text                      (Text)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow


firstDbConnection :: BotField -> IO ()
firstDbConnection bField = do
  conn <- open "data/board.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS board \
    \(id INTEGER PRIMARY KEY, username TEXT, fullname TEXT, score INTEGER)"
  execute conn "INSERT INTO board (username, fullname, score) VALUES (?,?,?)"
    bField
  r <- query_ conn "SELECT * FROM board" :: IO [BotField]
  mapM_ print r
  close conn
