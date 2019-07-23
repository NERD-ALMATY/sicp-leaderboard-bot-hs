{-# LANGUAGE OverloadedStrings #-}

module LeaderboardBot.Database where

import           LeaderboardBot.Internal

import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow


firstDbConnection :: BotField -> IO ()
firstDbConnection bField = do
  conn <- open "data/board.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS board \
    \(id INTEGER PRIMARY KEY, username TEXT, fullname TEXT, score INTEGER)"
  execute conn "INSERT INTO board (username, fullname, score) VALUES (?,?,?)"
    bField
--  r <- query_ conn "SELECT username, fullname, score FROM board\
--    \ ORDER BY score DESC" :: IO [BotField]
--  mapM_ print r
  close conn

fetchStats :: IO [BotField]
fetchStats = do
  conn <- open "data/board.db"
  r <- query_ conn "SELECT username, fullname, score FROM board\
    \ ORDER BY score DESC" :: IO [BotField]
  close conn
  pure r

-- | Prettyprinter for BotField
ppStats :: [BotField] -> Text
ppStats = T.concat . map go
  where
    go (BotField name_ flName_ score_) = "*" <> name_ <> "*\t" <> flName_ <>
      "\t*" <> (T.pack $ show score_) <> "*\n"
