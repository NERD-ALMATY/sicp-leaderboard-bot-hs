{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module LeaderboardBot.Database
  (firstDbConnection, fetchStats, ppStats, updateStats)
  where

import           LeaderboardBot.Internal

import           Control.Exception

import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T (putStrLn)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow


firstDbConnection :: BotField -> IO ()
firstDbConnection bField = do
  conn <- catch (open "data/board.db") $ \e -> do
    print (e :: IOException)
    throwIO e
  execute_ conn "CREATE TABLE IF NOT EXISTS board \
    \(id INTEGER PRIMARY KEY, username TEXT, \
    \fullname TEXT, repo TEXT, score INTEGER)"
  execute conn "INSERT INTO board \
    \(username, fullname, repo, score) VALUES (?,?,?,?)" bField
  close conn

fetchStats :: IO [BotField]
fetchStats = do
  !conn <- open "data/board.db"
  !r <- query_ conn "SELECT username, fullname, repo, score FROM board \
    \ORDER BY score DESC" :: IO [BotField]
  close conn
  pure r

-- | Pretty printer for BotField
ppStats :: [BotField] -> Text
ppStats = T.concat . map go
  where
    go !(BotField !name_ !flName_ _ !score_) = "*" <> name_ <> "*\t"
      <> flName_ <> "\t*" <> (T.pack $ show score_) <> "*\n"

updateStats :: IO ()
updateStats = do
  !conn <- open "data/board.db"
  !r <- query_ conn "SELECT username, fullname, repo, score FROM board"
    :: IO [BotField]
  mapM_ (updateStats_ conn) r
  close conn

updateStats_ :: Connection -> BotField -> IO ()
updateStats_ !conn (BotField !user_ _ !repo_ !score_) = do
  let !userConf = BotConfig (user_, repo_, "/")
  !num_ <- runLeaderBoardM defaultLB userConf
  case num_ of
    Left err  -> T.putStrLn err
    Right num ->
      if score_ /= num then
        execute conn "UPDATE board SET score = (?) WHERE username = (?)"
          (num, user_)
      else
        pure ()
