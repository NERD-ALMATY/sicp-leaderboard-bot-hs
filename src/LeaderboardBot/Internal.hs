{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module LeaderboardBot.Internal
  (BotConfig(..), LeaderBoardM(..), runLeaderBoardM, defaultLB, BotField(..)
  , fullUserName, putLogStrLn)
  where

import           Data.Monoid                     ((<>))

import           Control.Monad.Reader
import           Data.Text                       (Text, takeEnd)
import qualified Data.Text                       as T (length, pack)
import qualified Data.Text.IO                    as T (putStr, putStrLn)
import           Data.Time.Clock
import qualified Data.Vector                     as V
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow  ()
import           GitHub.Data.Definitions         (User (..))
import           GitHub.Data.Name                (Name (..))
import qualified GitHub.Endpoints.Repos.Contents as Git
import           GitHub.Endpoints.Users          (userInfoFor)
import           System.Console.ANSI

-- | Type for DB
-- username | first and lastname | repo | score
data BotField = BotField !Text !Text !Text !Int
  deriving Show

instance FromRow BotField where
  fromRow = BotField <$> field <*> field <*> field <*> field

instance ToRow BotField where
  toRow (BotField nick_ name_ repo_ score_) =
    toRow (nick_, name_, repo_, score_)

-- | Logging to stdout
putLogStrLn :: Text -> IO ()
putLogStrLn !txt = do
  !time_ <- getCurrentTime
  setSGR [SetColor Foreground Vivid Green]
  T.putStr $ "| " <> (T.pack $ show time_) <> " | "
  setSGR [SetColor Foreground Vivid Red]
  T.putStr $ txt
  setSGR [SetColor Foreground Vivid Green]
  T.putStrLn $ " ..|"
  setSGR [Reset]

-- |  User / Repo / Path
data BotConfig = BotConfig !(Text, Text, Text)

newtype LeaderBoardM a = LeaderBoardM
  { _runLeaderBoardM :: ReaderT BotConfig IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader BotConfig)

runLeaderBoardM :: LeaderBoardM a -> BotConfig -> IO a
runLeaderBoardM = runReaderT . _runLeaderBoardM

contentsForTemp :: Text -> Text -> Text
                -> IO (Either Git.Error Git.Content)
contentsForTemp !user !repo !path =
  Git.contentsFor (N user) (N repo) path Nothing


getFileName :: Git.ContentFileData -> Text
getFileName = Git.contentName . Git.contentFileInfo

getFileExt :: Git.ContentFileData -> Int
getFileExt !file = if 5 <= T.length (getFileName file) &&
                      (takeEnd 4 (getFileName file) == ".scm" ||
                      takeEnd 4 (getFileName file) == ".rkt")
                  then 1 else 0

-- | Counts a number of files with Scheme or Racket extensions
countExt :: BotConfig -> Git.Content -> IO Int
countExt _ (Git.ContentFile !file)
  = pure $ getFileExt file
countExt urp@(BotConfig (user_, repo_, _)) (Git.ContentDirectory !items) = do
  !items_ <- V.mapM sortItems items
  pure $ V.sum items_
  where
    sortItems :: Git.ContentItem -> IO Int
    sortItems !item = case Git.contentItemType item of
                       Git.ItemFile ->
                         pure $ getFileExt_ $
                           Git.contentName $ Git.contentItemInfo item

                       Git.ItemDir  -> do
                         !possibleRepo <- contentsForTemp
                             user_ repo_
                             (Git.contentPath $ Git.contentItemInfo item)
                         either
                           (\_ -> pure 0) (\r -> countExt urp r) possibleRepo

    getFileExt_ !file_ = if 5 <= T.length file_ &&
                            compareExts file_
                         then 1 :: Int else 0 :: Int
    -- Only Scheme and Racket allowed
    compareExts !txt = takeEnd 4 txt == ".scm" || takeEnd 4 txt == ".rkt"

defaultLB :: LeaderBoardM (Either Text Int)
defaultLB = do
  urp@(BotConfig (user_, repo_, path_)) <- ask
  !possibleRepo <- liftIO $ contentsForTemp user_ repo_ path_

  case possibleRepo of
    Left  !err  -> pure $ Left $! T.pack $! show $! err
    Right !repo -> do
      !num <- liftIO $ countExt urp repo
      pure $ Right num

fullUserName :: Text -> IO (Either Text Text)
fullUserName txt = do
  !name_ <- userInfoFor (N txt)
  case name_ of
    Left !err_  -> pure $ Left $ T.pack $! show err_
    Right !name -> pure $
      maybe (Left "Not found a full username") Right $ userName name
