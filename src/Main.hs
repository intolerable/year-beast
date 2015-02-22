module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.DateTime (DateTime)
import Data.Function
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder
import Reddit
import Reddit.Types.Subreddit
import Reddit.Types.Wiki
import Text.Read
import qualified Data.DateTime as DateTime
import qualified Data.Text as Text
import qualified System.Environment as System

subreddit :: SubredditName
subreddit = R "dota2"

main :: IO ()
main = do
  [user, pass] <- fmap Text.pack <$> System.getArgs
  go user pass

go :: Text -> Text -> IO ()
go user pass = every 30 $
  getSightings >>= \case
    Left _ -> putStrLn "couldn't get sightings"
    Right (Sightings (sighting : _)) -> do
      time <- DateTime.getCurrentTime
      let status = getYearBeastStatus time sighting
      void $ runReddit user pass $ do
        updateBanner status "config/sidebar"
        updateBanner status "sidebar"
    Right (Sightings []) -> return ()

data YearBeastStatus = ArrivingIn Integer
                     | LeavingIn Integer
                     | Missing
  deriving (Show, Read, Eq)

getYearBeastStatus :: DateTime -> Sighting -> YearBeastStatus
getYearBeastStatus cur (Sighting _ t d _) =
  case (cur < DateTime.addSeconds d t, cur < t) of
    (True, True) -> ArrivingIn $ DateTime.diffSeconds t cur `div` 60
    (True, False) -> LeavingIn $ DateTime.diffSeconds (DateTime.addSeconds d t) cur `div` 60
    _ -> Missing

foo :: Text -> Bool
foo = Text.isPrefixOf "#### "

updateBanner :: YearBeastStatus -> Text -> Reddit ()
updateBanner status page = do
  p <- getWikiPage subreddit page
  editWikiPage subreddit page (genLink status <> Text.dropWhile (/= '\n') (contentMarkdown p)) ""

genLink :: YearBeastStatus -> Text
genLink (ArrivingIn 0) = "#### [**The Year Beasts will arrive in less than a minute!**](http://2015.yearbeast.com)"
genLink (ArrivingIn 1) = "#### [**The Year Beasts will arrive in 1 minute!**](http://2015.yearbeast.com)"
genLink (ArrivingIn n) = "#### [**The Year Beasts will arrive in " <> tshow n <> " minutes!**](http://2015.yearbeast.com)"
genLink (LeavingIn 0) = "#### [**The Year Beasts will leave in less than a minute!**](http://2015.yearbeast.com)"
genLink (LeavingIn 1) = "#### [**The Year Beasts will leave in 1 minute!**](http://2015.yearbeast.com)"
genLink (LeavingIn n) = "#### [**The Year Beasts will leave in " <> tshow n <> " minutes!**](http://2015.yearbeast.com)"
genLink Missing = ""

tshow :: Show a => a -> Text
tshow = Text.pack . show

data Sighting =
  Sighting { sightingID :: Integer
           , timestamp :: DateTime
           , duration :: Integer
           , comment :: Text }
  deriving (Show, Read, Eq)

instance Ord Sighting where
  compare = compare `on` sightingID

instance FromJSON Sighting where
  parseJSON (Object o) =
    Sighting <$> (o .: "id" >>= readParse)
             <*> (DateTime.fromSeconds <$> (o .: "timestamp" >>= readParse))
             <*> (o .: "duration" >>= readParse)
             <*> o .: "comment"
  parseJSON _ = mempty

newtype Sightings = Sightings [Sighting]
  deriving (Show, Read, Eq)

instance FromJSON Sightings where parseJSON x = Sightings <$> parseJSON x
instance Receivable Sightings where receive = useFromJSON

yearBeast :: Builder
yearBeast = basicBuilder "Year Beast" "http://2015.yearbeast.com"

sightingsRoute :: Route
sightingsRoute =
  Route [ "history.json" ]
        [ ]
        "GET"

getSightings :: IO (Either (APIError ()) Sightings)
getSightings = execAPI yearBeast () $ runRoute sightingsRoute

readParse :: (MonadPlus m, Read a) => String -> m a
readParse x =
  case readMaybe x of
    Just r -> return r
    Nothing -> mzero

every :: MonadIO m => Int -> m a -> m ()
every s x = forever $ do
  _ <- x
  liftIO $ threadDelay $ s * 1000 * 1000
