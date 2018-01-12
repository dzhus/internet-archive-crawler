module Main where

import ClassyPrelude

import Data.Aeson
import Data.Aeson.Types
import Text.HTML.DOM
import Text.XML hiding (parseLBS)
import Text.XML.Cursor

import Network.HTTP.Simple
import Network.HTTP.Client.TLS

type Url = String

newtype Months = Months [Int]
  deriving (Generic, Show)

instance FromJSON Months

type Year = String

newtype Sparkline = Sparkline { years :: Map Year Months }
  deriving (Generic, Show)

instance FromJSON Sparkline

newtype YearCaptures = YearCaptures [MonthCaptures]
  deriving (Generic, Show)

instance FromJSON YearCaptures

newtype MonthCaptures = MonthCaptures [WeekCaptures]
  deriving (Generic, Show)

instance FromJSON MonthCaptures

newtype WeekCaptures = WeekCaptures [Maybe DayBlip]
  deriving (Generic, Show)

instance FromJSON WeekCaptures

type ArchiveTimestamp = Int

data DayBlip = DayBlip { ts :: [ArchiveTimestamp] }
             | EmptyBlip
  deriving (Generic, Show)

instance FromJSON DayBlip where
  parseJSON (Object o) =
    if null o
    then pure EmptyBlip
    else DayBlip <$> o .: "ts"
  parseJSON ty = typeMismatch "DayBlip" ty

type Timestamp = String

getCalendars :: MonadIO m => Url -> m [YearCaptures]
getCalendars url = do
  res <- httpJSON $ fromString $ "https://web.archive.org/__wb/sparkline?output=json&collection=web&url=" <> url
  forM (keys $ years $ getResponseBody res) $ \y -> do
    r <- httpJSON $ fromString $
         "https://web.archive.org/__wb/calendarcaptures?selected_year=" <> y <> "&url=" <> url
    return (getResponseBody r :: YearCaptures)

extractTimestamps :: [YearCaptures] -> [ArchiveTimestamp]
extractTimestamps = concat . mapMaybe extractTs . concatMap processYear
  where
    processYear (YearCaptures cs)   = concatMap processMonth cs
    processMonth (MonthCaptures cs) = concatMap processWeek cs
    processWeek (WeekCaptures cs)   = catMaybes cs
    extractTs :: DayBlip -> Maybe [ArchiveTimestamp]
    extractTs (DayBlip ts) = Just ts
    extractTs _            = Nothing

getSnapshot :: MonadIO m => Url -> ArchiveTimestamp -> m LByteString
getSnapshot url ts = do
  res <- httpLBS $ fromString $ "https://web.archive.org/web/" <> show ts <> "/" <> url
  return $ getResponseBody res

extractMyEntries :: LByteString
                 -- ^ RuNIX page snapshot body.
                 -> [(Url, LByteString)]
extractMyEntries res =
  mapMaybe renderCursor results
  where
    root = fromDocument $ parseLBS res
    results =
      root $//
      element "div" >=>
      attributeIs "class" "entrygroup" >=>
      checkElement (\(Element _ as _) ->
                      Just True == (("http://sphinx.net.ru" `isPrefixOf`) <$> lookup "id" as))
    renderCursor :: Cursor -> Maybe (Url, LByteString)
    renderCursor cur =
      case node cur of
        NodeElement el ->
          Just ( unpack $ concat $ attribute "id" cur
               , renderLBS def $ Document (Prologue [] Nothing []) el [])
        _ -> Nothing

main :: IO ()
main = do
  m <- newTlsManager
  -- cs <- getCalendars "http://runix.org"
  -- print $ extractTimestamps cs
  print =<< (extractMyEntries <$> getSnapshot "http://runix.org" 20090602054252)
