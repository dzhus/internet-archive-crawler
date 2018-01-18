module Main where

import ClassyPrelude

import Data.Aeson
import Data.Aeson.Types
import Data.Text (dropWhileEnd, split)
import Text.HTML.DOM
import Text.XML hiding (parseLBS)
import Text.XML.Cursor
import Text.Pandoc

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

data BlogEntry = BlogEntry
  { title   :: Text
  , url     :: Url
  , body    :: LText
  , date    :: Day
  }

extractMyEntries :: Text
                 -- ^ Matching @entrygroup@ id prefix.
                 -> LByteString
                 -- ^ RuNIX page snapshot body.
                 -> [BlogEntry]
extractMyEntries entryIdPrefix res =
  mapMaybe extractEntry results
  where
    root = fromDocument $ parseLBS res
    results =
      root $//
      element "div" >=>
      attributeIs "class" "entrygroup" >=>
      checkElement (\(Element _ as _) ->
                      Just True == ((entryIdPrefix `isPrefixOf`) <$> lookup "id" as))
    renderCursor :: Cursor -> Maybe LText
    renderCursor cur =
      case node cur of
        NodeElement el ->
          Just $ renderText def{rsXMLDeclaration = False, rsPretty = True} $
          Document (Prologue [] Nothing []) el []
        _ -> Nothing
    extractEntry :: Cursor -> Maybe BlogEntry
    extractEntry cur =
      BlogEntry <$>
      headMay (cur $// element "h4" &// content) <*>
      (unpack <$> headMay (attribute "id" cur)) <*>
      (renderCursor =<< elBody) <*>
      (extractDay =<< headMay (cur $//
                                (element "div" >=> attributeIs "class" "entry") &/
                                (element "p" >=> attributeIs "class" "date")))
      where
        elBody = headMay (cur $// (element "div" >=> attributeIs "class" "content"))
    extractDay :: Cursor -> Maybe Day
    extractDay c =
      parseTimeM True defaultTimeLocale "%d.%m.%Y" =<<
      headMay (words $ unpack $ concat $ c $// content)

storeBlogEntry :: BlogEntry -> IO ()
storeBlogEntry BlogEntry{..} =
  void $ runIO $
  readHtml def (toStrict body) >>=
  writeMarkdown def{writerReferenceLinks = True} >>=
  writeFileUtf8 (unpack fname)
  where
    fname = tshow date <> slug <> ".md"
    -- A slug for "http://foo.kek/bar/baz-slug/" is "baz-slug"
    slug =
      maybe "" ("-" <>) $
      lastMay (split slash $ dropWhileEnd slash $ pack url)
    slash = (== '/')

saveRunix m = do
  cs <- getCalendars "http://runix.org"
  forM_ (extractTimestamps cs) $
    \ts -> do
      putStrLn $ "Fetching " <> tshow ts
      es <- extractMyEntries "http://sphinx.net.ru"
        <$> getSnapshot "http://runix.org" ts
      unless (null es) $ forM_ es storeBlogEntry

main :: IO ()
main = do
  m <- newTlsManager
  saveRunix m
