{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ClassyPrelude

import Control.Lens hiding (element)
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

getSnapshot :: (MonadCatch m, MonadIO m) => Url -> ArchiveTimestamp -> m LByteString
getSnapshot url ts = do
  let m = pack url <> " @ " <> tshow ts
  putStrLn $ "Fetching " <> m
  catch
    (getResponseBody <$>
     httpLBS (fromString $ "https://web.archive.org/web/" <> show ts <> "/" <> url)) $
    \(e :: HttpException) -> do
      putStrLn $ "Error while fetching " <> m
      return ""

newtype ArchivedUrls = ArchivedUrls [(Text, Text, Url, Text, Text, Text, Text)]
  deriving (Generic)

instance FromJSON ArchivedUrls

instance ToJSON ArchivedUrls

-- | Find all URLs with a given prefix in the Internet Archive.
getArchivedUrls :: MonadIO m
                => Text
                -- ^ URL prefix.
                -> m [Url]
getArchivedUrls pref = do
  res <- httpJSON $ fromString $
    "https://web.archive.org/cdx/search?matchType=prefix&collapse=urlkey&output=json&url=" <> unpack pref
  let ArchivedUrls us = getResponseBody res
  return $ filter ("http://" `isPrefixOf`) $ map (^. _3) us

data BlogEntry = BlogEntry
  { title   :: Text
  , url     :: Url
  , body    :: LText
  , date    :: Day
  }

renderCursor :: Cursor -> Maybe LText
renderCursor cur =
  case node cur of
    NodeElement el ->
      Just $ renderText def{rsXMLDeclaration = False, rsPretty = True} $
      Document (Prologue [] Nothing []) el []
    _ -> Nothing

-- | Extract my entries from RuNIX page snapshot.
extractMyEntries :: Text
                 -- ^ Matching @entrygroup@ id prefix.
                 -> LByteString
                 -- ^ RuNIX page snapshot body.
                 -> [BlogEntry]
extractMyEntries entryIdPrefix res =
  mapMaybe extractRunixEntry results
  where
    root = fromDocument $ parseLBS res
    results =
      root $//
      element "div" >=>
      attributeIs "class" "entrygroup" >=>
      checkElement (\(Element _ as _) ->
                      Just True == ((entryIdPrefix `isPrefixOf`) <$> lookup "id" as))
    extractRunixEntry :: Cursor -> Maybe BlogEntry
    extractRunixEntry cur =
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

-- | Extract an entry from its IA snapshot.
extractEntry :: Url -> LByteString -> Maybe BlogEntry
extractEntry url res =
  BlogEntry <$>
  headMay (root $// element "h1" >=> attributeIs "class" "entry_title" &// content) <*>
  pure url <*>
  (renderCursor =<< headMay (root $// element "div" >=> attributeIs "class" "entry_body")) <*>
  extractDay (concat $ concat $ root $// element "div" >=> attributeIs "class" "entry_date" &| attribute "title")
  where
    extractDay :: Text -> Maybe Day
    extractDay t =
      headMay $ mapMaybe (parseTimeM True defaultTimeLocale "%d.%m.%Y")
      (words $ unpack t)
    root = fromDocument $ parseLBS res

getLargestEntry :: (MonadCatch m, MonadIO m) => Url -> m (Maybe BlogEntry)
getLargestEntry url = do
  ts <- extractTimestamps <$> getCalendars url
  snapshots <- forM ts (getSnapshot url)
  return $ headMay $ sortBy (comparing (length . body)) $ mapMaybe (extractEntry url) snapshots

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

saveRunix :: IO ()
saveRunix = do
  cs <- getCalendars "http://runix.org"
  forM_ (extractTimestamps cs) $
    \ts -> do
      putStrLn $ "Fetching " <> tshow ts
      es <- extractMyEntries "http://sphinx.net.ru"
        <$> getSnapshot "http://runix.org" ts
      unless (null es) $ forM_ es storeBlogEntry

saveMyEntries :: IO ()
saveMyEntries = do
  urls <- getArchivedUrls "http://sphinx.net.ru:80/blog/entry/"
  forM_ urls $ \u -> do
    putStrLn $ "Fetching " <> pack u
    s <- getLargestEntry (u :: Url)
    case s of
      Just e -> storeBlogEntry e
      Nothing -> putStrLn $ "No entries for " <> pack u

main :: IO ()
main = do
  saveRunix
  saveMyEntries
