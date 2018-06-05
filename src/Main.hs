{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ClassyPrelude

import Control.Concurrent.Async.Lifted hiding (async)
import Control.Lens hiding (element)
import Control.Retry hiding (recovering)
import Data.Aeson
import Data.Aeson.Types
import Data.List.Split (chunksOf)
import Data.Text (dropWhileEnd, split, splitOn)
import Text.HTML.DOM
import Text.XML hiding (parseLBS)
import Text.XML.Cursor
import Text.Pandoc
import Text.Pandoc.Builder (setMeta)

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

recovering :: (MonadIO m, MonadMask m) => (Int -> m a) -> m a
recovering f = recoverAll (limitRetries 10 <> fullJitterBackoff 1000000) (f . rsIterNumber)

logAttempt n s | n == 0    = putStrLn s
               | otherwise = putStrLn $ "[Attempt: " <> tshow (n + 1) <> "]: " <> s

-- | Get list of Internet Archive captures for a URL.
getCalendars :: (MonadCatch m, MonadIO m, MonadMask m)
             => Url
             -> m [YearCaptures]
getCalendars url = recovering $ \attempt -> do
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

getSnapshot :: (MonadCatch m, MonadIO m, MonadMask m) => Url -> ArchiveTimestamp -> m LByteString
getSnapshot url ts = recovering $ \attempt -> do
  let m = pack url <> " @ " <> tshow ts
  logAttempt attempt $ "Fetching " <> m
  getResponseBody <$>
    httpLBS (fromString $ "https://web.archive.org/web/" <> show ts <> "/" <> url)

newtype ArchivedUrls = ArchivedUrls [(Text, Text, Url, Text, Text, Text, Text)]
  deriving (Generic)

instance FromJSON ArchivedUrls

instance ToJSON ArchivedUrls

-- | Find all URLs with a given prefix in the Internet Archive.
getArchivedUrls :: (MonadCatch m, MonadIO m, MonadMask m)
                => Text
                -- ^ URL prefix.
                -> m [Url]
getArchivedUrls pref = recovering $ \attempt -> do
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

parseTime' = parseTimeM True defaultTimeLocale "%d.%m.%Y"

-- | Extract my entries from RuNIX page snapshot.
extractMyRunixEntries :: Text
                      -- ^ Matching @entrygroup@ id prefix.
                      -> LByteString
                      -- ^ RuNIX page snapshot body.
                      -> [BlogEntry]
extractMyRunixEntries entryIdPrefix res =
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
      parseTime' =<< headMay (words $ unpack $ concat $ c $// content)

extractEntry :: Cursor -> Maybe BlogEntry
extractEntry root =
  BlogEntry <$>
  headMay (root $// element "h1" >=> attributeIs "class" "entry_title" &// content) <*>
  permalinkToUrl (headMay $ concat $ root $// attributeIs "class" "entry_permalink" &| attribute "href") <*>
  (renderCursor =<< headMay (root $// element "div" >=> attributeIs "class" "entry_body")) <*>
  extractDay (concat $ concat $ root $// element "div" >=> attributeIs "class" "entry_date" &| attribute "title")
  where
    permalinkToUrl :: Maybe Text -> Maybe Url
    -- Permalinks on Archived pages have IA prefixes which we drop here
    permalinkToUrl t = unpack <$> (headMay =<< tailMay =<< splitOn "/http://" <$> t)
    extractDay :: Text -> Maybe Day
    extractDay t = headMay $ mapMaybe parseTime' (words $ unpack t)

-- | Extract all blog entries from an IA page snapshot.
--
-- There may be more than one entry on a page.
extractEntries :: LByteString -> [BlogEntry]
extractEntries res =
  catMaybes $ root $// attributeIs "class" "blog_entry" &| extractEntry
  where
    root = fromDocument $ parseLBS res

getAllEntries :: (MonadCatch m, MonadIO m, MonadMask m)
              => Url
              -> m [BlogEntry]
getAllEntries url = do
  ts <- extractTimestamps <$> getCalendars url
  snapshots <- forM ts (getSnapshot url)
  return $ concatMap extractEntries snapshots

storeBlogEntry :: BlogEntry -> IO ()
storeBlogEntry BlogEntry{..} =
  void $ runIO $
  addMeta <$> readHtml def (toStrict body) >>=
  writeMarkdown writeOptions >>=
  writeFileUtf8 (unpack fname)
  where
    addMeta = setMeta "title" (MetaString $ unpack title)
    writeOptions =
      def { writerReferenceLinks = True
          , writerSetextHeaders = False
          , writerExtensions = pandocExtensions
          , writerTemplate = Just "$titleblock$\n\n$body$"
          }
    fname = tshow date <> slug <> ".md"
    -- A slug for "http://foo.kek/bar/baz-slug/" is "baz-slug"
    slug =
      maybe "" ("-" <>) $
      lastMay (split slash $ dropWhileEnd slash $ pack url)
    slash = (== '/')

saveRunix :: IO ()
saveRunix = do
  cs <- getCalendars "http://runix.org"
  void $ concurrentlyPooled 10 (extractTimestamps cs) $
    \ts -> do
      putStrLn $ "Fetching " <> tshow ts
      es <- extractMyRunixEntries "http://sphinx.net.ru"
        <$> getSnapshot "http://runix.org" ts
      unless (null es) $ forM_ es storeBlogEntry

concurrentlyPooled n source action = do
  processors <-
    mapM (async . mapM action) $ chunksOf (length source `div` n) source
  concat <$> mapM wait processors

saveMyEntries :: Text -> IO ()
saveMyEntries pat = do
  urls <- getArchivedUrls pat
  putStrLn $ "Found " <> tshow (length urls) <> " URLs to crawl"
  -- Each URL may have multiple captures, collect them all
  entries <- concurrentlyPooled 10 urls $ \u -> do
    putStrLn $ "Fetching " <> pack u
    getAllEntries u
  mapM_ storeBlogEntry $
    mapMaybe (headMay . sortOn (Down . length . body)) $
    groupBy (\e1 e2 -> url e1 == url e2) $
    concat entries

main :: IO ()
main = do
  saveRunix
  saveMyEntries "http://sphinx.net.ru:80/blog/"
  saveMyEntries "http://dzhus.org:80/blog/"
