{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ClassyPrelude

import Control.Lens hiding (element)
import Control.Monad.Catch
import Control.Monad.IO.Unlift
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

getSnapshot :: (MonadCatch m, MonadIO m, MonadMask m)
            => Url
            -> ArchiveTimestamp
            -> m LByteString
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
  , tags    :: [Text]
  }

renderCursor :: Cursor -> Maybe LText
renderCursor cur =
  case node cur of
    NodeElement el ->
      -- Do not prettify output as it ruins newlines inside
      -- <pre><code> blocks
      Just $ renderText def{rsXMLDeclaration = False, rsPretty = False} $
      Document (Prologue [] Nothing []) el []
    _ -> Nothing

parseTime' :: String -> Maybe Day
parseTime' = parseTimeM True defaultTimeLocale "%d.%m.%Y"

-- | Extract my entries from RuNIX page snapshot.
extractMyRunixEntries :: LByteString
                      -- ^ RuNIX page snapshot body.
                      -> [BlogEntry]
extractMyRunixEntries res =
  mapMaybe extractRunixEntry entries
  where
    entryIdPrefix = "http://sphinx.net.ru"
    entries =
      fromDocument (parseLBS res) $//
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
                                (element "p" >=> attributeIs "class" "date"))) <*>
      pure ["RuNIX"]
      where
        elBody = headMay (cur $// (element "div" >=> attributeIs "class" "content"))
    extractDay :: Cursor -> Maybe Day
    extractDay c =
      parseTime' =<< headMay (words $ unpack $ concat $ c $// content)

-- | Extract all blog entries from a blog page snapshot.
--
-- There may be more than one entry on the page.
extractBlogEntries :: LByteString -> [BlogEntry]
extractBlogEntries res =
  catMaybes $ root $// attributeIs "class" "blog_entry" &| extractBlogEntry
  where
    root = fromDocument $ parseLBS res
    extractBlogEntry :: Cursor -> Maybe BlogEntry
    extractBlogEntry cur =
      BlogEntry <$>
      headMay (cur $// element "h1" >=> attributeIs "class" "entry_title" &// content) <*>
      permalinkToUrl (headMay $ concat $ cur $// attributeIs "class" "entry_permalink" &| attribute "href") <*>
      (renderCursor =<< headMay (cur $// element "div" >=> attributeIs "class" "entry_body")) <*>
      extractDay (concat $ concat $ cur $// element "div" >=> attributeIs "class" "entry_date" &| attribute "title") <*>
      (Just $ cur $// attributeIs "rel" "tag" &/ content)
      where
        -- | Permalinks on Archived pages have IA prefixes which we drop here
        permalinkToUrl :: Maybe Text -> Maybe Url
        permalinkToUrl t = unpack <$> (headMay =<< tailMay =<< splitOn "/http://" <$> t)
        extractDay :: Text -> Maybe Day
        extractDay t = headMay $ mapMaybe parseTime' (words $ unpack t)

getAllEntries :: (MonadCatch m, MonadIO m, MonadMask m)
              => Url
              -> m [BlogEntry]
getAllEntries url = do
  ts <- extractTimestamps <$> getCalendars url
  snapshots <- forM ts (getSnapshot url)
  return $ concatMap extractBlogEntries snapshots

storeBlogEntry :: BlogEntry -> IO ()
storeBlogEntry be@BlogEntry{..} =
  void $ runIO $
  addMeta <$> readHtml def (toStrict body) >>=
  writeMarkdown writeOptions >>=
  writeFileUtf8 (unpack fname)
  where
    addMeta = setMeta "title" (MetaString $ unpack title) .
              setMeta "tags" (MetaList $ map (MetaString . unpack) tags)
    writeOptions =
      def { writerReferenceLinks = True
          , writerSetextHeaders = False
          , writerExtensions =
            disableExtension Ext_shortcut_reference_links $
            disableExtension Ext_smart $
            disableExtension Ext_bracketed_spans
            pandocExtensions
          , writerTemplate = Just "$titleblock$\n\n$body$"
          }
    fname = urlSlug be <> ".md"

-- | A slug for an entry with URL @http://foo.kek/bar/baz-slug/@ is
-- @-baz-slug@.
urlSlug :: BlogEntry -> Text
urlSlug BlogEntry{..} =
  tshow date <>
  maybe "" ("-" <>) (lastMay (split slash $ dropWhileEnd slash $ pack url))
  where
    slash = (== '/')

concurrentlyPooled :: MonadUnliftIO m
                   => Int
                   -> [e]
                   -> (e -> m b)
                   -> m [b]
concurrentlyPooled n source action = do
  processors <-
    mapM (async . mapM action) $ chunksOf (length source `div` n) source
  concat <$> mapM wait processors

getRunix :: IO [BlogEntry]
getRunix = do
  cs <- getCalendars "http://runix.org"
  fmap concat $ concurrentlyPooled 10 (extractTimestamps cs) $ \ts -> do
      putStrLn $ "Fetching " <> tshow ts
      extractMyRunixEntries <$> getSnapshot "http://runix.org" ts

getMyEntries :: Text -> IO [BlogEntry]
getMyEntries pat = do
  urls <- getArchivedUrls pat
  putStrLn $ "Found " <> tshow (length urls) <> " URLs to crawl"
  -- Each URL may have multiple captures, collect them all
  fmap concat $ concurrentlyPooled 10 urls $ \u -> do
    putStrLn $ "Fetching " <> pack u
    getAllEntries u

main :: IO ()
main = do
  entries <- sequence [ getRunix
                      , getMyEntries "http://sphinx.net.ru:80/blog/"
                      , getMyEntries "http://dzhus.org:80/blog/"
                      ]
  -- Store the longest variant of each article, identified by URL
  -- slug.
  mapM_ storeBlogEntry $
    mapMaybe (headMay . sortOn (Down . length . body)) $
    groupBy (\e1 e2 -> comparing urlSlug e1 e2 == EQ) $
    sortOn urlSlug $
    concat entries
