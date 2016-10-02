{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           AFLTables
import           AFLTables.Types (ScoreEvent (..), EventID)
import           AFLTables.URL (eventURL, seasonURL)
import qualified Data.ByteString      as B (writeFile)
import           Data.Maybe           (fromMaybe)
import           Data.String.Utils    (replace)
import           Development.Shake
import           System.Exit          (ExitCode (ExitFailure), exitFailure,
                                       exitSuccess)
import           System.FilePath      (splitDirectories, splitDirectories,
                                       takeBaseName, takeDirectory, (<.>),
                                       (</>))
import           Text.Printf          (printf)
import           Text.XML.HXT.Core
import           Data.Csv                 (DefaultOrdered, ToField (..),
                                           ToNamedRecord,
                                           encodeDefaultOrderedByName)
import qualified Data.Vector as V
import Development.Shake.Rule
import Development.Shake.Classes
import System.Directory as IO
import Data.Time (UTCTime (..), utctDayTime)

type Year = Int

newtype EventHtmlQ = EventHtmlQ (Year, EventID)
    deriving (Show,Binary,NFData,Hashable,Typeable,Eq)

getModTime :: FilePath -> IO Double
getModTime = fmap utcToDouble . getModificationTime
  where
    utcToDouble = fromRational . toRational . utctDayTime

eventHtml (EventHtmlQ (year, eventid)) = "_data/events" </> (show year) </> eventid <.> "html"

instance Rule EventHtmlQ Double where
    storedValue _ q = do
        exists <- IO.doesFileExist $ eventHtml q
        if not exists then return Nothing
          else fmap Just $ getModTime $ eventHtml q
    equalValue _ _ old new = if old == new then EqualCheap else NotEqual

outdir = "output"
seasonHtml = outdir </> "*/*.season.html"
eventIds = outdir </> "*/eventids.txt"
scoreEvents = outdir </> "*/scoreEvents.csv"
playerEvents = outdir </> "*/playerEvents.csv"
-- eventHtml = outdir </> "*/*.event.html"
scoreCsv = outdir </> "*/*.scoreEvents.csv"
getYearS :: FilePath -> String
getYearS = head . tail . splitDirectories
getYear :: FilePath -> Int
getYear = read . getYearS

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build", shakeVerbosity=Chatty} $ do

  -- want [outdir </> "2014/playerEvents.csv"
  --      ,outdir </> "2014/scoreEvents.csv"]
  want [outdir </> "2014/scoreEvents.csv"]

  rule $ \q@(EventHtmlQ (year, eventid)) -> Just $ do
      opts <- getShakeOptions
      Stdout html  <- command [] "curl" [eventURL year eventid]
      writeFile' (eventHtml q) html
      (Exit ret, Stderr err) <- cmd $ "tidy -q -modify " ++ (eventHtml q)
      case ret of
        (ExitFailure 2) -> error $ "html tidy failed with errors: " ++ err
        _ -> liftIO $ getModTime (eventHtml q)

  scoreEvents
    %> \out -> do
    let eventstxt = outdir </> (getYearS out) </> "eventids.txt"
    need [eventstxt]
    eventids <- readFileLines eventstxt
    let csvs =  [outdir </> (getYearS out) </> eid <.> "scoreEvents.csv"
                      | eid <- eventids]
    need csvs
    allevents <- liftIO $ traverse readCSV csvs :: Action [Either String [ScoreEvent]]
    case sequenceA allevents of
      Left msg -> error msg
      Right rows -> liftIO $ writeCSV out $ concat rows
    -- events <- liftIO $ sequenceA $ zipWith readScoreEventsFromFile eventids eventHtmls
    -- case sequenceA events of
    --   Left msg -> error "empty"
    --   Right events' -> liftIO $ writeCSV out $ concat events'

  scoreCsv %> \out -> do
    let eventid = takeBaseName . takeBaseName $ out
        year = getYear out
        eventHtml = outdir </> (show year) </> eventid <.> "event.html"
    apply1 $ EventHtmlQ (year, eventid) :: Action Double
    events <- liftIO $ readScoreEventsFromFile eventid eventHtml
    case events of
      Left msg -> error $ msg ++ "\nfrom url: " ++ eventURL year eventid
      Right events' -> liftIO $ writeCSV out events'

  -- playerEvents %> \out -> do
  --   let eventstxt = outdir </> (getYearS out) </> "eventids.txt"
  --   need [eventstxt]
  --   eventids <- readFileLines eventstxt
  --   let eventHtmls =  [outdir </> (getYearS out) </> (show eid) <.> "event.html"
  --                     | eid <- eventids]
  --   need eventHtmls
  --   events <- liftIO $ fmap concat $ sequenceA $ zipWith readPlayerEventsFromFile eventids eventHtmls
  --   case sequenceA events of
  --     Left msg -> error "empty"
  --     Right playerEvents -> liftIO $ writeCSV out playerEvents

  eventIds %> \out -> do
    let url = seasonURL (getYear out)
    Stdout html  <- cmd $ "curl " ++ url
    writeFileLines out (getEventIds html)

  -- eventIds
  --   %> \out -> do
  --   let year = getYear out
  --       dep = insertYear year seasonHtml
  --   need [dep]
  --   html <- readFile' dep
  --   let pat = "[[:digit:]]{4}[[:digit:]]+\\.html"
  --       eventids = map (read . takeBaseName) $ getAllTextMatches (html =~ pat) :: [String]
  --       urlPat = "http://afltables.com/afl/stats/games/%d/%d.html"
  --       urls = map (\i->printf url year i) eventids
  --       outPat = (takeDirectory out) </> "%d.html"
  --       matchfiles = map (\i->printf outPat i) eventids
  --   traverse_ (command [] "curl -o") urls

  --   writeFile' out (unlines eventids)
