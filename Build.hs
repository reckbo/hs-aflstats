{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import qualified AFLTables                 as AFL (EventID, ScoreEvent (..),
                                                   eventURL, seasonURL)
import qualified Data.ByteString           as B (writeFile)
import           Data.Csv                  (DefaultOrdered, ToField (..),
                                            ToNamedRecord,
                                            encodeDefaultOrderedByName)
import           Data.Maybe                (fromMaybe)
import           Data.String.Utils         (replace)
import           Data.Time                 (UTCTime (..), utctDayTime)
import qualified Data.Vector               as V
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Rule
import           GHC.Generics
import           Shake.BuildNode
import           System.Directory          as IO
import           System.Exit               (ExitCode (ExitFailure), exitFailure,
                                            exitSuccess)
import           System.FilePath           (splitDirectories, splitDirectories,
                                            takeBaseName, takeDirectory, (<.>),
                                            (</>))

type Year = Int

data Event = PlayerEvent | ScoreEvent
           deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

data MatchOutput = MatchHtml Year AFL.EventID
                 | Match Event Year AFL.EventID
                 deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

data SeasonOutput = SeasonHtml Year
                 | Season Event Year
                 deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

outdir = "_data"

instance BuildNode MatchOutput where
  path (MatchHtml year eventid) = outdir </> (show year) </> eventid <.> "html"
  path (Match PlayerEvent year eventid) = outdir </> (show year) </> eventid <.> "playerEvent" <.> "csv"
  path (Match ScoreEvent year eventid) = outdir </> (show year) </> eventid <.> "scoreEvent" <.> "csv"

  build out@(MatchHtml year eventid) = Just $ do
    command_ [] "curl" [AFL.eventURL year eventid, "-o", path out]
    (Exit ret, Stderr err) <- cmd $ "tidy -q -modify " ++ (path out)
    case ret of
      (ExitFailure 2) -> error $ "html tidy failed with errors: " ++ err
      _ -> return ()


instance BuildNode SeasonOutput where
  path (SeasonHtml year) = outdir </> (show year) </> "season.html"
  path (Season event year) = case event of
    ScoreEvent -> outdir </> (show year) </> "scoreEvent" <.> "csv"
    PlayerEvent -> outdir </> (show year) </> "playerEvent" <.> "csv"

  build out@(SeasonHtml year) = Just $ do
    command_ [] "curl" [AFL.seasonURL year, "-o", path out]


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do

  -- want [outdir </> "2014/playerEvents.csv"
  --      ,outdir </> "2014/scoreEvents.csv"]
  action $ (apply [SeasonHtml 2014] :: Action [[Double]])

  rule $ (buildNode :: SeasonOutput -> Maybe (Action [Double]))
  rule $ (buildNode :: MatchOutput -> Maybe (Action [Double]))

    -- do
    --   opts <- getShakeOptions
    --   let outfile = eventHtml q
    --   Stdout html  <- command [] "curl" [eventURL year eventid]
    --   liftIO $ createDirectoryIfMissing True $ takeDirectory outfile
    --   writeFile' outfile html
    --   (Exit ret, Stderr err) <- cmd $ "tidy -q -modify " ++ outfile
    --   case ret of
    --     (ExitFailure 2) -> error $ "html tidy failed with errors: " ++ err
    --     _ -> liftIO $ getModTime (eventHtml q)

  -- scoreEvents
  --   %> \out -> do
  --   let eventstxt = outdir </> (getYearS out) </> "eventids.txt"
  --   need [eventstxt]
  --   eventids <- readFileLines eventstxt
  --   let csvs =  [outdir </> (getYearS out) </> eid <.> "scoreEvents.csv"
  --                     | eid <- eventids]
  --   need csvs
  --   allevents <- liftIO $ traverse readCSV csvs :: Action [Either String [ScoreEvent]]
  --   case sequenceA allevents of
  --     Left msg -> error msg
      -- Right rows -> liftIO $ writeCSV out $ concat rows

    -- events <- liftIO $ sequenceA $ zipWith readScoreEventsFromFile eventids eventHtmls
    -- case sequenceA events of
    --   Left msg -> error "empty"
    --   Right events' -> liftIO $ writeCSV out $ concat events'

  -- scoreCsv %> \out -> do
  --   let eventid = takeBaseName . takeBaseName $ out
  --       year = getYear out
  --       eventHtml = outdir </> (show year) </> eventid <.> "event.html"
  --   apply1 $ EventHtmlQ (year, eventid) :: Action Double
  --   events <- liftIO $ readScoreEventsFromFile eventid eventHtml
  --   case events of
  --     Left msg -> error $ msg ++ "\nfrom url: " ++ eventURL year eventid
  --     Right events' -> liftIO $ writeCSV out events'

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

  -- eventIds %> \out -> do
  --   let url = seasonURL (getYear out)
  --   Stdout html  <- cmd $ "curl " ++ url
  --   writeFileLines out (getEventIds html)

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
