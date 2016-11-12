{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import qualified AFLTables                 as AFL (EventID, PlayerEvent (..),
                                                   ScoreEvent (..), getEventIds,
                                                   matchURL, readCSV,
                                                   readPlayerEventsFromHtml,
                                                   readScoreEventsFromHtml,
                                                   seasonURL, writeCSV)
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
import           Development.Shake.Config
import           Development.Shake.Rule
import           GHC.Generics
import           Shake.BuildNode
import           System.Directory          as IO
import           System.Exit               (ExitCode (ExitFailure), exitFailure,
                                            exitSuccess)
import           System.FilePath           (splitDirectories, splitDirectories,
                                            takeBaseName, takeDirectory, (<.>),
                                            (</>))

outdir = "_data"

type Year = Int

data Event = PlayerEvent | ScoreEvent
           deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

data MatchOutput = MatchHtml Year AFL.EventID
                 | MatchCsv Event Year AFL.EventID
                 deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

data SeasonOutput = SeasonHtml Year
                  | SeasonCsv Event Year
                  deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildNode MatchOutput where
  path (MatchHtml year eventid) = outdir </> (show year) </> eventid <.> "html"
  path (MatchCsv PlayerEvent year eventid) = outdir </> (show year) </> eventid <.> "playerEvent" <.> "csv"
  path (MatchCsv ScoreEvent year eventid) = outdir </> (show year) </> eventid <.> "scoreEvent" <.> "csv"

  build out@(MatchHtml year eventid) = Just $ do
    command_ [] "curl" [AFL.matchURL year eventid, "-o", path out]
    (Exit ret, Stderr err) <- cmd $ "tidy -q -modify " ++ (path out)
    case ret of
      (ExitFailure 2) -> error $ "html tidy failed with errors: " ++ err
      _ -> return ()

  build out@(MatchCsv ScoreEvent year eventid) = Just $ do
     let matchhtml = MatchHtml year eventid
     apply1 matchhtml :: Action [Double]
     events <- liftIO $ AFL.readScoreEventsFromHtml eventid (path matchhtml)
     case events of
       Left msg -> error $ msg ++ "\nfrom url: " ++ AFL.matchURL year eventid
       Right events' -> liftIO $ AFL.writeCSV (path out) events'

  build out@(MatchCsv PlayerEvent year eventid) = Just $ do
     let matchhtml = MatchHtml year eventid
     apply1 matchhtml :: Action [Double]
     events <- liftIO $ AFL.readPlayerEventsFromHtml eventid (path matchhtml)
     case events of
       Left msg -> error $ msg ++ "\nfrom url: " ++ AFL.matchURL year eventid
       Right events' -> liftIO $ AFL.writeCSV (path out) events'


instance BuildNode SeasonOutput where
  path (SeasonHtml year) = outdir </> (show year) </> "season.html"
  path (SeasonCsv event year) = case event of
    ScoreEvent -> outdir </> (show year) </> "scoreEvent" <.> "csv"
    PlayerEvent -> outdir </> (show year) </> "playerEvent" <.> "csv"

  build out@(SeasonHtml year) = Just $ do
    command_ [] "curl" [AFL.seasonURL year, "-o", path out]

  build out@(SeasonCsv ScoreEvent year) = Just $ do
     Stdout html <- cmd $ "curl " ++ (AFL.seasonURL year)
     let eventids = AFL.getEventIds html
     let matchScoreEvents = [MatchCsv ScoreEvent year eventid | eventid <- eventids ]
     apply matchScoreEvents :: Action [[Double]]
     allScoreEvents <- liftIO $ traverse (AFL.readCSV . path) matchScoreEvents  :: Action [Either String [AFL.ScoreEvent]]
     case sequenceA allScoreEvents of
       Left msg -> error msg
       Right rows -> liftIO $ AFL.writeCSV (path out) $ concat rows

  build out@(SeasonCsv PlayerEvent year) = Just $ do
     Stdout html <- cmd $ "curl " ++ (AFL.seasonURL year)
     let eventids = AFL.getEventIds html
     let matchevents = [MatchCsv PlayerEvent year eventid | eventid <- eventids ]
     apply matchevents :: Action [[Double]]
     allevents <- liftIO $ traverse (AFL.readCSV . path) matchevents  :: Action [Either String [AFL.PlayerEvent]]
     case sequenceA allevents of
       Left msg -> error msg
       Right rows -> liftIO $ AFL.writeCSV (path out) $ concat rows


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do

  usingConfigFile "afltables.cfg"

  want [outdir </> "playerEvent.csv"
       ,outdir </> "scoreEvent.csv"]

  rule $ (buildNode :: SeasonOutput -> Maybe (Action [Double]))
  rule $ (buildNode :: MatchOutput -> Maybe (Action [Double]))

  outdir </> "playerEvent.csv" %> \out -> do
    Just years <- getConfig "years"
    let csvs = map (SeasonCsv PlayerEvent) (map read $ words years)
    apply csvs :: Action [[Double]]
    allevents <- liftIO $ traverse (AFL.readCSV . path) csvs  :: Action [Either String [AFL.PlayerEvent]]
    case sequenceA allevents of
      Left msg -> error msg
      Right rows -> liftIO $ AFL.writeCSV out (concat rows)

  outdir </> "scoreEvent.csv" %> \out -> do
    Just years <- getConfig "years"
    let csvs = map (SeasonCsv ScoreEvent) (map read $ words years)
    apply csvs :: Action [[Double]]
    allevents <- liftIO $ traverse (AFL.readCSV . path) csvs  :: Action [Either String [AFL.ScoreEvent]]
    case sequenceA allevents of
      Left msg -> error msg
      Right rows -> liftIO $ AFL.writeCSV out (concat rows)
