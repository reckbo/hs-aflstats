{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Main where

import qualified AFLTables                 as AFL (EventID, PlayerEvent (..),
                                                   ScoreEvent (..), getEventIds,
                                                   matchURL, readCSV,
                                                   readEventsFromHtml,
                                                   seasonURL, writeCSV)
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Config
import           Development.Shake.Rule
import           Shake.BuildNode
import           System.Exit               (ExitCode (ExitFailure))

outdir = "_data"

type Year = Int

data EventType = PlayerEvent | ScoreEvent
           deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

data MatchOutput = MatchHtml Year AFL.EventID
                 | MatchCsv EventType Year AFL.EventID
                 deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

data SeasonOutput = SeasonHtml Year
                  | SeasonCsv EventType Year
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

  build out@(MatchCsv eventtype year eventid) = case eventtype of
    ScoreEvent -> action (AFL.readEventsFromHtml :: AFL.EventID -> FilePath -> IO (Either String [AFL.ScoreEvent]))
    PlayerEvent -> action (AFL.readEventsFromHtml :: AFL.EventID -> FilePath -> IO (Either String [AFL.PlayerEvent]))
    where
      action readEventsFn = Just $ do
        apply1 (MatchHtml year eventid) :: Action [Double]
        events <- liftIO $ readEventsFn eventid (path $ MatchHtml year eventid)
        case events of
          Left msg -> error $ msg ++ "\nfrom url: " ++ AFL.matchURL year eventid
          Right events' -> liftIO $ AFL.writeCSV (path out) events'


instance BuildNode SeasonOutput where
  path (SeasonHtml year) = outdir </> (show year) </> "season.html"
  path (SeasonCsv ScoreEvent year) = outdir </> (show year) </> "scoreEvent" <.> "csv"
  path (SeasonCsv PlayerEvent year) = outdir </> (show year) </> "playerEvent" <.> "csv"

  build out@(SeasonHtml year) = Just $ do
    command_ [] "curl" [AFL.seasonURL year, "-o", path out]

  build out@(SeasonCsv event year) = case event of
    ScoreEvent -> action (AFL.readCSV :: FilePath -> IO (Either String [AFL.ScoreEvent]))
    PlayerEvent -> action (AFL.readCSV :: FilePath -> IO (Either String [AFL.PlayerEvent]))
    where
      action readCsvFn = Just $ do
        apply1 (SeasonHtml year) :: Action [Double]
        seasonHtml <- readFile' (path $ SeasonHtml year)
        let eventids = AFL.getEventIds seasonHtml
        let matchEvents = [MatchCsv event year eventid | eventid <- eventids ]
        apply matchEvents :: Action [[Double]]
        seasonEvents <- liftIO $ traverse (readCsvFn . path) matchEvents
        case sequenceA seasonEvents of
          Left msg -> error msg
          Right rows -> liftIO $ AFL.writeCSV (path out) $ concat rows

makeAllSeasonsCsv event out = case event of
  ScoreEvent -> action (AFL.readCSV :: FilePath -> IO (Either String [AFL.ScoreEvent]))
  PlayerEvent -> action (AFL.readCSV :: FilePath -> IO (Either String [AFL.PlayerEvent]))
  where action (readCsvFn) = do
          Just years <- getConfig "years"
          let csvs = map (SeasonCsv event) (map read $ words years)
          apply csvs :: Action [[Double]]
          allevents <- liftIO $ traverse (readCsvFn . path) csvs
          case sequenceA allevents of
            Left msg -> error msg
            Right rows -> liftIO $ AFL.writeCSV out (concat rows)


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  usingConfigFile "afltables.cfg"

  want [outdir </> "playerEvent.csv"
       ,outdir </> "scoreEvent.csv"]

  rule $ (buildNode :: SeasonOutput -> Maybe (Action [Double]))
  rule $ (buildNode :: MatchOutput -> Maybe (Action [Double]))
  outdir </> "playerEvent.csv" %> \out -> makeAllSeasonsCsv PlayerEvent out
  outdir </> "scoreEvent.csv" %> \out -> makeAllSeasonsCsv ScoreEvent out
