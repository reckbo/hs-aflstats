{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Main where

import qualified AFLTables             as AFL
import qualified AFLTables.PlayerEvent
import qualified AFLTables.ScoreEvent
import           BuildNode
import qualified Data.ByteString.Lazy  as BL
import           Data.Csv              (DefaultOrdered, FromNamedRecord, Header,
                                        ToNamedRecord, decodeByName,
                                        encodeDefaultOrderedByName)
import           Data.Proxy
import           Data.Traversable
import           System.Exit           (ExitCode (ExitFailure))

outdir = "_data"

data EventType = PlayerEvent | ScoreEvent
                 deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)


scrapeHtml :: EventType -> AFL.MatchId -> FilePath -> IO (Either String BL.ByteString)
scrapeHtml e = case e of
  PlayerEvent -> scrapeHtmlWithArrow' AFLTables.PlayerEvent.playerEventsArr
  ScoreEvent  -> scrapeHtmlWithArrow' AFLTables.ScoreEvent.scoreEventsArr
  where scrapeHtmlWithArrow' arr id htmlfile =
          fmap encodeDefaultOrderedByName <$> AFL.scrapeHtmlWithArrow arr id htmlfile

data MatchHtml = MatchHtml AFL.Year AFL.EventID
                 deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildNode MatchHtml where
  path (MatchHtml year eventid) = outdir </> (show year) </> eventid <.> "html"

  build out@(MatchHtml year eventid) = Just $ do
    command_ [] "curl" [AFL.matchURL year eventid, "-o", path out]
    (Exit ret, Stderr err) <- cmd $ "tidy -q -modify " ++ (path out)
    case ret of
      (ExitFailure 2) -> error $ "html tidy failed with errors: " ++ err
      _               -> return ()


data MatchCsv = MatchCsv EventType AFL.Year AFL.MatchId
                       deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildNode MatchCsv where
  path (MatchCsv e year matchid) = outdir </> show year  </>  matchid <.> show e <.> "csv"

  build out@(MatchCsv e year matchid) = Just $ do
    let htmlNode = MatchHtml year matchid
    need htmlNode
    res <- liftIO $ scrapeHtml e matchid (path htmlNode)
    case res of
        Left msg  -> error msg
        Right csv -> liftIO $ BL.writeFile (path out) csv


data SeasonHtml = SeasonHtml AFL.Year
                  deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildNode SeasonHtml where
  path (SeasonHtml year) = outdir </> (show year) </> "season.html"
  build out@(SeasonHtml year) = Just $ do
    command_ [] "curl" [AFL.seasonURL year, "-o", path out]


data SeasonCsv = SeasonCsv EventType AFL.Year
                  deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildNode SeasonCsv where
  path (SeasonCsv e year) = outdir </> (show year) </> show e <.> "csv"

  build out@(SeasonCsv e year) = Just $ do
        need $ SeasonHtml year
        seasonHtml <- readFile' (path $ SeasonHtml year)
        let eventids = AFL.extractEventIds seasonHtml
        let nodes = [MatchCsv e year eventid | eventid <- eventids ]
        needs nodes
        combinedCsv <- liftIO $ BL.concat <$> forM nodes (BL.readFile . path)
        liftIO $ BL.writeFile (path out) combinedCsv


data Csv = Csv EventType
                  deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildNode Csv where
  path (Csv e) = outdir </> show e <.> "csv"

  build out@(Csv e) = Just $ do
    Just years <- getConfig "years"
    let nodes = map (SeasonCsv e) (map read $ words years)
    needs nodes
    combinedCsv <- liftIO $ BL.concat <$> forM nodes (BL.readFile . path)
    liftIO $ BL.writeFile (path out) combinedCsv


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  usingConfigFile "years.cfg"

  action $ do
    need (Csv ScoreEvent)
    need (Csv PlayerEvent)

  rule $ (buildNode :: Csv -> Maybe (Action [Double]))
  rule $ (buildNode :: SeasonCsv -> Maybe (Action [Double]))
  rule $ (buildNode :: MatchCsv -> Maybe (Action [Double]))
  rule $ (buildNode :: MatchHtml -> Maybe (Action [Double]))
  rule $ (buildNode :: SeasonHtml -> Maybe (Action [Double]))
