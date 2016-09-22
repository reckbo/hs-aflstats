{-# LANGUAGE Arrows            #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module AFLTables
  -- ( getScoreEvents
  -- , readScoreEventsFromFile
  -- , readMatchInfoFromFile
  -- )
where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8    as B
import           Data.Csv
import           Data.List
import           Data.List.Split          (wordsBy)
import           Data.Maybe
import           Data.Time                (LocalTime, defaultTimeLocale,
                                           parseTimeM)
import           Data.Tree.NTree.TypeDefs
import           GHC.Generics
import           System.FilePath
import           Text.HandsomeSoup
import           Text.XML.HXT.Core

data Alignment = Home | Away
  deriving (Show,Eq)

data EventOutcome = Won | Lost | Drawn
  deriving Show

data ScoreType = Goal | Behind | RushedBehind
  deriving Show

type TeamEvent = (Team, Alignment)

type Player = String
type Team = String
type Time = Int

data ScoreEvent = ScoreEvent
  { _eventid     :: Int
  , _round       :: String
  , _venue       :: String
  , _date        :: LocalTime
  , _attendance  :: Int
  , _quarter     :: Int
  , _quarterTime :: Int
  , _team        :: String
  , _alignment   :: Alignment
  , _time        :: Time
  , _scoreType   :: ScoreType
  , _scorer      :: Maybe Player
  }
  deriving (Show, Generic)

instance ToField LocalTime where
  toField x =  B.pack $ show x
instance ToField ScoreType where
  toField x =  B.pack $ show x
instance ToField Alignment where
  toField x =  B.pack $ show x
instance ToNamedRecord ScoreEvent
instance DefaultOrdered ScoreEvent

trim :: String -> String
trim = unwords . words

readScoreLine :: [String] -> (Alignment, Time, ScoreType, Maybe Player)
readScoreLine xs = case head xs of
  "\160" -> readScoreLine' Away $ reverse xs
  _ ->  readScoreLine' Home $ xs
  where
   readScoreLine' align (description:t':_) =
      let t = readTime t'
      in
        if description == "Rushed Behind"
          then (align, t, RushedBehind, Nothing)
          else case reverse . words $ description of
                "behind":name -> (align, t, Behind, Just $ unwords name)
                "goal":name -> (align, t, Goal, Just $ unwords name)
                _ -> error $ "Unexpected goal type in: " ++ description
   readScoreLine' _ xs = error $ "couldnt parse " ++ (unwords xs)

readDate :: String -> LocalTime
readDate t = fromJust $ parseTimeM True defaultTimeLocale fmt t'
  where
    t' = trim $ takeWhile (/='(') t
    fmt = "%a, %e-%b-%Y %l:%M %p"

type Round = String
type Venue = String
type Attendance = Int

matchInfoArr :: IOSLA (XIOState ()) (NTree XNode) (Round, Venue, LocalTime, Attendance)
matchInfoArr = (css "table:first-child" >>>
                css "tr:first-child" >>>
                css "td:nth-child(2)" >>>
               removeAllWhiteSpace >>>
                (deep getText))
               >. proc l -> do
                    rnd <- (!! 1) -< l
                    venue <- (!! 3) -< l
                    date <- (!! 5) -< l
                    attendance <- (!! 7) -< l
                    returnA -< (rnd, venue, (readDate date), (read attendance))

scoreLinesArr = (css "table:nth-child(8)"
                  >>> css "tr"
                  >>> listA scoringLineArr)
                  >>. tail . tail . init -- last and first 2 rows
                  where
                    scoringLineArr =  css "td" >>> listA (deep getText >>. unwords)

teamsArr :: IOSLA (XIOState ()) (NTree XNode) [(Team,Alignment)]
teamsArr = (css "table:nth-child(8)"
             >>> css "tr:nth-child(2)"
             >>> removeAllWhiteSpace
             >>> css "th"
             //> getText) >. proc l -> do
                   team1 <- (,Home) . head -< l
                   team2 <- (,Away) . last -< l
                   returnA -< [team1,team2]

readTime :: String -> Time
readTime s = 60*min + sec
  where
    rd = read :: String -> Int
    [min,sec] = rd . init <$> words s

readQuarterTime :: String -> Time
readQuarterTime = readTime . striphead . striptail
  where
    striphead = tail . dropWhile (/='(')
    striptail = reverse. tail . dropWhile (/=')') . reverse

type MatchEvent = (Round, Venue, LocalTime, Attendance)
type QuarterEvent = (Int, Time)

joinEvents :: Int ->
              [MatchEvent] ->
              [TeamEvent] ->
              [QuarterEvent] ->
              [(Alignment, Time, ScoreType, Maybe Player)] ->
              [ScoreEvent]
joinEvents eventid matchEvent teamEvents quarterEvent scoreEvents = do
  (rnd, venue, date, attendance) <- matchEvent -- expect a one element list
  (team, teamAlign) <- teamEvents
  (qid, quarterTime) <- quarterEvent -- expect a one element list
  (scoreAlign, scoreTime, scoreType, player) <- scoreEvents
  guard $ teamAlign == scoreAlign
  return ScoreEvent { _eventid = eventid
                    , _round = rnd
                    , _venue = venue
                    , _date = date
                    , _attendance = attendance
                    , _quarter = qid
                    , _quarterTime = quarterTime
                    , _team        = team
                    , _alignment   = scoreAlign
                    , _time        = scoreTime
                    , _scoreType   = scoreType
                    , _scorer      = player
                    }

scoreEventsArr eventid = proc html -> do
  matchEvent <- listA matchInfoArr -< html
  teamEvents <- teamsArr -< html
  scoreLines' <- listA scoreLinesArr -< html
  let
    isQuarterLine = and . map ("quarter" `isInfixOf`)
    scoreEvents' =  fmap readScoreLine <$> wordsBy isQuarterLine scoreLines'
    quarterEvents = map (:[]) $ zip [1::Int ..] $ map readQuarterTime $ concat . filter isQuarterLine $ scoreLines'
    scoreEvents = concat $ zipWith (joinEvents eventid matchEvent teamEvents) quarterEvents scoreEvents'
  returnA -< scoreEvents

readScoreEventsFromFile html = do
  let matchid = read $ takeBaseName html
  fmap concat $ runX $ readDocument [withParseHTML yes, withRemoveWS yes] html
    >>> scoreEventsArr matchid

html2csv html = do
  events <- readScoreEventsFromFile html
  return $ encodeDefaultOrderedByName events