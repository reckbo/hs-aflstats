{-# LANGUAGE Arrows        #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module AFLTables.ScoreEvent
  (ScoreEvent (..)
  ,scoreEventsArr)
where

import           Control.Applicative
import           Control.Monad
import           Data.Csv                  (DefaultOrdered, FromField (..),
                                            FromNamedRecord, ToField (..),
                                            ToNamedRecord)
import           Data.List                 (isInfixOf)
import           Data.List.Split           (wordsBy)
import           Data.Maybe                (fromJust)
import           Data.Time                 (LocalTime, defaultTimeLocale,
                                            parseTimeM)
import           Data.Tree.NTree.TypeDefs
import           Text.HandsomeSoup         (css)
import           Text.XML.HXT.Core
import           Text.XML.HXT.XPath.Arrows

import           AFLTables.Types
import qualified Data.ByteString.Char8     as B8
import           Data.Time                 (LocalTime)
import           GHC.Generics

data ScoreType = Goal | Behind | RushedBehind
  deriving Show

type MatchEvent = (EventID, Round, Venue, LocalTime, Attendance)
type TeamEvent = (Team, Alignment)
type QuarterEvent = (Int, Time)
type ScoreEventLine = (Alignment, Time, ScoreType, Maybe Player)

type Time = Int

data ScoreEvent = ScoreEvent
  { _eventid     :: EventID
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
  toField x =  B8.pack $ show x
instance ToField ScoreType where
  toField x =  B8.pack $ show x
instance ToField Alignment where
  toField x =  B8.pack $ show x
instance FromField LocalTime where
  parseField x =  pure . read . B8.unpack $ x
instance FromField ScoreType where
  parseField x = case B8.unpack x of
    "Behind" -> pure Behind
    "RushedBehind" -> pure RushedBehind
    "Goal" -> pure Goal
instance FromField Alignment where
  parseField x =  case B8.unpack x of
    "Home" -> pure Home
    "Away" -> pure Away

instance ToNamedRecord ScoreEvent
instance FromNamedRecord ScoreEvent
instance DefaultOrdered ScoreEvent

type Round = String
type Venue = String
type Attendance = Int

trim :: String -> String
trim = unwords . words

readScoreLine :: [String] -> Either String ScoreEventLine
readScoreLine xs = case head xs of
  "\160" -> readScoreLine' Away $ reverse xs
  _ ->  readScoreLine' Home $ xs
  where
   readScoreLine' align xs@(description:t':_) =
      let t = readTime t'
      in
        if "Rushed" `isInfixOf` description
          then Right (align, t, RushedBehind, Nothing)
          else case reverse . words $ description of
                "behind":name -> Right (align, t, Behind, Just $ unwords name)
                "goal":name -> Right (align, t, Goal, Just $ unwords name)
                _ -> Left $ "Unexpected goal type in: " ++ (unwords xs)
   readScoreLine' _ xs = Left $ "couldnt parse " ++ (unwords xs)

readDate :: String -> LocalTime
readDate t = fromJust $ parseTimeM True defaultTimeLocale fmt t'
  where
    t' = trim $ takeWhile (/='(') t
    fmt = "%a, %e-%b-%Y %l:%M %p"

matchInfoArr :: EventID -> IOSLA (XIOState ()) (NTree XNode) MatchEvent
matchInfoArr eventid = (css "table:first-child" >>>
                        css "tr:first-child" >>>
                        css "td:nth-child(2)" >>>
                        removeAllWhiteSpace >>>
                        (deep getText))
                       >. proc l -> do
                          rnd <- (!! 1) -< l
                          venue <- (!! 3) -< l
                          date <- (!! 5) -< l
                          attendance <- (!! 7) -< l
                          returnA -< (eventid, trim rnd, venue, (readDate date), (read attendance))

scoreLinesArr eventid = (getXPathTrees "//table[tr[th[contains(.,'Scoring progression')]]]"
                  >>> css "tr"
                  >>> listA scoringLineArr)
                  >>. subset
                  where
                    scoringLineArr =  css "td" >>> listA (deep getText >>. unwords)
                    subset (h1:h2:xs) = init xs
                    subset _ = error $ "No valid score table found for: " ++ show eventid

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

joinEvents :: [MatchEvent] ->
              [TeamEvent] ->
              [QuarterEvent] ->
              [ScoreEventLine] ->
              [ScoreEvent]
joinEvents matchEvent teamEvents quarterEvent scoreEvents = do
  (eventid, rnd, venue, date, attendance) <- matchEvent -- expect a one element list
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

scoreEventsArr
  :: EventID -> IOSLA (XIOState ()) (NTree XNode) (Either String [ScoreEvent])
scoreEventsArr eventid = proc html -> do
  matchEvent <- listA (matchInfoArr eventid) -< html
  teamEvents <- teamsArr -< html
  scoreLines' <- listA (scoreLinesArr eventid) -< html
  let
    isQuarterLine = and . map ("quarter" `isInfixOf`)
    scoreEvents' =  sequenceA <$> map readScoreLine <$> wordsBy isQuarterLine scoreLines'
    quarterEvents = map (:[]) $ zip [1::Int ..] $ map readQuarterTime $ concat . filter isQuarterLine $ scoreLines'
    scoreEvents = case sequenceA scoreEvents' of
      Left msg -> (Left msg)
      Right scoreEvents'' -> Right $ concat $ zipWith (joinEvents matchEvent teamEvents) quarterEvents scoreEvents''
  returnA -< scoreEvents
