{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections #-}
module AFLTables
  -- ( getScoreEvents
  -- , readScoreEventsFromFile
  -- , readMatchInfoFromFile
  -- )
where

import           Control.Applicative
import Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Time                (LocalTime, defaultTimeLocale,
                                           parseTimeM)
import           Data.Tree.NTree.TypeDefs
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

data MatchInfo = MatchInfo
  { eventid    :: Int
  , round      :: Int
  , venue      :: String
  , date       :: LocalTime
  , attendance :: Int
  } deriving Show

type Player = String
type Team = String
type Time = Int

-- data ScoreEvent' = ScoreEvent'
--   { _alignment'  :: Alignment
--   , _time'      :: Time
--   , _scoreType' :: ScoreType
--   , _scorer'    :: Maybe Player
--   }
--   deriving Show

-- data Quarter = Quarter Int Time
--   deriving Show

data ScoreEvent = ScoreEvent
  { _eventid     :: Int
  , _quarter     :: Int
  , _quarterTime :: Int
  , _team        :: String
  , _alignment   :: Alignment
  , _time        :: Time
  , _scoreType   :: ScoreType
  , _scorer      :: Maybe Player
  }
  deriving Show

-- join :: Int -> Teams -> Quarter -> ScoreEvent' -> ScoreEvent
-- join matchid (Teams home away) (Quarter qnum qtime) ScoreEvent'{..}
--   = ScoreEvent matchid qnum qtime team _alignment' _time' _scoreType' _scorer'
--     where team = case _alignment' of
--             Home -> home
--             Away -> away

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
    trim = unwords . words
    fmt = "%a, %e-%b-%Y %l:%M %p"

matchInfoArr :: IOSLA (XIOState ()) (NTree XNode) [String]
matchInfoArr = (css "table:first-child" >>>
                css "tr:first-child" >>>
                css "td:nth-child(2)" >>>
               removeAllWhiteSpace >>>
                (deep getText))
               >. (fmap trim . getElems [1,3,5,7])
  where getElems is xs = map (xs !!) is

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

getMatchInfo matchid html = do
  let doc = readString [withParseHTML yes, withWarnings no] html
  [round, venue, date, attendance] <- fmap head $ runX $ doc >>> matchInfoArr
  return $ MatchInfo matchid (read round) venue (readDate date) (read attendance)

joinEvents :: Int -> [(Team, Alignment)] -> [(Int, Time)] -> [(Alignment, Time, ScoreType, Maybe Player)] -> [ScoreEvent]
joinEvents eventid teamEvents quarterEvent scoreEvents = do
  (team, teamAlign) <- teamEvents
  (qid, quarterTime) <- quarterEvent -- expect a one element list
  (scoreAlign, scoreTime, scoreType, player) <- scoreEvents
  guard $ teamAlign == scoreAlign
  return ScoreEvent { _eventid = eventid
                    , _quarter = qid
                    , _quarterTime = quarterTime
                    , _team        = team
                    , _alignment   = scoreAlign
                    , _time        = scoreTime
                    , _scoreType   = scoreType
                    , _scorer      = player
                    }

scoreEventsArr matchid = proc html -> do
  teamEvents <- teamsArr -< html
  scoreLines' <- listA scoreLinesArr -< html
  let
    isQuarterLine = and . map ("quarter" `isInfixOf`)
    scoreEvents' =  fmap readScoreLine <$> wordsBy isQuarterLine scoreLines'
    quarterEvents = map (:[]) $ zip [1::Int ..] $ map readQuarterTime $ concat . filter isQuarterLine $ scoreLines'
    scoreEvents = concat $ zipWith (joinEvents matchid teamEvents) quarterEvents scoreEvents'
  returnA -< scoreEvents

readScoreEventsFromFile html = do
  let matchid = read $ takeBaseName html
  fmap concat $ runX $ readDocument [withParseHTML yes, withRemoveWS yes] html
    >>> scoreEventsArr matchid 
