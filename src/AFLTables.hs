{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AFLTables
  ( readScoreEvents
  , readScoreEventsFromFile
  ) where

import           Control.Applicative
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Time                (LocalTime, defaultTimeLocale,
                                           parseTimeM)
import           Data.Tree.NTree.TypeDefs
import           Text.HandsomeSoup
import           Text.XML.HXT.Core
import System.FilePath

data Alignment = Home | Away
  deriving Show

data EventOutcome = Won | Lost | Drawn
  deriving Show

data MatchInfo = MatchInfo
  { round      :: Int
  , venue      :: String
  , date       :: LocalTime
  , attendance :: Int
  } deriving Show

type Player = String
type Team = String
type Time = Int

data ScoreType = Goal | Behind | RushedBehind
  deriving Show

data Teams = Teams
  { home :: Team
  , away :: Team }
  deriving Show

data ScoreEvent' = ScoreEvent'
  { _alignment'  :: Alignment
  , _time'      :: Time
  , _scoreType' :: ScoreType
  , _scorer'    :: Maybe Player
  }
  deriving Show

data Quarter = Quarter Int Time
  deriving Show

data ScoreEvent = ScoreEvent
  { _matchid :: Int
  , _quarter :: Int
  , _quarterTime :: Int
  , _team :: String
  , _alignment  :: Alignment
  , _time      :: Time
  , _scoreType :: ScoreType
  , _scorer    :: Maybe Player
  }
  deriving Show

join :: Int -> Teams -> Quarter -> ScoreEvent' -> ScoreEvent
join matchid (Teams home away) (Quarter qnum qtime) ScoreEvent'{..}
  = ScoreEvent matchid qnum qtime team _alignment' _time' _scoreType' _scorer'
    where team = case _alignment' of
            Home -> home
            Away -> away

trim :: String -> String
trim = unwords . words

readScoreEvent :: [String] -> ScoreEvent'
readScoreEvent xs = case head xs of
  "\160" -> uncurry3 (ScoreEvent' Away) $ fromLine $ reverse xs
  _ -> uncurry3 (ScoreEvent' Home) $ fromLine xs
  where
   fromLine :: [String] -> (Int, ScoreType, Maybe Player)
   fromLine (description:t':_) =
      let t = readTime t'
      in
        if description == "Rushed Behind"
          then (t, RushedBehind, Nothing)
          else case reverse . words $ description of
                "behind":name -> (t, Behind, Just $ unwords name)
                "goal":name -> (t, Goal, Just $ unwords name)
                _ -> error $ "Unexpected goal type in: " ++ description
   fromLine xs = error $ "couldnt parse " ++ (unwords xs)

readMatchInfo :: [String] -> MatchInfo
readMatchInfo [round, venue, date, attendance]
  = MatchInfo (read round) venue (readDate date) (read attendance)

readDate :: String -> LocalTime
readDate t = fromJust $ parseTimeM True defaultTimeLocale fmt t'
  where
    t' = trim $ takeWhile (/='(') t
    trim = unwords . words
    fmt = "%a, %e-%b-%Y %l:%M %p"

matchInfoArr :: IOSLA (XIOState ()) (NTree XNode) MatchInfo
matchInfoArr = (css "table:first-child" >>>
                css "tr:first-child" >>>
                css "td:nth-child(2)" >>>
               removeAllWhiteSpace >>>
                (deep getText))
               >. (readMatchInfo . fmap trim . getElems [1,3,5,7])
  where getElems is xs = map (xs !!) is

scoreLinesArr = (css "table:nth-child(8)"
                  >>> css "tr"
                  >>> listA scoringLineArr)
                  >>. tail . tail . init -- last and first 2 rows
                  where
                    scoringLineArr =  css "td" >>> listA (deep getText >>. unwords)

teamsArr :: IOSLA (XIOState ()) (NTree XNode) Teams
teamsArr = (css "table:nth-child(8)"
             >>> css "tr:nth-child(2)"
             >>> removeAllWhiteSpace
             >>> css "th"
             //> getText) >. (Teams <$> head <*> last)

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

readScoreEvents matchid html = do
  -- let matchid = read $ takeBaseName html
  let    doc = readString [withParseHTML yes, withWarnings no] html
  teams <- fmap head $ runX $ doc >>> teamsArr
  scoreLines <- runX $ doc >>>  scoreLinesArr
  let
    isQuarterLine = and . map ("quarter" `isInfixOf`)
    scoreEvents = (fmap readScoreEvent) <$> wordsBy isQuarterLine scoreLines
    quarters = zipWith Quarter [1..] quarterTimes
                where quarterTimes = map readQuarterTime $ concat . filter isQuarterLine $ scoreLines
  return $ concat $ zipWith (\q es -> map (join matchid teams q) es) quarters scoreEvents

readScoreEventsFromFile html = do
  let matchid = read $ takeBaseName html
  html' <- readFile html
  readScoreEvents matchid html'

-- main :: IO ()
-- main = () <$ getScore
