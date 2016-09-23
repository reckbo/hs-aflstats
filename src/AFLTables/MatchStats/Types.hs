{-# LANGUAGE DeriveGeneric     #-}
module AFLTables.MatchStats.Types where

import           GHC.Generics
import           Data.Csv (ToField (..), ToNamedRecord, DefaultOrdered)
import           Data.Time                (LocalTime) 
import qualified Data.ByteString.Char8    as B8

data Alignment = Home | Away
  deriving (Show,Eq)

data ScoreType = Goal | Behind | RushedBehind
  deriving Show

type MatchEvent = (Int, Round, Venue, LocalTime, Attendance)
type TeamEvent = (Team, Alignment)
type QuarterEvent = (Int, Time)
type ScoreEvent' = (Alignment, Time, ScoreType, Maybe Player)

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
  toField x =  B8.pack $ show x
instance ToField ScoreType where
  toField x =  B8.pack $ show x
instance ToField Alignment where
  toField x =  B8.pack $ show x
instance ToNamedRecord ScoreEvent
instance DefaultOrdered ScoreEvent

type Round = String
type Venue = String
type Attendance = Int
