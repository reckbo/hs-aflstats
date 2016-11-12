{-# LANGUAGE DeriveGeneric     #-}
module AFLTables.Types where

import           GHC.Generics
import           Data.Csv (ToField (..),
                           FromField (..),
                           ToNamedRecord,
                           FromNamedRecord,
                           DefaultOrdered)
import           Data.Time                (LocalTime)
import qualified Data.ByteString.Char8    as B8

data Alignment = Home | Away
  deriving (Show,Eq)

data ScoreType = Goal | Behind | RushedBehind
  deriving Show

type MatchEvent = (EventID, Round, Venue, LocalTime, Attendance)
type TeamEvent = (Team, Alignment)
type QuarterEvent = (Int, Time)
type ScoreEventLine = (Alignment, Time, ScoreType, Maybe Player)

type Player = String
type Team = String
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

data PlayerEvent = PlayerEvent
  { eventid                                                                   :: EventID
  , team                                                                      :: Team
  , jumper                                                                    :: Int
  , player                                                                    :: String
  , ki,mk,hb,di,gl,bh,ho,k,r,i50,cl,cg,ff,fa,br,cp,up,cm,mi,onePct,bo,ga,pctp :: Maybe Int
  } deriving (Show, Generic)

instance ToNamedRecord PlayerEvent
instance FromNamedRecord PlayerEvent
instance DefaultOrdered PlayerEvent

type Round = String
type Venue = String
type Attendance = Int
type EventID = String
