module AFLTables.Types where

import qualified Data.ByteString.Char8 as B8
import           Data.Csv              (FromField (..), ToField (..))


type EventID = String

type Team = String

type Player = String

data Alignment = Home | Away
  deriving (Show,Eq)

instance FromField Alignment where
  parseField x =  case B8.unpack x of
    "Home" -> pure Home
    "Away" -> pure Away

instance ToField Alignment where
  toField x =  B8.pack $ show x

