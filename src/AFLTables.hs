module AFLTables
  ( module AFLTables.PlayerEvents
  , module AFLTables.ScoreEvents
  , getEventIds
  , writeCSV
  , readCSV
  )

where

import           AFLTables.PlayerEvents
import           AFLTables.ScoreEvents
import           AFLTables.Types
import qualified Data.ByteString.Lazy   as BL (readFile, writeFile)
import           Data.Csv               (DefaultOrdered, FromNamedRecord,
                                         Header, ToNamedRecord, decodeByName,
                                         encodeDefaultOrderedByName)
import           Data.Vector            (Vector (..))
import           System.FilePath        (takeBaseName)
import           Text.Printf
import           Text.Regex.Posix       (getAllTextMatches, (=~))
import qualified Data.Vector as V
import Data.Either (either)

type HTML = String

getEventIds :: HTML -> [EventID]
getEventIds html
  = map takeBaseName (getAllTextMatches (html =~ pat) :: [String])
  where pat = "[[:digit:]]{4}[[:digit:]]+\\.html"

writeCSV :: (DefaultOrdered a, ToNamedRecord a) => FilePath -> [a] -> IO ()
writeCSV csv events = BL.writeFile csv (encodeDefaultOrderedByName events)

readCSV :: (FromNamedRecord a) => FilePath -> IO (Either String [a])
readCSV csv = fmap (V.toList . snd) <$> decodeByName <$> BL.readFile csv