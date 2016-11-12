module AFLTables
  ( module AFLTables.PlayerEvent
  , module AFLTables.ScoreEvent
  , module AFLTables.URL
  , module AFLTables.Types
  , Event (..)
  , getEventIds
  , writeCSV
  , readCSV
  , readEventsFromHtml
  ) where

import           AFLTables.PlayerEvent
import           AFLTables.ScoreEvent
import           AFLTables.Types
import           AFLTables.URL
import qualified Data.ByteString.Lazy     as BL (readFile, writeFile)
import           Data.Csv                 (DefaultOrdered
                                          ,FromNamedRecord
                                          ,Header
                                          ,ToNamedRecord
                                          ,decodeByName
                                          ,encodeDefaultOrderedByName)
import           Data.Either              (either)
import           Data.List                (isInfixOf)
import           Data.Tree.NTree.TypeDefs
import qualified Data.Vector              as V
import           System.FilePath          (takeBaseName)
import           Text.Printf
import           Text.Regex.Posix         (getAllTextMatches, (=~))
import           Text.XML.HXT.Core

type HTML = String

class (DefaultOrdered e, FromNamedRecord e, ToNamedRecord e) => Event e where
  eventArr :: EventID -> IOSLA (XIOState ()) (NTree XNode) (Either String [e])

instance Event PlayerEvent where
  eventArr = AFLTables.PlayerEvent.playerEventsArr

instance Event ScoreEvent where
  eventArr = AFLTables.ScoreEvent.scoreEventsArr

readEventsFromHtml :: Event e => String -> String -> IO (Either String [e])
readEventsFromHtml eventid htmlfile =
  let preCheckArr = this //> hasText (isInfixOf "This page has been sent off")
  in do
      [events] <- runX $
        constA htmlfile
        >>> readFromDocument [withWarnings no, withParseHTML yes, withRemoveWS yes]
        >>> (ifA
             preCheckArr
             (constA $ Left $ "Score Events page is an invalid AFLTables page: "
              ++ eventid)
             (eventArr eventid))
      return events

writeCSV :: Event e => FilePath -> [e] -> IO ()
writeCSV csv events = BL.writeFile csv (encodeDefaultOrderedByName events)

readCSV :: (Event e) => FilePath -> IO (Either String [e])
readCSV csv = fmap (V.toList . snd) <$> decodeByName <$> BL.readFile csv

getEventIds :: HTML -> [EventID]
getEventIds html
  = map takeBaseName (getAllTextMatches (html =~ pat) :: [String])
  where pat = "[[:digit:]]{4}[[:digit:]]+\\.html"
