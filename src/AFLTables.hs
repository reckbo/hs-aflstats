module AFLTables
  ( module AFLTables.PlayerEvents
  , module AFLTables.ScoreEvents
  , seasonURL
  , eventURL
  , getEventIds
  )

where

import           AFLTables.PlayerEvents
import           AFLTables.ScoreEvents
import           System.FilePath        (takeBaseName)
import           Text.Printf
import           Text.Regex.Posix       (getAllTextMatches, (=~))

type HTML = String

seasonURL :: Int -> String
seasonURL year = "http://afltables.com/afl/seas/" ++ (show year) ++ ".html"

eventURL :: Int -> Int -> String
eventURL year eventid = printf "http://afltables.com/afl/stats/games/%d/%d.html" year eventid

getEventIds :: HTML -> [Int]
getEventIds html
  = map (read . takeBaseName) (getAllTextMatches (html =~ pat) :: [String])
  where pat = "[[:digit:]]{4}[[:digit:]]+\\.html"
