module AFLTables.URL where

import Text.Printf

seasonURL :: Int -> String
seasonURL year = "http://afltables.com/afl/seas/" ++ (show year) ++ ".html"

eventURL :: Int -> String -> String
eventURL year eventid = printf "http://afltables.com/afl/stats/games/%d/%s.html" year eventid
