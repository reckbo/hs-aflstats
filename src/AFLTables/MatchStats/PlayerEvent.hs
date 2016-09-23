{-# LANGUAGE Arrows #-}
module AFLTables.MatchStats.PlayerEvent
  where

import           Data.Char
import           Text.HandsomeSoup (css)
import           Text.XML.HXT.Core

-- homePlayersAr :: IOSLA (XIOState ()) (NTree XNode) (Round, Venue, LocalTime, Attendance)
homePlayersArr
  = (css "table:nth-child(3)"
  >>> css "tr"
  >>> listA (css "td" //> getText))
  >>. drop 3 . reverse . drop 3 . reverse
  >>. map readStatsLine

data PlayerStats = PlayerStats
  { jumper :: Int
  , player :: String
  , ki,mk,hb,di,gl,bh,ho,k,r,i50,cl,cg,ff,fa,br,cp,up,cm,mi,onePct,bo,ga,pctp :: Maybe Int
  } deriving Show

-- #|Player|KI|MK|H|DI|GL|H|HO|K|R|IF|CL|CG|FF|F|R|CP|UP|CM|MI|1%|O|G|%P
readStatsLine :: [String] -> Either String PlayerStats
readStatsLine (jumper:player:xs) = case map toMaybeInt xs of
  (ki:mk:hb:di:gl:bh:ho:k:r:i50:cl:cg:ff:fa:br:cp:up:cm:mi:onePct:bo:ga:pctp:[]) ->
    Right $ PlayerStats (read . head . words $ jumper) player ki mk hb di gl bh ho k r i50 cl cg ff fa br cp up cm mi onePct bo ga pctp
  _ -> Left $ "Wrong number of elements: " ++ (show . length $ xs)
  where
    toMaybeInt x | and . map isDigit $ x = Just (read x)
                 | otherwise = Nothing
readStatsLine _ = Left "stats line less than 3 elements long"



main = runX $ readDocument [withParseHTML yes, withRemoveWS yes] "123.html" >>> homePlayersArr
