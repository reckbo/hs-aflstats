{-# LANGUAGE Arrows        #-}
{-# LANGUAGE DeriveGeneric #-}
module AFLTables.PlayerEvent
  (PlayerEvent (..)
  ,playerEventsArr)
  where

import           AFLTables.Types
import           Data.Char
import           Data.Csv                 (DefaultOrdered, FromNamedRecord,
                                           ToField (..), ToNamedRecord)
import           Data.Tree.NTree.TypeDefs
import           GHC.Generics
import           Text.HandsomeSoup        (css)
import           Text.Printf
import           Text.XML.HXT.Core

data PlayerEvent = PlayerEvent
  { eventid :: EventID
  , team    :: Team
  , jumper  :: Int
  , player  :: String
  , ki,mk,hb,di,gl,bh,ho,k,r,i50,
    cl,cg,ff,fa,br,cp,up,cm,mi,
    onePct,bo,ga,pctp :: Maybe Int
  } deriving (Show, Generic)

instance ToNamedRecord PlayerEvent
instance FromNamedRecord PlayerEvent
instance DefaultOrdered PlayerEvent

tableArr align = let x = if align == Home then 3 else 5 :: Int
                 in css (printf "table:nth-child(%d)" x)

teamArr align = tableArr align
                >>> css "thead"
                >>> css "tr:first-child"
                >>> deep getText
                >. head
                >>> arr (unwords . takeWhile (/= "Match") . words)

linesArr :: Alignment -> IOSLA (XIOState ()) (NTree XNode) [String]
linesArr align = (tableArr align
                  >>> css "tr"
                  >>> listA (css "td" //> getText))
                 >>. drop 2 . reverse . drop 3 . reverse

playerEventsArr' eventid align = proc html -> do
  team <- teamArr align -< html
  lines <- linesArr align -< html
  returnA -< (readStatsLine eventid team) lines

playerEventsArr :: EventID -> IOSLA (XIOState ()) (NTree XNode) (Either String [PlayerEvent])
playerEventsArr eventid = proc html -> do
  players <- listA $ playerEventsArr' eventid Home -< html
  players' <- listA $ playerEventsArr' eventid Away -< html
  returnA -< sequenceA (players ++ players')

readStatsLine :: EventID -> Team -> [String] -> Either String PlayerEvent
readStatsLine eventid team (jumper:player:xs) = case map toMaybeInt xs of
  (ki:mk:hb:di:gl:bh:ho:k:r:i50:cl:cg:ff:fa:br:cp:up:cm:mi:onePct:bo:ga:pctp:[]) ->
    Right $ PlayerEvent
    eventid
    team
    (read . head . words $ jumper)
    (filter (/='\n')  player)
    ki mk hb di gl bh ho k r i50 cl cg ff fa br cp up cm mi onePct bo ga pctp
  _ -> Left $ "Wrong number of elements: " ++ (show . length $ xs)
  where
    toMaybeInt x | and . map isDigit $ x = Just (read x)
                 | otherwise = Nothing
readStatsLine _ _ _ = Left "stats line less than 3 elements long"
