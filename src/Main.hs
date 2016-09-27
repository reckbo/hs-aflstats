{-# LANGUAGE Arrows #-}
module Main where

import           AFLTables
import qualified Data.ByteString      as B (writeFile)
import qualified Data.ByteString.Lazy as BL (writeFile)
import           Data.Maybe           (fromMaybe)
import           Data.String.Utils    (replace)
import           Development.Shake
import           System.Exit          (ExitCode (ExitFailure), exitFailure,
                                       exitSuccess)
import           System.FilePath      (splitDirectories, splitDirectories,
                                       takeBaseName, takeDirectory, (<.>),
                                       (</>))
import           Text.Printf          (printf)
import           Text.XML.HXT.Core
import           Data.Csv                 (DefaultOrdered, ToField (..),
                                           ToNamedRecord,
                                           encodeDefaultOrderedByName)

getYearS :: FilePath -> String
getYearS = head . tail . splitDirectories
getYear :: FilePath -> Int
getYear = read . getYearS

-- insertYear :: Int -> FilePattern -> FilePath
-- insertYear yr pat = replace "*" (show yr) pat

-- withYearOf :: FilePath -> FilePattern -> FilePath
-- withYearOf filepath filePat = insertYear (getYear filepath) filePat

outdir = "output"
seasonHtml = outdir </> "*/*.season.html"
scoreEvents = outdir </> "*/scoreEvents.csv"
playerEvents = outdir </> "*/playerEvents.csv"
eventHtml = outdir </> "*/*.event.html"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build"} $ do

  want [outdir </> "2014/playerEvents.csv"]

  playerEvents
    %> \out -> do
    let seasonhtml = outdir </> (getYearS out) </> (getYearS out) <.> "season.html"
    need [seasonhtml]
    eventids <- fmap getEventIds $ readFile' seasonhtml
    let eventHtmls =  [outdir </> (getYearS out) </> (show eid) <.> "event.html"
                      | eid <- eventids]
    need eventHtmls
    events <- liftIO $ fmap concat $ traverse readPlayerEventsFromFile eventHtmls
    case sequenceA events of
      Left msg -> error "empty"
      Right playerEvents -> liftIO $
        BL.writeFile out (encodeDefaultOrderedByName playerEvents)

  seasonHtml %> \out -> do
    let url = seasonURL (getYear out)
    Stdout html  <- cmd $ "curl " ++ url
    writeFile' out html

  eventHtml %> \out -> do
    let eventid = takeBaseName . takeBaseName $ out
        url = eventURL (getYear out) (read eventid)
    Stdout html  <- command [] "curl" [url]
    writeFile' out html
    (Exit ret, Stderr err) <- cmd $ "tidy -q -modify " ++ out
    case ret of
      (ExitFailure 2) -> error $ "html tidy failed with errors: " ++ err
      _ -> return ()


  -- eventIds
  --   %> \out -> do
  --   let year = getYear out
  --       dep = insertYear year seasonHtml
  --   need [dep]
  --   html <- readFile' dep
  --   let pat = "[[:digit:]]{4}[[:digit:]]+\\.html"
  --       eventids = map (read . takeBaseName) $ getAllTextMatches (html =~ pat) :: [String]
  --       urlPat = "http://afltables.com/afl/stats/games/%d/%d.html"
  --       urls = map (\i->printf url year i) eventids
  --       outPat = (takeDirectory out) </> "%d.html"
  --       matchfiles = map (\i->printf outPat i) eventids
  --   traverse_ (command [] "curl -o") urls

  --   writeFile' out (unlines eventids)
