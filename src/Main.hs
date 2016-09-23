module Main where

import qualified Data.ByteString   as B (writeFile)
import           Data.String.Utils (replace)
import           Development.Shake
import           System.Exit       (ExitCode (ExitFailure), exitFailure,
                                    exitSuccess)
import           System.FilePath   (takeBaseName, (</>))
import           Text.Printf       (printf)
import Text.Regex.Posix (getAllTextMatches, (=~))
import Data.Maybe (fromMaybe)

getYear :: FilePath -> String
getYear = takeBaseName . takeBaseName

insertYear :: FilePattern -> String -> FilePath
insertYear = replace "*"

withYrOf :: FilePath -> FilePattern -> FilePath
withYrOf filepath filePat = insertYear filePat (getYear filepath)

outdir :: [Char]
outdir = "output"

seasonHtml :: FilePattern
seasonHtml = outdir </> "*.season.html"

eventIds :: FilePattern
eventIds = outdir </> "*.eventids.txt"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build"} $ do

  want $ map (insertYear eventIds . show ) [2014, 2015]

  seasonHtml
    %> \out -> do
    let url = printf "http://afltables.com/afl/seas/%s.html" (getYear out)
    Stdout html  <- command [] "curl" [url]
    writeFile' out html
    (Exit ret, Stderr err) <- cmd $ "tidy -q -modify " ++ out
    case ret of
      (ExitFailure 2) -> error $ "html tidy failed with errors: " ++ err
      _ -> return ()

  eventIds
    %> \out -> do
    let dep = withYrOf out seasonHtml
    need [dep]
    html <- readFile' dep
    let pat = "[[:digit:]]{4}[[:digit:]]+\\.html"
        eventids = map takeBaseName $ getAllTextMatches (html =~ pat) :: [String]
    writeFile' out (unlines eventids)
