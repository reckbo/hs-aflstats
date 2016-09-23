module Main where

import qualified Data.ByteString   as B (writeFile)
import           Data.String.Utils (replace)
import           Development.Shake
import           System.Exit       (ExitCode (ExitFailure), exitFailure,
                                    exitSuccess)
import           System.FilePath   (takeBaseName, (</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)
import           Text.Printf       (printf)
import Text.Regex.Posix (getAllTextMatches, (=~))
import Data.Maybe (fromMaybe)

getYear :: FilePath -> Int
getYear = read . takeBaseName . takeBaseName

insertYear :: Int -> FilePattern -> FilePath
insertYear yr pat = replace "*" (show yr) pat

withYrOf :: FilePath -> FilePattern -> FilePath
withYrOf filepath filePat = insertYear (getYear filepath) filePat

outdir :: [Char]
outdir = "output"

seasonHtml :: FilePattern
seasonHtml = outdir </> "*.season.html"

eventIds = outdir </> "*.events/*.eventids.txt"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build"} $ do

  want $ map (flip insertYear eventIds) [2014, 2015]

  seasonHtml
    %> \out -> do
    let url = printf "http://afltables.com/afl/seas/%d.html" (getYear out)
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
    -- liftIO $ createDirectoryIfMissing False (takeDirectory out)
    writeFile' out (unlines eventids)
