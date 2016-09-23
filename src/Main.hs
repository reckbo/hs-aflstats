module Main where

import qualified Data.ByteString   as B (writeFile)
import           Development.Shake
import           System.Exit       (exitFailure, exitSuccess, ExitCode (ExitFailure))
import           System.FilePath   (takeBaseName, (</>))
import           Text.Printf       (printf)

getYr :: FilePath -> String
getYr = takeBaseName . takeBaseName

out x = "output" </> x

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build"} $ do

  want $ map out ["2014.season.html"
                 ,"2015.season.html"]

  "output/*.season.html"
    %> \out -> do
    let year = getYr out
        url = printf "http://afltables.com/afl/seas/%s.html" year
    Stdout html  <- command [] "curl" [url]
    writeFile' out html
    (Exit ret, Stderr err) <- cmd $ "tidy -q -modify " ++ out
    case ret of
      (ExitFailure 2) -> error $ "html tidy failed with errors: " ++ err
      _ -> return ()
