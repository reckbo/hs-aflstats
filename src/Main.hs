module Main where

import           Development.Shake

import Text.Printf (printf)
import System.FilePath (takeBaseName)
import qualified Data.ByteString as B (writeFile)

getYr :: FilePath -> String
getYr = takeBaseName . takeBaseName

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build", shakeVerbosity=Chatty} $ do

  want ["output/2015.season.html"]

  "output/*.season.html"
    %> \out -> do
    let year = getYr out
        url = printf "http://afltables.com/afl/seas/%s.html" year
    Stdout html  <- command [] "curl" [url]
    writeFile' out html
    cmd $ "tidy -modify " ++ out
