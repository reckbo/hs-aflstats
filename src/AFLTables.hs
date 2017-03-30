{-# LANGUAGE ConstraintKinds            #-}
module AFLTables
  ( module AFLTables.URL
  , module AFLTables.Types
  , scrapeHtmlWithArrow
  , extractEventIds
  ) where

import           AFLTables.Types
import           AFLTables.URL
import           Data.List                (isInfixOf)
import           Data.Tree.NTree.TypeDefs
import           System.FilePath          (takeBaseName)
import           Text.Regex.Posix         (getAllTextMatches, (=~))
import           Text.XML.HXT.Core

scrapeHtmlWithArrow eventArr eventid htmlfile =
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


extractEventIds :: HTML -> [MatchId]
extractEventIds html
  = map takeBaseName (getAllTextMatches (html =~ pat) :: [String])
  where pat = "[[:digit:]]{4}[[:digit:]]+\\.html"
