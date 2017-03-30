{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module BuildNode
  (module Development.Shake
  ,module Development.Shake.Command
  ,module Development.Shake.FilePath
  ,module Development.Shake.Classes
  ,module Development.Shake.Config
  ,module Development.Shake.Rule
  ,module GHC.Generics
  ,BuildNode (..)
  ,buildNode
  ,need
  ,needs
  ,ShakeKey
  )
  where

import           Data.Time                  (UTCTime (..), utctDayTime)
import           Development.Shake hiding (need)
import           Development.Shake.Classes
import           Development.Shake.Command
import           Development.Shake.Config
import           Development.Shake.FilePath
import           Development.Shake.Rule     (EqualCost (..), Rule (..), apply,
                                             apply1, rule)
import           Development.Shake.Util
import           GHC.Generics
import           System.Directory           as IO
import           Text.Printf


getModTime :: FilePath -> IO Double
getModTime = fmap utcToDouble . getModificationTime
  where
    utcToDouble = fromRational . toRational . utctDayTime

type CaseId = String
type ShakeKey k  = (Generic k,Typeable k,Show k,Eq k,Hashable k,Binary k,NFData k)

class BuildNode a where
  paths :: a -> (FilePath, [FilePath])
  paths x = (path x, [])

  path :: a -> FilePath
  path = fst . paths

  paths' :: a -> [FilePath]
  paths' x = let (p1, ps) = (paths x) in p1:ps

  pathPrefix :: a -> FilePath
  pathPrefix = dropExtensions . path

  build :: a -> Maybe (Action ())
  build _ = Nothing

instance (ShakeKey k, BuildNode k) => Rule k [Double] where
    storedValue _ q = do
        exists <- traverse IO.doesFileExist $ paths' q
        if not (and exists) then return Nothing
        else fmap Just $ traverse getModTime $ paths' q
    equalValue _ _ old new = if old == new then EqualCheap else NotEqual

buildNode :: BuildNode a => a -> Maybe (Action [Double])
buildNode k = case (build k) of
  Nothing -> Just $ liftIO $ traverse getModTime $ paths' k -- No action, source node
  (Just action) -> Just $ do
      liftIO $ traverse (createDirectoryIfMissing True) $ map takeDirectory . paths' $ k
      action
      liftIO $ traverse getModTime $ paths' k

need :: (ShakeKey k, BuildNode k) => k -> Action [Double]
need = apply1

needs :: (ShakeKey k, BuildNode k) => [k] -> Action [[Double]]
needs = apply
