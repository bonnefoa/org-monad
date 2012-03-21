module Test.Common
  where

import System.Directory

cleanStateDir :: IO()
cleanStateDir = do
  exists <- doesDirectoryExist "state"
  if exists
      then removeDirectoryRecursive "state"
      else createDirectory "state"

