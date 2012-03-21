{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Test.OrgMonad
  where

import Data.Acid
import Data.Typeable
import Data.Monoid
import OrgMonad.IndexState
import OrgMonad.Type
import OrgMonad.IndexType
import qualified Data.Map as M
import System.Directory
import Test.HUnit hiding (assert)
import Data.SafeCopy

testOrgMonad :: Test
testOrgMonad = TestCase assertionRead

assertionRead :: Assertion
assertionRead = do
  removeDirectoryRecursive "state"
  {-conf <- initConf-}

  {-let testTask = IndexTask 1 [TestBackend]-}
  {-update acid (UpdateTask testTask)-}
  {-res <- query acid GetIndexTasks-}
  {-Just testTask @?= M.lookup 1 res-}
  return ()


