{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Test.OrgMonad
  where

import Data.Acid
import Data.Typeable
import Data.Monoid
import OrgMonad.MetaState
import OrgMonad.Type
import OrgMonad.MetaType
import qualified Data.Map as M
import System.Directory
import Test.HUnit hiding (assert)
import Data.SafeCopy

testOrgMonad :: Test
testOrgMonad = TestCase assertionRead

assertionRead :: Assertion
assertionRead = do
  removeDirectoryRecursive "state"
  conf <- initConf

  {-let testTask = MetaTask 1 [TestBackend]-}
  {-update acid (UpdateTask testTask)-}
  {-res <- query acid GetMetaTasks-}
  {-Just testTask @?= M.lookup 1 res-}
  return ()


