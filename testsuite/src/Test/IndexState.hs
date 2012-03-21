{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Test.IndexState
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

data TestBackend = TestBackend
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''TestBackend)

testIndexState :: Test
testIndexState = TestCase assertionIndexState

assertionIndexState :: Assertion
assertionIndexState = do
  removeDirectoryRecursive "state"
  acid <- openLocalState mempty
  let testTask = IndexTask 1 [TestBackend]
  update acid (UpdateTask testTask)
  res <- query acid GetIndexTasks
  Just testTask @?= M.lookup 1 res

