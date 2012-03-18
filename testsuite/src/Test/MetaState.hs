module Test.MetaState
  where

import Data.Acid
import Data.Monoid
import Test.HUnit hiding (assert)
import Data.Acid

import OrgMonad.Type
import OrgMonad.MetaState

import qualified Data.Map as M
import System.Directory

testMetaState :: Test
testMetaState = TestCase assertionMetaState

assertionMetaState :: Assertion
assertionMetaState = do
  removeDirectoryRecursive "state"
  acid <- getAcidState
  let testDb = MetaTask 1
  update acid (UpdateTask testDb)
  res <- query acid GetMetaMap
  Just testDb @?= M.lookup 1 res

