module Test.Backends.AcidBackend
  where

import Data.Acid
import Data.Monoid
import Test.HUnit hiding (assert)
import Data.Acid

import OrgMonad.Backends.AcidType
import OrgMonad.Backends.AcidBackend

import qualified Data.Map as M
import System.Directory

testAcidBackend :: Test
testAcidBackend = TestCase assertionAcidState

assertionAcidState :: Assertion
assertionAcidState = do
  removeDirectoryRecursive "state"
  acid <- getAcidState
  let testDb = Task 1 "test"
  update acid (WriteState testDb)
  res <- query acid QueryState
  Just testDb @?= M.lookup 1 res


