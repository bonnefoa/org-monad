module Test.Backends.AcidBackend
  where

import Data.Acid
import Data.Monoid
import Test.HUnit hiding (assert)
import Data.Acid

import OrgMonad.Type
import OrgMonad.Backends.AcidType
import OrgMonad.Backends.AcidBackend

import qualified Data.Map as M
import System.Directory
import Test.Common

testAcidBackend :: Test
testAcidBackend = TestCase assertionAcidState

assertionAcidState :: Assertion
assertionAcidState = do
  cleanStateDir
  acid <- openLocalState mempty
  let testDb = Task 1 "test"
  update acid (WriteState testDb)
  res <- query acid GetTasks
  Just testDb @?= M.lookup 1 res

