module Test.AcidState
  where

import Data.Acid
import Data.Monoid
import Test.HUnit hiding (assert)
import Data.Acid

import OrgMonad.Type
import OrgMonad.AcidState

import qualified Data.Map as M

testAcidState :: Test
testAcidState = TestCase assertionAcidState

assertionAcidState :: Assertion
assertionAcidState = do
  acid <- getAcidState
  let testDb = Task 1 "test" mempty
  update acid (WriteState testDb)
  res <- query acid QueryState
  Just testDb @?= M.lookup 1 res

