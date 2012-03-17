module Test.AcidState
  where

import Data.Acid
import Data.Monoid
import Test.HUnit hiding (assert)
import Data.Acid

import OrgMonad.Type
import OrgMonad.AcidState

testAcidState :: Test
testAcidState = TestCase assertionAcidState

assertionAcidState :: Assertion
assertionAcidState = do
  acid <- getAcidState
  let testTask = Task "test" mempty
  update acid (WriteState testTask)
  res <- query acid QueryState
  testTask @?= res
