{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Test.IndexState
  where

import Data.Acid
import Data.Typeable
import Data.Monoid
import OrgMonad.IndexState
import OrgMonad.Type
import qualified Data.Map as M
import System.Directory
import Test.HUnit hiding (assert)
import Data.SafeCopy
import Test.Common

data TestBackend = TestBackend
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''TestBackend)

testIndexState :: Test
testIndexState = TestCase assertionIndexState

assertionIndexState :: Assertion
assertionIndexState = do
  cleanStateDir
  acid <- openLocalState mempty
  let testTask = IndexTask 1 [TestBackend]
  update acid (UpdateTask testTask)
  res <- query acid (GetIndexTask 1)
  res @?= Just testTask

