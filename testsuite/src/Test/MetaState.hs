{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Test.MetaState
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

data TestBackend = TestBackend
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''TestBackend)

testMetaState :: Test
testMetaState = TestCase assertionMetaState

assertionMetaState :: Assertion
assertionMetaState = do
  removeDirectoryRecursive "state"
  acid <- openLocalState mempty
  let testTask = MetaTask 1 [TestBackend]
  update acid (UpdateTask testTask)
  res <- query acid GetMetaTasks
  Just testTask @?= M.lookup 1 res

