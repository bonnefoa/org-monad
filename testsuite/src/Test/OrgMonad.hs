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

data SimpleBackend = SimpleBackend {
  orgDbId :: Integer
} deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''SimpleBackend)

testOrgMonad :: Test
testOrgMonad = TestCase assertionRead

assertionRead :: Assertion
assertionRead = do
  {-acid <- openLocalState mempty-}
  {-metaAcid <- openLocalState mempty-}

  {-removeDirectoryRecursive "state"-}
  {-acid <- openLocalState mempty-}
  {-let testTask = MetaTask 1 [TestBackend]-}
  {-update acid (UpdateTask testTask)-}
  {-res <- query acid GetMetaTasks-}
  {-Just testTask @?= M.lookup 1 res-}
  return ()

