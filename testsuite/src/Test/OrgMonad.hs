{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Test.OrgMonad
  where

import Control.Monad.Reader
import Data.Acid
import Data.Monoid
import Data.SafeCopy
import Data.Typeable
import OrgMonad.Backends.AcidBackend
import OrgMonad.IndexState
import OrgMonad.IndexType
import OrgMonad.OrgMonad
import OrgMonad.Type
import qualified Data.Map as M
import System.Directory
import Test.HUnit hiding (assert)
import Test.Common

testOrgMonad :: Test
testOrgMonad = TestCase assertionSimpleBackend

assertionSimpleBackend :: Assertion
assertionSimpleBackend = do
  cleanStateDir
  conf@(GlobalConf acid indexAcid) <- initConf

  {-Push index-}
  let testBackend = SimpleBackend 1
  let indexTask = IndexTask 1 [testBackend]
  update indexAcid (UpdateTask indexTask)
  {-Check index-}
  res <- query indexAcid GetIndexTasks
  Just indexTask @?= M.lookup 1 res

  {-Push a task to backend-}
  let testTask = mempty {taskId=1, taskName="test"}
  runReaderT (backendPush indexTask testTask) conf

  {-Check backend-}
  res <- query acid GetTasks
  Just testTask @?= M.lookup 1 res

  return ()

