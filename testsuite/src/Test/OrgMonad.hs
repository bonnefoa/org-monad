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
import OrgMonad.OrgMonad
import OrgMonad.Type
import qualified Data.Map as M
import System.Directory
import Test.HUnit hiding (assert)
import Test.Common

testOrgMonadPush :: Test
testOrgMonadPush = TestCase assertionPush

testOrgMonadPull :: Test
testOrgMonadPull = TestCase assertionPull

testBackend = SimpleBackend (Just 1)
indexTask = IndexTask 1 testBackend
testTask = mempty {taskId=1, taskName="test"}

simplePush :: GlobalConf -> IO()
simplePush conf@(GlobalConf acid indexAcid) = do
  {-Push index-}
  update indexAcid (UpdateTask indexTask)
  {-Push a task to backend-}
  runReaderT (backendPush indexTask testTask) conf

assertionPush :: Assertion
assertionPush = do
  cleanStateDir
  conf@(GlobalConf acid indexAcid) <- initConf
  simplePush conf
  {-Check index-}
  indexInBase <- query indexAcid (GetIndexTask 1)
  Just indexTask @?= indexInBase
  {-Check backend-}
  taskInBase <- query acid (GetTask 1)
  Just testTask @?= taskInBase
  return ()

assertionPull :: Assertion
assertionPull = do
  cleanStateDir
  conf@(GlobalConf acid indexAcid) <- initConf
  simplePush conf
  taskInBase <- query acid (GetTask 1)
  inBaseTask <- runReaderT (backendPull indexTask) conf
  inBaseTask @?= taskInBase
  return ()

