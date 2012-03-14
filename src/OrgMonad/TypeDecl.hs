module OrgMonad.TypeDecl
  where

import Control.Monad.Reader
import Data.Monoid
import OrgMonad.Type
import OrgMonad.Utils

instance Monoid a => Eq (Task a) where
  task1 == task2 = (taskId task1) == (taskId task2)

instance Monoid a => Show (Task a)
 where show task =
          concat [ "Task id : " ,show $ taskId task ]

instance Monoid a => Monoid (Task a) where
  mempty = Task {
    taskId           = 0
    ,taskName        = ""
    ,taskChildren    = []
    ,taskBackends    = mempty
    }
  mappend task1 task2 = Task {
    taskId           = 0
    ,taskName        = ""
    ,taskChildren    = mappend (taskChildren task1) (taskChildren task2)
    ,taskBackends    = mappend (taskBackends task1) (taskBackends task2)
  }

data RedmineBackendTask = RedmineBackendTask {
  redmineId :: String
}

data GcalBackendTask = GcalBackendTask {
  gcalId :: String
}

data BackendTask = BackendTask {
  redmineBackend :: Maybe RedmineBackendTask
  , gcalBackend :: Maybe GcalBackendTask
}

instance Monoid BackendTask where
  mempty
    = BackendTask Nothing Nothing
  mappend (BackendTask red1 gcal1) (BackendTask red2 gcal2)
    = BackendTask (mergeMaybe red1 red2) (mergeMaybe gcal1 gcal2)

instance BackendSync BackendTask OrgMonad where
  backendPull _backendTask = return mempty
  backendPush _task = return ()

