module OrgMonad.Type
  where

import Control.Monad.Reader

-- * Types synonyms

type OrgMonadR = ReaderT OrgMonad

-- * Task definition

-- | Datastructure for org-monad tasks
data MetaTask a = MetaTask {
  taskId            :: Integer
  , taskChildren    :: [Task a]
  , taskBackends    :: [ a ]
}

-- | Datastructure for org-monad tasks
data Task a = Task {
  taskId            :: Integer
  , taskName        :: String
  , taskChildren    :: [Task a]
  , taskBackends    :: [ a ]
}

class BackendSync a b where
  backendPull :: a -> (ReaderT b) IO (Task a)
  backendPush :: Task a -> (ReaderT b) IO ()

-- * Backend definition

-- | Configuration for redmine backend
data RedmineBackendConf = RedmineBackendConf {
    redmineToken :: String
    , redmineHost :: String
}

-- | Configuration for google cal backend
data GcalBackendConf = GcalBackendConf {
    gcalToken :: String
}

-- * Global configuration

data OrgMonad = OrgMonad {
    orgMonadRedmineConf :: RedmineBackendConf
    , orgMonadGcalConf :: GcalBackendConf
}

