{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module OrgMonad.Type
  where

import Control.Monad.Reader
import Data.SafeCopy

import Data.Typeable

-- * Task definition

-- | Datastructure for org-monad tasks
data  MetaTask a = MetaTask {
  metaTaskId            :: Integer
  , metaTaskBackends    :: [ a ]
} deriving (Show, Typeable)

-- | Datastructure for org-monad tasks
data Task a = Task {
  taskId            :: Integer
  , taskName        :: String
  , taskMeta    :: MetaTask a
}

data OrgMonad = OrgMonad {
  test :: String
}

data OrgMonadState = OrgMonadState {
  orgMonad :: OrgMonad
}

$(deriveSafeCopy 1 'base ''OrgMonadState)

class BackendSync a b where
  backendPull :: a -> (ReaderT b) IO (Task a)
  backendPush :: Task a -> (ReaderT b) IO ()




-- * Backend definition

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




-- * Configuration definitions


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



-- * Types synonyms

type OrgMonadR = ReaderT OrgMonad

