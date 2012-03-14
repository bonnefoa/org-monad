module OrgMonad.Type
  where

import Data.Monoid
import Control.Monad.Reader

-- * Task definition

-- | Datastructure for org-monad tasks
data Monoid a => Task a = Task {
  taskId            :: Integer
  , taskName        :: String
  , taskChildren    :: [Task a]
  , taskBackends    :: a
}

class Monoid a => BackendSync a b where
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

