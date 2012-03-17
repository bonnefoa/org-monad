module OrgMonad.Backends.AcidType
  where

import qualified Data.Map as M
import Data.Typeable
import Data.Monoid

type TaskMap = M.Map Integer Task

-- * Task definitions

data Task = Task {
  taskId     :: Integer
  , taskName :: String
} deriving (Show, Typeable, Eq)

instance Monoid Task where
  mempty = Task {
      taskId     = 1
      , taskName = mempty
    }
  mappend t1 _t2 = t1

-- * Dbs definitions

data OrgDB = OrgDB {
  dbTasks :: TaskMap
} deriving (Show, Typeable)

instance Monoid OrgDB where
  mempty = OrgDB mempty
  mappend t1 _t2 = t1

updateOrgDBWithTask :: OrgDB -> Task -> OrgDB
updateOrgDBWithTask orgDB task =
  orgDB {
    dbTasks = (M.insert (taskId task) task (dbTasks orgDB)) }

