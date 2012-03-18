module OrgMonad.Backends.AcidType
  where

import qualified Data.Map as M
import Data.Typeable
import Data.Monoid
import OrgMonad.Type

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

