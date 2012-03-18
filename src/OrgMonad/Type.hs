module OrgMonad.Type
  where

import qualified Data.Map as M
import Data.Typeable
import Data.Monoid

type MetaTaskMap = M.Map Integer MetaTask

-- * MetaTask definitions

data MetaTask = MetaTask {
  metaTaskId :: Integer
} deriving (Show, Typeable, Eq)

instance Monoid MetaTask where
  mempty = MetaTask {
    metaTaskId = 0
  }
  mappend t1 _t2 = t1

-- * Dbs definitions

data MetaOrgDB = MetaOrgDB {
  dbMetaTasks :: MetaTaskMap
} deriving (Show, Typeable)

instance Monoid MetaOrgDB where
  mempty = MetaOrgDB mempty
  mappend t1 _t2 = t1

updateMetaOrgDBWithTask :: MetaOrgDB -> MetaTask -> MetaOrgDB
updateMetaOrgDBWithTask metaOrgDB metaTask =
  metaOrgDB {
    dbMetaTasks = (M.insert (metaTaskId metaTask) metaTask (dbMetaTasks metaOrgDB)) }

