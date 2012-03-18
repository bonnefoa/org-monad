module OrgMonad.MetaType
  where

import Data.Acid
import Data.Monoid
import Data.Typeable
import qualified Data.Map as M
import OrgMonad.Type

-- * MetaTask definitions

data MetaTask a = MetaTask {
  metaTaskId :: Integer
  , metaTaskBackend :: [a]
} deriving (Show, Typeable)

instance Eq (MetaTask a) where
  m1 == m2 = (metaTaskId m1) == (metaTaskId m2)

-- * Dbs definitions

data MetaOrgDB a = MetaOrgDB {
  dbMetaTasks :: MetaTaskMap a
} deriving (Show, Typeable)

instance Monoid (MetaOrgDB a) where
  mempty = MetaOrgDB mempty
  mappend t1 _t2 = t1

updateMetaOrgDBWithTask :: MetaOrgDB a -> MetaTask a -> MetaOrgDB a
updateMetaOrgDBWithTask metaOrgDB metaTask =
  metaOrgDB {
    dbMetaTasks = (M.insert (metaTaskId metaTask) metaTask (dbMetaTasks metaOrgDB)) }

-- * Backends definitions

class BackendSync a where
  backendPull :: MetaTask a -> IO(Task)
  backendPush :: MetaTask a -> Task -> IO ()

-- * Types synonyms

type MetaDataState a = AcidState (MetaOrgDB a)
type MetaTaskMap a = M.Map Integer (MetaTask a)

