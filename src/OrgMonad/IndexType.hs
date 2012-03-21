module OrgMonad.IndexType
  where

import Control.Monad.Reader
import Data.Acid
import Data.Monoid
import Data.Typeable
import qualified Data.Map as M
import OrgMonad.Type

-- * IndexTask definitions

data IndexTask a = IndexTask {
  metaTaskId :: Integer
  , metaTaskBackend :: [a]
} deriving (Show, Typeable)

instance Eq (IndexTask a) where
  m1 == m2 = (metaTaskId m1) == (metaTaskId m2)

-- * Dbs definitions

data IndexOrgDB a = IndexOrgDB {
  dbIndexTasks :: IndexTaskMap a
} deriving (Show, Typeable)

instance Monoid (IndexOrgDB a) where
  mempty = IndexOrgDB mempty
  mappend t1 _t2 = t1

updateIndexOrgDBWithTask :: IndexOrgDB a -> IndexTask a -> IndexOrgDB a
updateIndexOrgDBWithTask metaOrgDB metaTask =
  metaOrgDB {
    dbIndexTasks = (M.insert (metaTaskId metaTask) metaTask (dbIndexTasks metaOrgDB)) }

-- * Backends definitions

class BackendSync a b where
  backendPull :: IndexTask a -> (ReaderT b) IO(Task)
  backendPush :: IndexTask a -> Task -> (ReaderT b) IO ()

-- * Types synonyms

type IndexDataState a = AcidState (IndexOrgDB a)
type IndexTaskMap a = M.Map Integer (IndexTask a)

