module OrgMonad.Type
  where

import Control.Monad.Reader
import Data.Acid
import Data.Monoid
import Data.Typeable
import qualified Data.Map as M

type TaskId = Integer
type TaskMap = M.Map TaskId Task

-- * Task definitions

data Task = Task {
  taskId     :: TaskId
  , taskName :: String
} deriving (Show, Typeable, Eq)

instance Monoid Task where
  mempty = Task {
      taskId     = 0
      , taskName = mempty
    }
  mappend t1 _t2 = t1


type IndexId = Integer

-- * IndexTask definitions

data IndexTask a = IndexTask {
  indexTaskId :: IndexId
  , indexTaskBackend :: a
} deriving (Show, Typeable)

instance Eq (IndexTask a) where
  m1 == m2 = (indexTaskId m1) == (indexTaskId m2)

-- * Dbs definitions

data IndexOrgDB a = IndexOrgDB {
  dbIndexTasks :: IndexTaskMap a
} deriving (Show, Typeable)

instance Monoid (IndexOrgDB a) where
  mempty = IndexOrgDB mempty
  mappend t1 _t2 = t1

updateIndexOrgDBWithTask :: IndexOrgDB a -> IndexTask a -> IndexOrgDB a
updateIndexOrgDBWithTask indexOrgDB indexTask =
  indexOrgDB {
    dbIndexTasks = (M.insert (indexTaskId indexTask) indexTask (dbIndexTasks indexOrgDB)) }

-- * Backends definitions

class BackendSync a b where
  backendPull :: IndexTask a -> (ReaderT b) IO(Maybe Task)
  backendPush :: IndexTask a -> Task -> (ReaderT b) IO ()

-- * Types synonyms

type IndexDataState a = AcidState (IndexOrgDB a)
type IndexTaskMap a = M.Map Integer (IndexTask a)

