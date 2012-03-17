module OrgMonad.Type
  where

import Data.Typeable
import Data.Monoid

-- * Task definitions

data Task = Task {
  taskName :: String
  , taskMeta :: MetaTask
} deriving (Show, Typeable, Eq)

instance Monoid Task where
  mempty = Task {
    taskName = mempty
    , taskMeta = mempty
  }
  mappend t1 _t2 = t1

data MetaTask = MetaTask {
  metaTaskId :: Integer
} deriving (Show, Typeable, Eq)


instance Monoid MetaTask where
  mempty = MetaTask {
    metaTaskId = 1
  }
  mappend t1 _t2 = t1



data OrgDB = OrgDB Task
    deriving (Show, Typeable)


instance Monoid OrgDB where
  mempty = OrgDB mempty
  mappend t1 _t2 = t1
