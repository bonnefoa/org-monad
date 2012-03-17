module OrgMonad.Type
  where

import qualified Data.Map as M
import Data.Typeable
import Data.Monoid

-- * MetaTask definitions

data MetaTask = MetaTask {
  metaTaskId :: Integer
} deriving (Show, Typeable, Eq)

instance Monoid MetaTask where
  mempty = MetaTask {
    metaTaskId = 1
  }
  mappend t1 _t2 = t1

-- * Dbs definitions

data MetaOrgDB = MetaOrgDB {
  dbMetaTasks :: [MetaTask]
} deriving (Show, Typeable)

instance Monoid MetaOrgDB where
  mempty = MetaOrgDB mempty
  mappend t1 _t2 = t1
