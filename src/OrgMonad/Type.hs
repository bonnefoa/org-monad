module OrgMonad.Type
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
      taskId     = 0
      , taskName = mempty
    }
  mappend t1 _t2 = t1

