module OrgMonad.Type
  where

import qualified Data.Map as M
import Data.Typeable
import Data.Monoid

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
