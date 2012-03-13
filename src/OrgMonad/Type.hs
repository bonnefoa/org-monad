module OrgMonad.Type
  where

import Data.Monoid
import Data.Time.LocalTime

-- * Task definition

-- | Datastructure for org-monad tasks
data Task = Task {
  taskId            :: Integer
  , taskName        :: String
  , taskChildren    :: [Task]
  , taskDescription :: Maybe String
  , taskBegin       :: Maybe LocalTime
  , taskEnd         :: Maybe LocalTime
  , taskDuration    :: Maybe Integer
  , taskBackends    :: [ BackendType ]
}

instance Eq Task where
  task1 == task2 = taskId task1 == taskId task2

instance Show Task
 where show task =
          concat [
            "Task |id : " ,show $ taskId task
            ," |name : ",show $ taskName task
          ]

instance Monoid Task where
  mempty = Task {
    taskId           = 0
    ,taskName        = ""
    ,taskChildren    = []
    ,taskDescription = Nothing
    ,taskBegin       = Nothing
    ,taskEnd         = Nothing
    ,taskDuration    = Nothing
    ,taskBackends    = []
    }
  mappend task1 task2 = Task {
    taskId           = 0
    ,taskName        = ""
    ,taskChildren    = []
    ,taskDescription = Nothing
    ,taskBegin       = Nothing
    ,taskEnd         = Nothing
    ,taskDuration    = Nothing
    ,taskBackends    = []
  }





-- * Backend definition

-- | Types of backends available for tasks data BackendType =
data BackendType =
  Redmine
  | GoogleCal
  deriving (Show, Eq)

-- | Configuration for redmine backend
data RedmineBackend = RedmineBackend {
    redmineToken :: String
}

-- | Configuration for google cal backend
data GcalBackend = GcalBackend {
    gcalToken :: String
}

-- * Global configuration

{-
 -data Configuration = Configuration {
 -  backends :: RedmineBackend
 - }
 -}


