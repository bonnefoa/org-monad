{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module OrgMonad.Index.IndexState
  where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import OrgMonad.Index.IndexType
import qualified Data.Map as M

type IndexAcidOrgState a = AcidState (IndexOrgDB a)


$(deriveSafeCopy 0 'base ''IndexTask)
$(deriveSafeCopy 0 'base ''IndexOrgDB)

updateTask :: IndexTask a -> Update (IndexOrgDB a) ()
updateTask indexTask = do
  indexOrgDB <- get
  put $ updateIndexOrgDBWithTask indexOrgDB indexTask

getIndexTasks :: Query (IndexOrgDB a) (IndexTaskMap a)
getIndexTasks = do
  IndexOrgDB indexTask <- ask
  return indexTask

getIndexTask :: IndexId -> Query (IndexOrgDB a) (Maybe (IndexTask a))
getIndexTask indexId = getIndexTasks >>= return . (M.lookup indexId)


$(makeAcidic ''IndexOrgDB ['updateTask, 'getIndexTasks, 'getIndexTask])

