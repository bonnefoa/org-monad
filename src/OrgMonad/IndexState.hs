{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module OrgMonad.IndexState
  where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import OrgMonad.IndexType

type IndexAcidOrgState a = AcidState (IndexOrgDB a)

$(deriveSafeCopy 0 'base ''IndexTask)
$(deriveSafeCopy 0 'base ''IndexOrgDB)

updateTask :: IndexTask a -> Update (IndexOrgDB a) ()
updateTask metaTask = do
  metaOrgDB <- get
  put $ updateIndexOrgDBWithTask metaOrgDB metaTask

getIndexTasks :: Query (IndexOrgDB a) (IndexTaskMap a)
getIndexTasks = do
  IndexOrgDB metaTask <- ask
  return metaTask

$(makeAcidic ''IndexOrgDB ['updateTask, 'getIndexTasks])

