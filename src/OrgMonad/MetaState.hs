{-# LANGUAGE TemplateHaskell #-}
module OrgMonad.MetaState
  where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import OrgMonad.MetaType

$(deriveSafeCopy 0 'base ''MetaTask)
$(deriveSafeCopy 0 'base ''MetaOrgDB)

updateTask :: MetaTask a -> Update (MetaOrgDB a) ()
updateTask metaTask = do
  metaOrgDB <- get
  put $ updateMetaOrgDBWithTask metaOrgDB metaTask

getMetaTasks :: Query (MetaOrgDB a) (MetaTaskMap a)
getMetaTasks = do
  MetaOrgDB metaTask <- ask
  return metaTask

$(makeAcidic ''MetaOrgDB ['updateTask, 'getMetaTasks])

