{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module OrgMonad.MetaState
  where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Monoid
import Data.SafeCopy
import OrgMonad.Type

type MetaDataState = AcidState MetaOrgDB

$(deriveSafeCopy 0 'base ''MetaTask)
$(deriveSafeCopy 0 'base ''MetaOrgDB)

getAcidState :: IO(MetaDataState)
getAcidState = openLocalState mempty

updateTask :: MetaTask -> Update MetaOrgDB ()
updateTask metaTask = do
  metaOrgDB <- get
  put $ updateMetaOrgDBWithTask metaOrgDB metaTask

getMetaMap :: Query MetaOrgDB MetaTaskMap
getMetaMap = do
  MetaOrgDB metaTask <- ask
  return metaTask

$(makeAcidic ''MetaOrgDB ['updateTask, 'getMetaMap])

