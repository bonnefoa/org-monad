{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module OrgMonad.Backends.AcidBackend
  where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import OrgMonad.Backends.AcidType
import OrgMonad.Type
import qualified Data.Map as M

type AcidOrgState = AcidState OrgDB

$(deriveSafeCopy 0 'base ''Task)
$(deriveSafeCopy 0 'base ''OrgDB)

writeTask :: Task -> Update OrgDB ()
writeTask task = do
  orgDB <- get
  put (updateOrgDBWithTask orgDB task)

getTasks :: Query OrgDB TaskMap
getTasks = do
  OrgDB task <- ask
  return task

getTask :: TaskId -> Query OrgDB (Maybe Task)
getTask taskId = getTasks >>= return . (M.lookup taskId)

$(makeAcidic ''OrgDB ['writeTask, 'getTasks, 'getTask])

pushToAcidBackend :: Task -> AcidOrgState -> IO ()
pushToAcidBackend task acid =
  update acid (WriteTask task)

