{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module OrgMonad.Backends.AcidBackend
  where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import OrgMonad.Backends.AcidType
import OrgMonad.Type

type AcidOrgState = AcidState OrgDB

$(deriveSafeCopy 0 'base ''Task)
$(deriveSafeCopy 0 'base ''OrgDB)

writeState :: Task -> Update OrgDB ()
writeState task = do
  orgDB <- get
  put (updateOrgDBWithTask orgDB task)

getTasks :: Query OrgDB TaskMap
getTasks = do
  OrgDB task <- ask
  return task

$(makeAcidic ''OrgDB ['writeState, 'getTasks])

