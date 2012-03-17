{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module OrgMonad.AcidState
  where

import Data.Acid
import Data.Monoid

import Control.Monad.State
import Control.Monad.Reader
import Data.SafeCopy

import Data.Typeable
import OrgMonad.Type
import qualified Data.Map as M


type AcidOrgState = AcidState OrgDB

$(deriveSafeCopy 0 'base ''Task)
$(deriveSafeCopy 0 'base ''MetaTask)
$(deriveSafeCopy 0 'base ''OrgDB)

getAcidState :: IO(AcidOrgState)
getAcidState = openLocalState mempty

writeState :: Task -> Update OrgDB ()
writeState task = do
  orgDB <- get
  put (updateOrgDBWithTask orgDB task)

queryState :: Query OrgDB TaskMap
queryState = do
  OrgDB task <- ask
  return task

$(makeAcidic ''OrgDB ['writeState, 'queryState])

