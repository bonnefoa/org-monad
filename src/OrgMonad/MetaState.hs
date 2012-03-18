{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module OrgMonad.MetaState
  where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Monoid
import Data.SafeCopy
import Data.Typeable
import OrgMonad.Type
import qualified Data.Map as M


type MetaDataState = AcidState MetaOrgDB

$(deriveSafeCopy 0 'base ''MetaTask)
$(deriveSafeCopy 0 'base ''MetaOrgDB)

getAcidState :: IO(MetaDataState)
getAcidState = openLocalState mempty

writeState :: MetaTask -> Update MetaOrgDB ()
writeState task = do
  metaOrgDB <- get
  put (metaOrgDB {dbMetaTasks = task : (dbMetaTasks metaOrgDB) })

queryState :: Query MetaOrgDB [MetaTask]
queryState = do
  MetaOrgDB task <- ask
  return task

$(makeAcidic ''MetaOrgDB ['writeState, 'queryState])

