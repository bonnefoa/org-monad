module OrgMonad.OrgMonad
  where

import Data.Maybe
import Control.Monad.Reader
import Data.Acid
import Data.Monoid
import Data.SafeCopy
import Data.Typeable
import OrgMonad.Backends.AcidBackend
import OrgMonad.Index.IndexState
import OrgMonad.Index.IndexType
import OrgMonad.Type

data SimpleBackend = SimpleBackend {
  mbOrgDbId :: Maybe TaskId
} deriving (Show, Typeable)

data Backends =
  AcidBackend {
    acidBackendId :: TaskId
  }
  | FileBackend {
    fileBackendId :: TaskId
  }


data GlobalConf = GlobalConf {
  confBackend :: AcidOrgState
  , confIndexBackend :: ReifiedIndex
}

type ReifiedIndex = IndexAcidOrgState SimpleBackend

$(deriveSafeCopy 0 'base ''SimpleBackend)

instance BackendSync SimpleBackend GlobalConf where
  backendPull indexTask = do
    acid <- asks confBackend
    let mbTask = (GetMbTask . mbOrgDbId . indexTaskBackend) indexTask
    lift $ (query acid) mbTask
  backendPush _indexTask task = do
    acid <- asks confBackend
    lift $ pushToAcidBackend task acid

initConf :: IO GlobalConf
initConf = do
  acid <- openLocalState mempty
  indexAcid <- openLocalState mempty
  return $ GlobalConf acid indexAcid

