module OrgMonad.OrgMonad
  where

import Control.Monad.Reader
import Data.Acid
import Data.Monoid
import Data.SafeCopy
import Data.Typeable
import OrgMonad.Backends.AcidBackend
import OrgMonad.IndexState
import OrgMonad.IndexType
import OrgMonad.Type
import qualified Data.Map as M

data SimpleBackend = SimpleBackend {
  orgDbId :: TaskId
} deriving (Show, Typeable)

data GlobalConf = GlobalConf {
  confBackend :: AcidOrgState
  , confIndexBackend :: ReifiedIndex
}

type ReifiedIndex = IndexAcidOrgState SimpleBackend

$(deriveSafeCopy 0 'base ''SimpleBackend)

instance BackendSync SimpleBackend GlobalConf where
  backendPull indexTask = return mempty
  backendPush indexTask task = do
    acid <- asks confBackend
    lift $ pushToAcidBackend task acid

initConf :: IO GlobalConf
initConf = do
  acid <- openLocalState mempty
  metaAcid <- openLocalState mempty
  return $ GlobalConf acid metaAcid

