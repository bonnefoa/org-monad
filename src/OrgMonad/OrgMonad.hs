module OrgMonad.OrgMonad
  where

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

type ReifiedIndex = IndexAcidOrgState SimpleBackend

$(deriveSafeCopy 0 'base ''SimpleBackend)

instance BackendSync SimpleBackend where
  backendPull ::

data GlobalConf = GlobalConf {
  confBackend :: AcidOrgState
  , confIndexBackend :: ReifiedIndex
}

initConf :: IO GlobalConf
initConf = do
  acid <- openLocalState mempty
  metaAcid <- openLocalState mempty
  return $ GlobalConf acid metaAcid

