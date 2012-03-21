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

$(deriveSafeCopy 0 'base ''SimpleBackend)

{-instance BackendSync SimpleBackend where-}
  {-backendPull ::-}

data GlobalConf = GlobalConf {
  confBackend :: AcidOrgState
  , confIndexBackend :: IndexAcidOrgState SimpleBackend
}

{-initConf :: GlobalConf-}
{-initConf = do-}
  {-acid <- openLocalState mempty-}
  {-metaAcid <- openLocalState mempty-}
  {-return $ GlobalConf acid metaAcid-}

