module OrgMonad.AcidState
  where

import Data.Acid
import Data.SafeCopy
import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader

import OrgMonad.Type

writeState :: OrgMonad -> Update OrgMonadState ()
writeState orgMonad = put (OrgMonadState orgMonad)

queryState :: Query OrgMonadState OrgMonad
queryState = do
  OrgMonadState res <- ask
  return res

$(makeAcidic ''OrgMonadState ['writeState, 'queryState])

doSave :: AcidState -> OrgMonadState -> IO()
doSave acid st = update acid (WriteState st)

