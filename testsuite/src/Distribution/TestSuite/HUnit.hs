-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.TestSuite.HUnit
-- Copyright   :  Thomas Tuegel 2010
--
-- Maintainer  :  ttuegel@gmail.com
-- Portability :  portable
--
-- This module defines the instances to use HUnit with the test interface
-- defined in 'Distribution.TestSuite'.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Thomas Tuegel nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}
module Distribution.TestSuite.HUnit
    ( test
    , autoTest
    ) where

import Control.Monad (liftM)
import qualified Control.Exception as C
import qualified Distribution.TestSuite as Cabal
import Language.Haskell.TH
import qualified Test.HUnit as HUnit

data HUnitTest = HUnitTest String HUnit.Test

-- | Convert an HUnit test into a named Cabal test.
test :: String -> HUnit.Test -> Cabal.Test
test n t = Cabal.impure $ HUnitTest n t

-- | Use @autoTest@ when you have a list of tests that should have the same
-- name as the functions that define them.  Given a list of test names,
-- @autoTest@ calls 'test' with the name of each and the function of the same
-- name.  Splicing @autoTest@ results in an expression of type @[Test].
autoTest :: [String] -> Q Exp
autoTest strings = do
    let testE = [| test |]
        names = map (litE . stringL) strings
        functions = map (varE . mkName) strings
    listE $ zipWith (\s n -> testE `appE` s `appE` n) names functions

instance Cabal.TestOptions HUnitTest where
    name (HUnitTest n _) = n
    options _ = []
    defaultOptions _ = return $! Cabal.Options []
    check _ _ = []

instance Cabal.ImpureTestable HUnitTest where
    runM (HUnitTest _ t) _ =
        C.catch go raiseMgmt
        where
            start :: HUnit.State -> Cabal.Result -> IO Cabal.Result
            start _ x = return x
            anError :: String -> HUnit.State -> Cabal.Result -> IO Cabal.Result
            anError msg _ _ = return $ Cabal.Error msg
            failure :: String -> HUnit.State -> Cabal.Result -> IO Cabal.Result
            failure msg _ _ = return $ Cabal.Fail msg
            raiseMgmt :: C.SomeException -> IO Cabal.Result
            raiseMgmt = (return . Cabal.Error . show)
            go = liftM snd
                $ HUnit.performTest start anError failure Cabal.Pass t
