-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.TestSuite.QuickCheck2
-- Copyright   :  Thomas Tuegel 2010
--
-- Maintainer  :  ttuegel@gmail.com
-- Portability :  portable
--
-- This module defines the instances to use QuickCheck 2 with the test
-- interface defined in 'Distribution.TestSuite'.

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
{-# LANGUAGE ExistentialQuantification #-}

module Distribution.TestSuite.QuickCheck2
    ( test
    , autoTest
    ) where

import Control.Monad (liftM)
import qualified Control.Exception as C
import Data.Maybe (catMaybes, fromJust, maybe)
import Data.Typeable (Typeable(..))
import qualified Distribution.TestSuite as Cabal
import Language.Haskell.TH
import System.Random (newStdGen, next, StdGen)
import qualified Test.QuickCheck as QC

data QCTest = forall prop. QC.Testable prop => QCTest String prop

-- | Convert a testable QuickCheck property into a named test.
test :: QC.Testable prop
     => String      -- ^ test name
     -> prop        -- ^ QuickCheck test
     -> Cabal.Test
test n p = Cabal.impure $ QCTest n p

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

instance Cabal.TestOptions QCTest where
    name (QCTest n _) = n

    options _ =
        [ ("std-gen", typeOf (undefined :: String))
        , ("max-success", typeOf (undefined :: Int))
        , ("max-discard", typeOf (undefined :: Int))
        , ("size", typeOf (undefined :: Int))
        ]

    defaultOptions _ = do
        rng <- newStdGen
        return $ Cabal.Options $
            [ ("std-gen", show rng)
            , ("max-success", show $ QC.maxSuccess QC.stdArgs)
            , ("max-discard", show $ QC.maxDiscard QC.stdArgs)
            , ("size", show $ QC.maxSize QC.stdArgs)
            ]

    check t (Cabal.Options opts) = catMaybes
        [ maybeNothing "max-success" ([] :: [(Int, String)])
        , maybeNothing "max-discard" ([] :: [(Int, String)])
        , maybeNothing "size" ([] :: [(Int, String)])
        ]
        -- There is no need to check the parsability of "std-gen"
        -- because the Read instance for StdGen always succeeds.
        where
            maybeNothing n x =
                maybe Nothing (\str ->
                    if reads str == x then Just n else Nothing)
                    $ lookup n opts

instance Cabal.ImpureTestable QCTest where
    runM (QCTest _ prop) o =
        C.catch go raiseMgmt
        where
            go = do
                result <- QC.quickCheckWithResult args prop
                return $ case result of
                        QC.Success {} -> Cabal.Pass
                        QC.GaveUp {}->
                            Cabal.Fail $ "gave up after "
                                       ++ show (QC.numTests result)
                                       ++ " tests"
                        QC.Failure {} -> Cabal.Fail $ QC.reason result
                        QC.NoExpectedFailure {} ->
                            Cabal.Fail "passed (expected failure)"
            raiseMgmt :: C.SomeException -> IO Cabal.Result
            raiseMgmt = (return . Cabal.Error . show)
            args = QC.Args
                { QC.replay = Just
                    ( Cabal.lookupOption "std-gen" o
                    , Cabal.lookupOption "size" o
                    )
                , QC.maxSuccess = Cabal.lookupOption "max-success" o
                , QC.maxDiscard = Cabal.lookupOption "max-discard" o
                , QC.maxSize = Cabal.lookupOption "size" o
                , QC.chatty = False
                }
