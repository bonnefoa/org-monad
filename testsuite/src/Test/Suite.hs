{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Test.Suite ( tests ) where

import Distribution.TestSuite
import qualified Distribution.TestSuite.HUnit as HU
import qualified Distribution.TestSuite.QuickCheck2 as QC
import Test.MetaState
import Test.Backends.AcidBackend

tests :: [Test]
tests = huTests ++ qcTests
  where
    huTests = $(HU.autoTest [
      "testMetaState"
      , "testAcidBackend"
      ])
    qcTests = $(QC.autoTest [
      ])

