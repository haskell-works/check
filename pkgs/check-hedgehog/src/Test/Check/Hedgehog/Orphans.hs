{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Check.Hedgehog.Orphans
  (
  ) where

import Hedgehog (Property)
import Test.Check.Core
import Test.Check.Hedgehog.Core

instance IsGroup Property where
  toGroup = testPropertyNamed
