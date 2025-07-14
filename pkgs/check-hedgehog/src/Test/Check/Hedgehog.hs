module Test.Check.Hedgehog
  ( propertyNamed
  ) where

import Hedgehog (Property)
import Hedgehog qualified as H
import Test.Check.Core.Types

propertyNamed :: Property -> VeriTest
propertyNamed p =
  VeriTest
    { name = TestName "Hedgehog Property"
    , description = "A property defined using Hedgehog"
    , test = id @(IO Result) $ do
        result <- H.check p
        pure $ Result
          { outcome = if result then Success else Failure "Property failed"
          }
    }
