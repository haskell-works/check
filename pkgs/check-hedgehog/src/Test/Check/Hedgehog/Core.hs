module Test.Check.Hedgehog.Core
  ( propertyNamed,
    propertyOnce,
  ) where

import Control.Monad.Morph
import Control.Monad.Trans.Resource
import Hedgehog (Property)
import Hedgehog qualified as H
import Test.Check.Core.Types

propertyNamed :: Property -> Test
propertyNamed p =
  Test
    { name = TestName "Hedgehog Property"
    , description = "A property defined using Hedgehog"
    , test = id @(IO Result) $ do
        result <- H.check p
        pure $ Result
          { outcome = if result then Success else Failure "Property failed"
          }
    }

propertyOnce :: H.PropertyT (ResourceT IO) () -> H.Property
propertyOnce = H.withTests 1 . H.withShrinks 0 . H.property . hoist runResourceT
