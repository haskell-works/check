module Test.Check.Hedgehog.Core
  ( testPropertyNamed,
    propertyOnce,
  ) where

import Control.Monad.Morph
import Control.Monad.Trans.Resource
import Hedgehog (Property)
import Hedgehog qualified as H
import Test.Check.Core.Types

testPropertyNamed :: String -> String -> Property -> Group
testPropertyNamed name description p =
  GroupOfTest $
    Test
      { name = TestName name
      , description = description
      , test = id @(IO Result) $ do
          result <- H.check p
          pure $ Result
            { outcome = if result then Success else Failure "Property failed"
            }
      }

propertyOnce :: H.PropertyT (ResourceT IO) () -> H.Property
propertyOnce = H.withTests 1 . H.withShrinks 0 . H.property . hoist runResourceT
