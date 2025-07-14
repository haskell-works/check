module Test.Check.Discover.TastyInfo
  ( TastyInfo(..)
  ) where

import Data.Monoid
import GHC.Generics (Generic)

data TastyInfo = TastyInfo
  { name        :: Last String
  , description :: Last String
  } deriving stock (Eq, Show, Generic)

instance Semigroup TastyInfo where
  a <> b = TastyInfo
    { name        = a.name        <> b.name
    , description = a.description <> b.description
    }

instance Monoid TastyInfo where
  mempty = TastyInfo
    { name        = Last Nothing
    , description = Last Nothing
    }
