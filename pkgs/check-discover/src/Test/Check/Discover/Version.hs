module Test.Check.Discover.Version
  ( version
  ) where

import Data.Version (Version(..))

import qualified Paths_check_discover as P

version :: Version
version = P.version
