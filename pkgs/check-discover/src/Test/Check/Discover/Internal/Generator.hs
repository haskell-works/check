-- | The test generator boilerplate module.
--
-- Any test that is supported (HUnit, HSpec, etc.) provides here, a
-- generator type with all the context necessary for outputting the
-- necessary boilerplate for the generated main function that will
-- run all the tests.

module Test.Check.Discover.Internal.Generator
  ( -- * Types
    Generator (..)
  , Test (..)

    -- * Generators
  , generators
  , getGenerator
  , getGenerators

    -- * Boilerplate Formatter
  , showSetup

    -- * Type Constructor
  , mkTest
  ) where

import Data.Function   (on)
import Data.List       (find, groupBy, isPrefixOf, sortOn)
import Data.Maybe      (fromJust)
import GHC.Generics    (Generic)
import System.FilePath (dropExtension, isPathSeparator)

-- | The test type.
data Test = Test
  { testModule   :: String -- ^ Module name.
  , testFunction :: String -- ^ Function name.
  } deriving stock (Eq, Show, Generic, Ord)

-- | 'Test' constructor.
mkTest :: FilePath -> String -> Test
mkTest = Test . replacePathSepTo '.' . dropExtension
  where replacePathSepTo c1 = map $ \c2 -> if isPathSeparator c2 then c1 else c2

-- | The generator type.
data Generator = Generator
  { generatorPrefix   :: String          -- ^ Generator prefix.
  , generatorImports  :: [String]        -- ^ Module import path.
  , generatorClass    :: String          -- ^ Generator class.
  , generatorSetup    :: Test -> String  -- ^ Generator setup.
  } deriving stock (Generic)

-- | Module import qualifier.
qualifyFunction :: Test -> String
qualifyFunction t = t.testModule ++ "." ++ t.testFunction

-- | Function namer.
name :: Test -> String
name = chooser '_' ' ' . tail . dropWhile (/= '_') . (.testFunction)
  where chooser c1 c2 = map $ \c3 -> if c3 == c1 then c2 else c3

-- | Generator retriever (single).
getGenerator :: Test -> Generator
getGenerator t = fromJust $ getPrefix generators
  where getPrefix = find ((`isPrefixOf` t.testFunction) . (.generatorPrefix))

-- | Generator retriever (many).
getGenerators :: [Test] -> [Generator]
getGenerators =
  map head .
  groupBy  ((==) `on` (.generatorPrefix)) .
  sortOn (.generatorPrefix) .
  map getGenerator

-- | Boilerplate formatter.
showSetup :: Test -> String -> String
showSetup t var = "  " ++ var ++ " <- " ++ setup ++ "\n"
  where gen = getGenerator t
        setup = gen.generatorSetup t

-- | All types of tests supported for boilerplate generation.
generators :: [Generator]
generators =
  [ smallCheckPropertyGenerator
  , hedgehogPropertyGenerator
  , hunitTestCaseGenerator
  , hspecTestCaseGenerator
  , tastyTestGroupGenerator
  , tastyGenerator
  ]

-- | Quickcheck group generator prefix.
hedgehogPropertyGenerator :: Generator
hedgehogPropertyGenerator = Generator
  { generatorPrefix   = "hprop_"
  , generatorImports  = ["import qualified Test.Check.Hedgehog as H", "import Data.String (fromString)"]
  , generatorClass    = ""
  , generatorSetup    = \t -> "pure $ H.testPropertyNamed \"" ++ name t ++ "\" (fromString \"" ++ qualifyFunction t ++ "\") " ++ qualifyFunction t
  }

-- | Smallcheck group generator prefix.
smallCheckPropertyGenerator :: Generator
smallCheckPropertyGenerator = Generator
  { generatorPrefix   = "scprop_"
  , generatorImports  = ["import qualified Test.Check.SmallCheck as SC"]
  , generatorClass    = ""
  , generatorSetup    = \t -> "pure $ SC.testProperty \"" ++ name t ++ "\" " ++ qualifyFunction t
  }

-- | HUnit generator prefix.
hunitTestCaseGenerator :: Generator
hunitTestCaseGenerator = Generator
  { generatorPrefix   = "unit_"
  , generatorImports  = []
  , generatorClass    = concat
    [ "class TestCase a where testCase :: String -> a -> IO T.Group\n"
    , "instance TestCase (IO ())                      where testCase n = pure . HU.testCase      n\n"
    , "instance TestCase (IO String)                  where testCase n = pure . HU.testCaseInfo  n\n"
    , "instance TestCase ((String -> IO ()) -> IO ()) where testCase n = pure . HU.testCaseSteps n\n"
    ]
  , generatorSetup  = \t -> "testCase \"" ++ name t ++ "\" " ++ qualifyFunction t
  }

-- | Hspec generator prefix.
hspecTestCaseGenerator :: Generator
hspecTestCaseGenerator = Generator
  { generatorPrefix   = "spec_"
  , generatorImports  = ["import qualified Test.Check.Hspec as HS"]
  , generatorClass    = ""
  , generatorSetup    = \t -> "HS.testSpec \"" ++ name t ++ "\" " ++ qualifyFunction t
  }

-- | Tasty group generator prefix.
tastyTestGroupGenerator :: Generator
tastyTestGroupGenerator = Generator
  { generatorPrefix   = "test_"
  , generatorImports  = ["import qualified Test.Check.Hedgehog ()"]
  , generatorClass    = mempty
  , generatorSetup  = \t -> "T.namedGroupOf \"" ++ name t ++ "\" " ++ qualifyFunction t
  }

-- | Tasty group generator prefix.
tastyGenerator :: Generator
tastyGenerator = Generator
  { generatorPrefix   = "check_"
  , generatorImports  = ["import qualified Test.Check.Discover as TD"]
  , generatorClass    = []
  , generatorSetup    = \t -> "TD.tasty (TD.description \"" ++ name t ++ "\" <> TD.name \"" ++ qualifyFunction t ++ "\") " ++ qualifyFunction t
  }
