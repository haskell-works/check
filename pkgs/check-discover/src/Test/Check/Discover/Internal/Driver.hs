{-# LANGUAGE CPP #-}

-- | Automatic test discovery and runner for the tasty framework.
module Test.Check.Discover.Internal.Driver
  ( -- * Main Test Generator
    generateTestDriver

    -- * For Testing Purposes Only
  , ModuleTree (..)
  , findTests
  , mkModuleTree
  , showTests
  ) where

import Data.List                              (dropWhileEnd, intercalate, isPrefixOf, nub, sort, stripPrefix)
import Data.Maybe                             (fromMaybe)
import System.FilePath                        (pathSeparator)
import System.FilePath.Glob                   (compile, globDir1, match)
import System.IO                              (IOMode (ReadMode), withFile)
import Test.Check.Discover.Internal.Config    (Config (..), GlobPattern)
import Test.Check.Discover.Internal.Generator (Generator (..), Test (..), generators, getGenerators, mkTest, showSetup)

import qualified Data.Map.Strict as M

#if defined(mingw32_HOST_OS)
import GHC.IO.Encoding.CodePage (mkLocaleEncoding)
import GHC.IO.Encoding.Failure  (CodingFailureMode (TransliterateCodingFailure))
import GHC.IO.Handle            (hGetContents, hSetEncoding)
#else
import GHC.IO.Handle (hGetContents)
#endif

defaultImports :: [String]
defaultImports =
  [ "import Prelude"
  , "import qualified Test.Check.Core as T"
  ]

-- | Main function generator, along with all the boilerplate which
-- which will run the discovered tests.
generateTestDriver :: Config -> String -> [String] -> FilePath -> [Test] -> String
generateTestDriver config modname is src tests =
  let generators' = getGenerators tests
      testNumVars = map (("t"++) . show) [(0 :: Int)..]
      testKindImports = map (.generatorImports) generators' :: [[String]]
      testImports = showImports (map ingredientImport is ++ map (.testModule) tests) :: [String]
  in concat
    [ "{-# LANGUAGE FlexibleInstances #-}\n"
    , "\n"
    , "module " ++ modname ++ " (main, mkTests) where\n"
    , "\n"
    , unlines $ nub $ sort $ mconcat (defaultImports:testKindImports) ++ testImports
    , "\n"
    , "{- HLINT ignore \"Use let\" -}\n"
    , "\n"
    , unlines $ map (.generatorClass) generators'
    , "mkTests :: IO T.Group\n"
    , "mkTests = do\n"
    , unlines $ zipWith showSetup tests testNumVars
    , "  pure $ T.groupBranchNamed " ++ show src ++ " ["
    , intercalate "," $ showTests config tests testNumVars
    , "]\n"
    , "\n"
    , "main :: IO ()\n"
    , "main = do\n"
    , "  mkTests >>= T.runGroup\n"
    ]

-- | Match files by specified glob pattern.
filesByModuleGlob :: FilePath -> Maybe GlobPattern -> IO [String]
filesByModuleGlob directory globPattern = globDir1 pattern directory
  where pattern = compile ("**/" ++ fromMaybe "*.hs*" globPattern)

-- | Filter and remove files by specified glob pattern.
ignoreByModuleGlob :: [FilePath] -> Maybe GlobPattern -> [FilePath]
ignoreByModuleGlob filePaths Nothing = filePaths
ignoreByModuleGlob filePaths (Just ignoreGlob) = filter (not . match pattern) filePaths
  where pattern = compile ("**/" ++ ignoreGlob)

-- | Discover the tests modules.
findTests :: Config -> IO [Test]
findTests config = do
  let directory = config.searchDir
  allModules <- filesByModuleGlob directory config.modules
  let filtered = ignoreByModuleGlob allModules config.ignores
      -- The files to scan need to be sorted or otherwise the output of
      -- findTests might not be deterministic
      sortedFiltered = sort filtered
  concat <$> traverse (extract directory) sortedFiltered
  where extract directory filePath =
          withFile filePath ReadMode $ \h -> do
#if defined(mingw32_HOST_OS)
          -- Avoid internal error: hGetContents: invalid argument (invalid byte sequence)' non UTF-8 Windows
            hSetEncoding h $ mkLocaleEncoding TransliterateCodingFailure
#endif
            tests <- extractTests (dropDirectory directory filePath) <$> hGetContents h
            seq (length tests) (return tests)
        dropDirectory directory filePath = fromMaybe filePath $
          stripPrefix (directory ++ [pathSeparator]) filePath

-- | Extract the test names from discovered modules.
extractTests :: FilePath -> String -> [Test]
extractTests file = mkTestDeDuped . isKnownPrefix . parseTest
  where mkTestDeDuped :: [String] -> [Test]
        mkTestDeDuped = map (mkTest file) . nub

        isKnownPrefix :: [String] -> [String]
        isKnownPrefix = filter (\g -> any (checkPrefix g) generators)

        checkPrefix :: String -> Generator -> Bool
        checkPrefix g = (`isPrefixOf` g) . (.generatorPrefix)

        parseTest :: String -> [String]
        parseTest     = map fst . concatMap lex . lines

-- | Show the imports.
showImports :: [String] -> [String]
showImports mods = sort $ map ("import qualified " ++) mods

-- | Retrieve the ingredient name.
ingredientImport :: String -> String
ingredientImport = init . dropWhileEnd (/= '.')

-- | Show the tests.
showTests :: Config -> [Test] -> [String] -> [String]
showTests config tests testNumVars = if config.treeDisplay
  then showModuleTree $ mkModuleTree tests testNumVars
  else zipWith const testNumVars tests

newtype ModuleTree = ModuleTree (M.Map String (ModuleTree, [String]))
  deriving stock (Eq, Show)

showModuleTree :: ModuleTree -> [String]
showModuleTree (ModuleTree mdls) = map showModule $ M.assocs mdls
  where -- special case, collapse to mdl.submdl
        showModule :: ([Char], (ModuleTree, [String])) -> [Char]
        showModule (mdl, (ModuleTree subMdls, [])) | M.size subMdls == 1 =
          case M.assocs subMdls of
            [(subMdl, (subSubTree, testVars))] -> showModule (mdl ++ '.' : subMdl, (subSubTree, testVars))
            as -> error $ "Excepted number of submodules != 1.  Found " <> show (length as)
        showModule (mdl, (subTree, testVars)) = concat
          [ "T.groupBranchNamed \"", mdl
          , "\" [", intercalate "," (showModuleTree subTree ++ testVars), "]" ]

mkModuleTree :: [Test] -> [String] -> ModuleTree
mkModuleTree tests testVars = ModuleTree $
    foldr go M.empty $ zipWith (\t tVar -> (t.testModule, tVar)) tests testVars
  where go (mdl, tVar) mdls = M.insertWith merge key val mdls
          where (key, val) = case break (== '.') mdl of
                  (_, [])              -> (mdl, (ModuleTree M.empty, [tVar]))
                  (topMdl, '.':subMdl) -> (topMdl, (ModuleTree $ go (subMdl, tVar) M.empty, []))
                  _                    -> error "impossible case in mkModuleTree.go.key"
        merge (ModuleTree mdls1, tVars1) (ModuleTree mdls2, tVars2) =
          (ModuleTree $ M.unionWith merge mdls1 mdls2, tVars1 ++ tVars2)
