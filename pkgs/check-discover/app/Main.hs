-- | Main executable module.

module Main where

import Control.Monad                       (when)
import Data.Maybe                          (fromMaybe)
import System.Environment                  (getArgs, getProgName)
import System.Exit                         (exitFailure)
import System.FilePath                     (takeDirectory)
import System.IO                           (IOMode(ReadMode), hGetContents, hPutStrLn, withFile, stderr)
import Test.Check.Discover.Internal.Config (Config (..), parseConfig)
import Test.Check.Discover.Internal.Driver (findTests, generateTestDriver)

-- | Main function.
main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    src:_:dst:opts ->
      case parseConfig (takeDirectory src) name opts of
        Left err -> do
          hPutStrLn stderr err
          exitFailure
        Right config -> do
          tests <- findTests config
          let ingredients = config.tastyIngredients
              moduleName  = fromMaybe "Main" config.generatedModuleName
          header <- readHeader src
          let output = generateTestDriver config moduleName ingredients src tests
          when config.debug $ hPutStrLn stderr output
          when config.inPlace $ writeFile src $ unlines $ header ++ [marker, output]
          writeFile dst $
            "{-# LINE " ++ show (length header + 2) ++ " " ++ show src ++ " #-}\n"
            ++ output
    _ -> do
      hPutStrLn stderr "Usage: check-discover src _ dst [OPTION...]"
      exitFailure
  where
    marker = "-- GENERATED BY check-discover"
    readHeader src = withFile src ReadMode $ \h -> do
      header <- takeWhile (marker /=) . lines <$> hGetContents h
      seq (length header) (return header)
