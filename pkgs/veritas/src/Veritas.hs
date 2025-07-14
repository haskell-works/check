{-# LANGUAGE LambdaCase #-}
module Veritas
  ( FailureReason(..),
    Outcome(..),
    Plan(..),
    Result(..),
    State(..),
    TestName(..),
    VeriGroup(..),
    VeriTest(..),
    VeriTree(..),
    
    IsTest(..),

    mkPlan,
    enumerate,
    runTree,
    runTests,
    runPlan,
    checkResults,
    reportResults,
  ) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.STM qualified as STM
import Control.Monad ( forM_ )
import Data.Map qualified as Map
import System.Exit (ExitCode(..))
import UnliftIO.Async (pooledReplicateConcurrently_)
import Veritas.Types

enumerate :: VeriTree -> [VeriTest]
enumerate = \case
  VeriTreeTest test -> [test]
  VeriTreeGroup group -> mconcat $ map enumerate group.children

dequeueTest :: Plan -> IO (Maybe VeriTest)
dequeueTest plan = STM.atomically $ do
    tests <- STM.readTVar plan.tQueue
    state <- STM.readTVar plan.tState
    case tests of
      [] -> pure Nothing
      (t:ts) -> do
        case Map.lookup t.name state of
          Just Queued -> do
            STM.writeTVar plan.tQueue ts
            STM.writeTVar plan.tState (Map.insert t.name Running state)
            pure $ Just t
          Just other ->
            error $ "Unexpected state for test " ++ show t.name ++ ": " ++ show other
          Nothing ->
            error $ "Test " ++ show t.name ++ " not found in state"

completeTest :: Plan -> VeriTest -> Result -> IO ()
completeTest plan test result = do
  STM.atomically $ do
    oldState <- STM.readTVar plan.tState
    case Map.lookup test.name oldState of
      Just Running -> do
        let newState = Map.insert test.name (Completed result) oldState
        STM.writeTVar plan.tState newState
      Just other -> do
        error $ "Unexpected state for test " ++ show test.name ++ ": " ++ show other ++ ". Plan state: " ++ show oldState
      Nothing ->
        error $ "Test " ++ show test.name ++ " not found in state"

runWorker :: Plan -> IO ()
runWorker plan = do
  mTest <- dequeueTest plan

  forM_ mTest $ \test -> do
    result <- runTest test
    completeTest plan test result
    runWorker plan

runTree :: VeriTree -> IO ()
runTree tree = do
  plan <- mkPlan $ enumerate tree

  runPlan plan

runTests :: [VeriTest] -> IO ()
runTests tests = do
  plan <- mkPlan tests

  runPlan plan

runPlan :: Plan -> IO ()
runPlan plan = do
  numCaps <- getNumCapabilities
  pooledReplicateConcurrently_ numCaps $ runWorker plan

checkResults :: Plan -> IO ExitCode
checkResults plan = do
  state <- STM.atomically $ STM.readTVar plan.tState
  let results = Map.elems state
  if all stateIsSuccessful results
    then pure ExitSuccess
    else pure $ ExitFailure 1

reportResults :: Plan -> IO ()
reportResults plan = do
  planState <- STM.atomically $ STM.readTVar plan.tState
  queue <- STM.readTVarIO plan.tQueue
  let namedResults = Map.toList planState
  let results = fmap snd namedResults
  putStrLn $ "Total tests: " ++ show (length results)
  putStrLn $ "Successful tests: " ++ show (length $ filter stateIsSuccessful results)
  putStrLn $ "Failed tests: " ++ show (length $ filter (not . stateIsSuccessful) results)
  putStrLn $ "Queued tests: " ++ show (length $ filter (== Queued) results)
  putStrLn $ "Running tests: " ++ show (length $ filter (== Running) results)
  putStrLn $ "Completed tests: " ++ show (length $ filter stateIsSuccessful results)
  putStrLn $ "Total tests in plan: " ++ show (Map.size plan.tests)
  putStrLn $ "Tests in queue: " ++ show (length queue)
  putStrLn "Results report completed."