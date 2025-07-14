module Veritas.Types
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
    stateIsSuccessful,
  ) where

import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Data.Map (Map)
import Data.Map qualified as Map
import HaskellWorks.Prelude

class IsTest t where
  runTest :: t -> IO Result

instance IsTest (IO Bool) where
  runTest action = do
    outcome <- action
    pure $ Result
      { outcome = if outcome then Success else Failure "Test failed"
      }

instance IsTest (IO Result) where
  runTest = id

data FailureReason
  = TestFailed
  | TestThrewException SomeException

data Outcome
  = Success
  | Failure String
  deriving (Show, Eq)

newtype Result = Result
  { outcome :: Outcome
  }
  deriving (Show, Eq)

data VeriTree
  = VeriTreeTest VeriTest
  | VeriTreeGroup VeriGroup

data VeriGroup = VeriGroup
  { name :: String
  , description :: String
  , children :: [VeriTree]
  }

data VeriTest = forall t . IsTest t => VeriTest
  { name :: TestName
  , description :: String
  , test :: t
  }

instance IsTest VeriTest where
  runTest = \case
    VeriTest _ _ test ->
      runTest test

newtype TestName = TestName
  { value :: String
  }
  deriving (Show, Eq, Ord)

data Plan = Plan
  { tests :: Map TestName VeriTest
  , tQueue :: TVar [VeriTest]
  , tState :: TVar (Map TestName State)
  }

data State
  = Queued
  | Running
  | Completed Result
  deriving (Show, Eq)

mkPlan :: [VeriTest] -> IO Plan
mkPlan tests = do
  let initialState = Map.fromList [(test.name, Queued) | test <- tests]
  tQueue <- STM.newTVarIO tests
  tState <- STM.newTVarIO initialState
  pure $ Plan
    { tests = Map.fromList [(test.name, test) | test <- tests]
    , tQueue = tQueue
    , tState = tState
    }

stateIsSuccessful :: State -> Bool
stateIsSuccessful = \case
  Completed (Result Success) -> True
  _ -> False
