module Test.Check.Core.Types
  ( FailureReason(..),
    Outcome(..),
    Plan(..),
    Result(..),
    State(..),
    TestName(..),
    Branch(..),
    Test(..),
    Group(..),
    
    IsGroup(..),
    IsTest(..),

    mkPlan,
    stateIsSuccessful,
    groupBranchNamed,
    namedGroupOf,
  ) where

import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Data.Map (Map)
import Data.Map qualified as Map
import HaskellWorks.Prelude

class IsGroup t where
  toGroup :: t -> Group

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

data Group
  = GroupOfTest Test
  | GroupOfBranch Branch

data Branch = Branch
  { name :: String
  -- , description :: String
  , children :: [Group]
  }

data Test = forall t . IsTest t => Test
  { name :: TestName
  , description :: String
  , test :: t
  }

instance IsTest Test where
  runTest = \case
    Test _ _ test ->
      runTest test

newtype TestName = TestName
  { value :: String
  }
  deriving (Show, Eq, Ord)

data Plan = Plan
  { tests :: Map TestName Test
  , tQueue :: TVar [Test]
  , tState :: TVar (Map TestName State)
  }

data State
  = Queued
  | Running
  | Completed Result
  deriving (Show, Eq)

mkPlan :: [Test] -> IO Plan
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

groupBranchNamed :: String -> [Group] -> Group
groupBranchNamed name children =
  GroupOfBranch $ Branch name children

namedGroupOf :: IsGroup t => String -> t -> IO Group
namedGroupOf name t =
  pure $ groupBranchNamed name [toGroup t]
