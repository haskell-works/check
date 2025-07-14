{-# LANGUAGE FlexibleInstances #-}

module Test.Check.Discover
  ( TastyInfo
  , name
  , description
  , nameOf
  , descriptionOf
  ) where

import Data.Maybe
import Data.Monoid
import Test.Check.Discover.TastyInfo (TastyInfo)

import qualified Test.Check.Discover.TastyInfo as TI

nameOf :: TastyInfo -> String
nameOf info =
  fromMaybe "<unnamed>" (getLast info.name)

descriptionOf :: TastyInfo -> String
descriptionOf info =
  fromMaybe "<undescribed>" (getLast info.description)

name :: String -> TastyInfo
name n = mempty
  { TI.name = Last $ Just n
  }

description :: String -> TastyInfo
description n = mempty
  { TI.description = Last $ Just n
  }
