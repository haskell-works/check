{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use camelCase" -}

module DiscoverTest where

import Hedgehog (Property, property, forAll, (===))

import qualified Hedgehog.Gen        as G
import qualified Hedgehog.Range      as R

------------------------------------------------------------------------------------------------

-- prop_additionCommutative :: Int -> Int -> Bool
-- prop_additionCommutative a b = a + b == b + a

------------------------------------------------------------------------------------------------

-- scprop_sortReverse :: [Int] -> Bool
-- scprop_sortReverse list = sort list == sort (reverse list)

------------------------------------------------------------------------------------------------

hprop_addition :: Property
hprop_addition = property $ do
  a <- forAll $ G.int R.linearBounded
  b <- forAll $ G.int R.linearBounded
  a + b === b + a

------------------------------------------------------------------------------------------------

-- test_multiplication :: [Property]
-- test_multiplication =
--   [ propertyNamed "Multiplication commutes" $ \(a :: Int) (b :: Int) -> a * b == b * a
--   , propertyNamed "One is identity" $ \(a :: Int) -> a == a
--   ]

------------------------------------------------------------------------------------------------

-- test_generateTree :: IO Group
-- test_generateTree = do
--   let input = "Some input"
--   pure $ testNamed input $ pure ()

------------------------------------------------------------------------------------------------

-- test_generateTrees :: IO [Group]
-- test_generateTrees = pure (map (\s -> testCase s $ pure ()) ["First input", "Second input"])

-- ------------------------------------------------------------------------------------------------
-- -- How to simultaneously support tasty-hedgehog <1.2 and ^>1.2 using a custom test

-- newtype Property = Property
--   { unProperty :: H.Property
--   }

-- instance IsGroup Property where
--   tasty info (Property p) = pure $
--     TH.propertyNamed (TD.nameOf info) (fromString (TD.descriptionOf info)) p

-- property :: HasCallStack => H.PropertyT IO () -> Property
-- property = Property . H.property

-- {- HLINT ignore "Avoid reverse" -}
-- check_reverse :: H.Property
-- check_reverse = H.property $ do
--   xs <- H.forAll $ G.list (R.linear 0 100) G.alpha
--   reverse (reverse xs) H.=== xs
