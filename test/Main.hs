{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad (unless)
import Data.Nimber
import System.Exit (exitFailure)
import Test.QuickCheck
import Data.Word (Word64)

instance Arbitrary Nimber where
  arbitrary = fromIntegral @Word64 . getLarge <$> arbitrary

prop_neg :: Nimber -> Bool
prop_neg a = a - a == 0

prop_add_id :: Nimber -> Bool
prop_add_id a = (a + 0) == a && (0 + a == a)

prop_mult_id :: Nimber -> Bool
prop_mult_id a = (a * 1) == a && (1 * a == a)

prop_assoc_add :: Nimber -> Nimber -> Nimber -> Bool
prop_assoc_add a b c = a + (b + c) == (a + b) + c

prop_assoc_mul :: Nimber -> Nimber -> Nimber -> Bool
prop_assoc_mul a b c = a * (b * c) == (a * b) * c

prop_distrib :: Nimber -> Nimber -> Nimber -> Bool
prop_distrib a b c = a * (b + c) == a * b + a * c

prop_recip :: Nimber -> Bool
prop_recip a = a == 0 || a / a == 1

prop_inv :: Nimber -> Bool
prop_inv a = a == 0 || recip (recip a) == a

mex :: (Functor f, Foldable f, Eq b, Num b, Num a) => f b -> a
mex s = if 0 `notElem` s then 0 else 1 + mex (fmap (+ (-1)) s)

-- | Compute nimber sum directly from the definition.  This is very slow.
nimberAdd :: Int -> Int -> Int
nimberAdd = (!!) . (nimberSumTable !!)

nimberSumTable :: [[Int]]
nimberSumTable = fmap add <$> [(i,) <$> [0 ..] | i <- [0 ..]]
  where
    add (a, b) = mex $ [nimberSumTable !! a' !! b | a' <- [0 .. a - 1]] ++ [nimberSumTable !! a !! b' | b' <- [0 .. b - 1]]

-- | Compute nimber product directly from the definition.  This is very slow.
nimberMul :: Int -> Int -> Int
nimberMul = (!!) . (nimberProdTable !!)

nimberProdTable :: [[Int]]
nimberProdTable = fmap mul <$> [(i,) <$> [0 ..] | i <- [0 ..]]
  where
    mul (a, b) = mex $ [(nimberProdTable !! a' !! b) `nimberAdd` (nimberProdTable !! a !! b') `nimberAdd` (nimberProdTable !! a' !! b') | a' <- [0 .. a - 1], b' <- [0 .. b - 1]]

pure []
runTests :: IO Bool
runTests = $forAllProperties $ verboseCheckWithResult stdArgs {maxSuccess = 200}

main :: IO ()
main = do success <- runTests
          unless success exitFailure
