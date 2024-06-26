{-# HLINT ignore "Evaluate" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad (unless)
import Data.Bits
import Data.Nimber
import Data.Word (Word64)
import System.Exit (exitFailure)
import Test.QuickCheck

instance Arbitrary FiniteNimber where
  arbitrary = fromIntegral @Word64 . getLarge <$> arbitrary

prop_neg :: FiniteNimber -> Bool
prop_neg a = a - a == 0

prop_add_id :: FiniteNimber -> Bool
prop_add_id a = a + 0 == a && 0 + a == a

prop_mult_id :: FiniteNimber -> Bool
prop_mult_id a = a * 1 == a && 1 * a == a

prop_comm_add :: FiniteNimber -> FiniteNimber -> Bool
prop_comm_add a b = a + b == b + a

prop_comm_mult :: FiniteNimber -> FiniteNimber -> Bool
prop_comm_mult a b = a * b == b * a

prop_assoc_add :: FiniteNimber -> FiniteNimber -> FiniteNimber -> Bool
prop_assoc_add a b c = a + (b + c) == (a + b) + c

prop_assoc_mult :: FiniteNimber -> FiniteNimber -> FiniteNimber -> Bool
prop_assoc_mult a b c = a * (b * c) == (a * b) * c

prop_distrib :: FiniteNimber -> FiniteNimber -> FiniteNimber -> Bool
prop_distrib a b c = a * (b + c) == a * b + a * c

prop_recip :: NonZero FiniteNimber -> Bool
prop_recip NonZero {getNonZero = a} = a / a == 1

prop_inv :: FiniteNimber -> Bool
prop_inv a = a == 0 || recip (recip a) == a

prop_sqr :: FiniteNimber -> Bool
prop_sqr a = sqr a == a * a

prop_pow :: NonZero FiniteNimber -> Integer -> Bool
prop_pow NonZero {getNonZero = a} n = pow a n == a ^^ n

prop_pow_zero :: NonNegative Integer -> Bool
prop_pow_zero NonNegative {getNonNegative = n} = 0 `pow` n == if n == 0 then 1 else 0

prop_sqrt :: FiniteNimber -> Bool
prop_sqrt a = sqr (sqrt a) == a

prop_as :: FiniteNimber -> Bool
prop_as a = let x = artinSchreierRoot a in sqr x + x == a && not (x `testBit` 0)

prop_quadratic :: FiniteNimber -> FiniteNimber -> FiniteNimber -> Bool
prop_quadratic p q r =
  let (x, y) = solveQuadratic p q
   in sqr x + p * x + q == 0
        && sqr y + p * y + q == 0
        && (r - x) * (r - y) == sqr r + p * r + q
        && x <= y

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
nimberMult :: Int -> Int -> Int
nimberMult = (!!) . (nimberProdTable !!)

nimberProdTable :: [[Int]]
nimberProdTable = [mult i <$> [0 ..] | i <- [0 ..]]
  where
    mult a b = mex $ [(nimberProdTable !! a' !! b) `nimberAdd` (nimberProdTable !! a !! b') `nimberAdd` (nimberProdTable !! a' !! b') | a' <- [0 .. a - 1], b' <- [0 .. b - 1]]

prop_def_add :: Bool
prop_def_add = and $ do
  i <- [0 .. 15]
  j <- [0 .. 15]
  pure $ fromIntegral @Int @FiniteNimber (nimberAdd i j) == fromIntegral i + fromIntegral j

prop_def_mult :: Bool
prop_def_mult = and $ do
  i <- [0 .. 15]
  j <- [0 .. 15]
  pure $ fromIntegral @Int @FiniteNimber (nimberMult i j) == fromIntegral i * fromIntegral j

pure []

runTests :: IO Bool
runTests = $forAllProperties $ verboseCheckWithResult stdArgs {maxSuccess = 500}

main :: IO ()
main = do
  success <- runTests
  unless success exitFailure
