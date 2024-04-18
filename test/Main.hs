{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Monad (unless)
import Nimbers
import System.Exit (exitFailure)
import Test.QuickCheck

instance Arbitrary Nimber where
  arbitrary = fromInteger <$> arbitrary

prop_neg :: Nimber -> Bool
prop_neg a = a + a == 0

prop_assoc_add :: Nimber -> Nimber -> Nimber -> Bool
prop_assoc_add a b c = a + (b + c) == (a + b) + c

prop_assoc_mul :: Nimber -> Nimber -> Nimber -> Bool
prop_assoc_mul a b c = a * (b * c) == (a * b) * c

prop_distrib :: Nimber -> Nimber -> Nimber -> Bool
prop_distrib a b c = a * (b + c) == a * b + a * c

prop_recip :: Nimber -> Bool
prop_recip a = a == 0 || a / a == 1

main :: IO ()
main = check prop_neg >> check prop_assoc_add >> check prop_assoc_mul >> check prop_distrib >> check prop_recip
  where
    check prop = do
      result <- verboseCheckResult prop
      unless (isSuccess result) exitFailure
