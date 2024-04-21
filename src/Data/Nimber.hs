{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | In combinatorial game theory, nimbers represent the values of impartial games.  They are the simplest way of making the ordinals into a Field.
--   See /On Numbers and Games/ by John Conway.
--
--   Nimber addition is defined by \(\alpha+\beta = \operatorname{mex}\{\alpha'+\beta, \alpha+\beta'\}\), where \(\operatorname{mex} S\) is the smallest ordinal not in \(S\).
--
--   Nimber multiplication is defined by \(\alpha\cdot\beta = \operatorname{mex}\{\alpha'\cdot\beta + \alpha\cdot\beta' - \alpha'\cdot\beta'\}\).
--
--   This module implements /finite/ nimbers.  The set of finite nimbers is the quadratic closure of the field with two elements.
module Data.Nimber where

import Data.Bits

type Natural = Integer

newtype Nimber = Nimber {getNimber :: Natural}
  deriving newtype (Show, Eq, Ord, Enum, Bits)

twoPowers :: (Num a, Bits a, Integral b) => a -> [b]
twoPowers 0 = []
twoPowers m =
  (if m `testBit` 0 then (0 :) else id) . fmap (+ 1) $ twoPowers (m `shiftR` 1)

floorLog :: (Num a, Bits a, Num b) => a -> b
floorLog 0 = error "Logarithm of 0"
floorLog 1 = 0
floorLog n = 1 + floorLog (n `shiftR` 1)

-- | Finite nimber addition is calculated as follows: the nimber sum of a two-power and itself is 0, while the nimber sum of two distinct two-powers is their ordinary sum.
--
--   Finite nimber multiplication is calculated as follows: the nimber square of a Fermat two-power is its sesquimultiple, while the nimber product of two distinct Fermat two-powers is their ordinary product.
--   The sesquimultiple of a Fermat two-power is equal to itself plus the product of all smaller Fermat two-powers.
--
--  @'abs'@ and @'signum'@ don't really make sense for nimbers.  They are defined as @'id'@ here.
instance Num Nimber where
  fromInteger = Nimber . fromIntegral . abs
  (+) = xor
  (-) = (+)
  0 * _ = 0
  _ * 0 = 0
  1*b = b
  a*1 = a
  a * b =
    let m = max (floorLog @Int (floorLog a)) (floorLog @Int (floorLog b)) -- D = 2^2^m is the largest Fermat 2-power less than or equal to both a and b
        semiD = bit (bit m - 1) -- semimultiple of D
        a1 = a `shiftR` bit m -- a = a1D+a2
        a2 = a .^. (a1 `shiftL` bit m)
        b1 = b `shiftR` bit m -- b = b1D+b2
        b2 = b .^. (b1 `shiftL` bit m)
     in ((a1*b1 + a2*b1 + a1*b2) `shiftL` bit m) + a1*b1*semiD + a2*b2
  negate = id
  abs = id
  signum = id

-- | The finite nimbers are a field of characteristic 2.  There is no field homomorphism from the rationals to the nimbers, so @'fromRational'@ is always an error.
instance Fractional Nimber where
  fromRational _ = error "Cannot map from field of characteristic 0 to characteristic 2"
  recip 0 = error "Divide by zero"
  recip 1 = 1
  recip n =
    let m = floorLog @Int $ floorLog n -- D = 2^2^m is the largest Fermat 2-power less than or equal to n
        a = n `shiftR` bit m -- n = aD+b
        aD = a `shiftL` bit m
        b = n .^. aD
        semiD = bit (bit m - 1) -- semimultiple of D
     in (aD + a + b) / (semiD * a ^ 2 + b * (a + b))
