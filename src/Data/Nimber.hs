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
import Numeric.Natural

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
  1 * b = b
  a * 1 = a
  a * b =
    let m = max (floorLog @Int (floorLog a)) (floorLog @Int (floorLog b)) -- D = 2^2^m is the largest Fermat 2-power less than or equal to both a and b
        semiD = bit (bit m - 1) -- semimultiple of D
        a1 = a `shiftR` bit m -- a = a1D+a2
        a2 = a .^. (a1 `shiftL` bit m)
        b1 = b `shiftR` bit m -- b = b1D+b2
        b2 = b .^. (b1 `shiftL` bit m)
        c = a2 * b2
     in (((a1 + a2) * (b1 + b2) - c) `shiftL` bit m) + a1 * b1 * semiD + c
  negate = id
  abs = id
  signum = id

-- | Squaring function.  Faster than multiplying @n@ by itself.
sqr :: Nimber -> Nimber
sqr 0 = 0
sqr 1 = 1
sqr n =
  let m = floorLog @Int $ floorLog n -- D = 2^2^m is the largest Fermat 2-power less than or equal to n
      a = n `shiftR` bit m -- n = aD+b
      aD = a `shiftL` bit m
      b = n .^. aD
      semiD = bit (bit m - 1) -- semimultiple of D
      sqra = sqr a
   in sqra `shiftL` bit m + sqra * semiD + sqr b

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
     in (aD + a + b) / (semiD * sqr a + b * (a + b))

-- | The only reason this instance exists is to define square roots.  None of the other @'Floating'@ methods apply to @'Nimber'@s.
instance Floating Nimber where
  sqrt 0 = 0
  sqrt 1 = 1
  sqrt n =
    let m = floorLog @Int $ floorLog n -- D = 2^2^m is the largest Fermat 2-power less than or equal to n
        a = n `shiftR` bit m -- n = aD+b
        aD = a `shiftL` bit m
        b = n .^. aD
        semiD = bit (bit m - 1) -- semimultiple of D
        sqrta = sqrt a
     in sqrta `shiftL` bit m + sqrta * sqrt semiD + sqrt b
  pi = error "π is not a nimber"
  exp _ = error "exp undefined for nimbers"
  log _ = error "log undefined for nimbers"
  sin _ = error "Trigonometric functions undefined for nimbers"
  cos _ = error "Trigonometric functions undefined for nimbers"
  tan _ = error "Trigonometric functions undefined for nimbers"
  asin _ = error "Trigonometric functions undefined for nimbers"
  acos _ = error "Trigonometric functions undefined for nimbers"
  atan _ = error "Trigonometric functions undefined for nimbers"
  sinh _ = error "Hyperbolic functions undefined for nimbers"
  cosh _ = error "Hyperbolic functions undefined for nimbers"
  tanh _ = error "Hyperbolic functions undefined for nimbers"
  asinh _ = error "Hyperbolic functions undefined for nimbers"
  acosh _ = error "Hyperbolic functions undefined for nimbers"
  atanh _ = error "Hyperbolic functions undefined for nimbers"
