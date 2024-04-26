{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | In combinatorial game theory, nimbers represent the values of impartial games.  They are the simplest way of making the ordinals into a Field.
--   See /On Numbers and Games/ by John Conway.
--
--   Nimber addition is defined by \(\alpha+\beta = \operatorname{mex}\{\alpha'+\beta, \alpha+\beta'\}\), where \(\operatorname{mex} S\) is the smallest ordinal not in \(S\).
--
--   Nimber multiplication is defined by \(\alpha\cdot\beta = \operatorname{mex}\{\alpha'\cdot\beta + \alpha\cdot\beta' - \alpha'\cdot\beta'\}\).
--
--   This module implements /finite/ nimbers, which form the smallest quadratically closed field of characteristic 2.
module Data.Nimber
  ( Nimber (..),
    floorLog,
    sqr,
    pow,
    artinSchreierRoot,
    solveQuadratic,
  )
where

import Data.Bits
import Numeric.Natural

newtype Nimber = Nimber {getNimber :: Natural}
  deriving newtype (Show, Eq, Ord, Enum, Bits)

-- | Index of highest-order set bit, or -1 if there are none.
floorLog :: (Bits a, Num b) => a -> b
floorLog n
  | n == zeroBits = -1
  | otherwise = 1 + floorLog (n .>>. 1)

mult' :: Int -> Nimber -> Nimber -> Nimber
mult' _ 0 _ = 0
mult' _ _ 0 = 0
mult' _ 1 b = b
mult' _ a 1 = a
mult' m a b =
  let semiD = bit (bit m - 1) -- semimultiple of D
      s1 = a .>>. bit m -- a = a1D+a2
      s2 = a .^. (s1 .<<. bit m)
      t1 = b .>>. bit m -- b = b1D+b2
      t2 = b .^. (t1 .<<. bit m)
      c = mult' (m - 1) s2 t2
   in ((mult' (m - 1) (s1 + s2) (t1 + t2) - c) .<<. bit m) + mult' (m - 1) (mult' (m - 1) s1 t1) semiD + c

-- | Finite nimber addition is calculated as follows: the nimber sum of a two-power and itself is 0, while the nimber sum of a set of distinct two-powers is their ordinary sum.
--
--   Finite nimber multiplication is calculated as follows: the nimber square of a Fermat two-power is its sesquimultiple, while the nimber product of a set of distinct Fermat two-powers is their ordinary product.
--   The sesquimultiple of a Fermat two-power is equal to itself plus the product of all smaller Fermat two-powers.
instance Num Nimber where
  fromInteger = Nimber . fromIntegral . abs
  (+) = xor
  (-) = xor
  a * b =
    let m = max (floorLog @Int (floorLog a)) (floorLog @Int (floorLog b)) -- D = 2^2^m is the largest Fermat 2-power less than or equal to both a and b
     in mult' m a b
  negate = id
  abs = id
  signum 0 = 0
  signum _ = 1

sqr' :: Int -> Nimber -> Nimber
sqr' _ 0 = 0
sqr' _ 1 = 1
sqr' m n =
  let a = n .>>. bit m -- n = aD+b
      aD = a .<<. bit m
      b = n .^. aD
      semiD = bit (bit m - 1) -- semimultiple of D
      a2 = sqr' (m - 1) a -- a^2
   in a2 .<<. bit m + mult' (m - 1) a2 semiD + sqr' (m - 1) b

-- | Squaring function.  Faster than multiplying @n@ by itself.
sqr :: Nimber -> Nimber
sqr n =
  let m = floorLog @Int $ floorLog n -- D = 2^2^m is the largest Fermat 2-power less than or equal to n
   in sqr' m n

-- | Raise a @'Nimber'@ to an integral power.  Faster than using '^' or '^^'.
pow :: (Integral a, Bits a) => Nimber -> a -> Nimber
x `pow` n
  | n < 0 = recip x `pow` negate n
  | otherwise =
      let m = floorLog @Int $ floorLog x
       in product . fmap snd . filter (testBit n . fst) . zip [0 ..] . take (1 + floorLog (n + 1)) $ iterate (sqr' m) x

-- | The finite nimbers are a field of characteristic 2.  There is no field homomorphism from the rationals to the nimbers, so @'fromRational'@ is always an error.
instance Fractional Nimber where
  fromRational _ = error "Cannot map from field of characteristic 0 to characteristic 2"
  recip n =
    let m = floorLog @Int $ floorLog n -- D = 2^2^m is the largest Fermat 2-power less than or equal to n
        recip' _ 0 = error "Divide by zero"
        recip' _ 1 = 1
        recip' k l =
          let a = l .>>. bit k -- n = aD+b
              aD = a .<<. bit k
              b = l .^. aD
              semiD = bit (bit k - 1) -- semimultiple of D
           in mult' k (l + a) $ recip' (k - 1) (mult' (k - 1) semiD (sqr' (k - 1) a) + mult' (k - 1) b (a + b))
     in recip' m n

-- | The only reason this instance exists is to define square roots.  None of the other @'Floating'@ methods apply to @'Nimber'@s.
instance Floating Nimber where
  sqrt n =
    let m = floorLog @Int $ floorLog n -- D = 2^2^m is the largest Fermat 2-power less than or equal to n
        sqrt' _ 0 = 0
        sqrt' _ 1 = 1
        sqrt' k l =
          let a = l .>>. bit k -- n = aD+b
              aD = a .<<. bit k
              b = l .^. aD
              semiD = bit $ bit k - 1 -- semimultiple of D
              sqrta = sqrt' (k - 1) a
           in sqrta .<<. bit k + mult' (k - 1) sqrta (sqrt' (k - 1) semiD) + sqrt' (k - 1) b
     in sqrt' m n
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

evens :: [a] -> [a]
evens (x : _ : xs) = x : evens xs
evens xs = xs

-- | @'artinSchreierRoot' n@ is the smallest solution to the equation \(x^2 - x = n\).
--   The algorithm is due to Chin-Long Chen: <https://ieeexplore.ieee.org/document/1056557>.
--   In fields of characteristic 2, the standard quadratic formula does not work, but any quadratic equation can be solved using square roots and Artin-Schreier roots.
--
--   This function is __much__ slower than @'sqrt'@.
artinSchreierRoot :: Nimber -> Nimber
artinSchreierRoot 0 = 0
artinSchreierRoot 1 = 2
artinSchreierRoot 2 = 4
artinSchreierRoot 3 = 6
artinSchreierRoot n =
  let m = 1 + floorLog @Int (floorLog n) -- 2^2^m is the order of the smallest field containing n
      m' = if n < bit (bit m - 1) then m else m + 1 -- 2^2^m' is the order of the smallest field containing the Artin-Schreier root of n
      squares = iterate (sqr' m) n
      quarts = evens squares
      semiTrace = sum $ take (bit (m' - 1)) quarts -- trace of the Artin-Screier root of n
   in if semiTrace == 1
        then
          let s = sum $ do
                j <- [1 .. bit (m' - 2) - 1]
                i <- [j .. bit (m' - 2) - 1]
                pure $ mult' m' (squares !! (shiftL i 1 - 1 .^. bit (m' - 1))) (squares !! (shiftL j 1 - 2))
           in flip clearBit 0 $
                s
                  + sqr s
                  + (squares !! (bit m' - 1))
                    * (1 + sum (take (bit (m' - 2)) $ drop (bit (m' - 2)) quarts))
        else
          let y = bit $ bit m' - 1
              z = artinSchreierRoot $ sqr y + y + n
           in y + z

-- | @'solveQuadratic' p q@ returns the solutions to the equation \(X^2 + px + q = 0\).
solveQuadratic :: Nimber -> Nimber -> (Nimber, Nimber)
solveQuadratic 0 q = (sqrt q, sqrt q)
solveQuadratic p q = let x = p * artinSchreierRoot (q / sqr p) in (min x $ x + p, max x $ x + p)
