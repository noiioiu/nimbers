-- | In combinatorial game theory, nimbers represent the values of impartial games.  They are the simplest way of making the ordinals into a Field.
--   See /On Numbers and Games/ by John Conway.
--
--   Nimber addition is defined by \(\alpha+\beta = \operatorname{mex}\{\alpha'+\beta, \alpha+\beta'\}\).
--
--   Nimber multiplication is defined by \(\alpha\cdot\beta = \operatorname{mex}\{\alpha'\cdot\beta + \alpha\cdot\beta' - \alpha'\cdot\beta'\}\).
--
--   This module implements /finite/ nimbers.  The set of finite nimbers is the quadratic closure of the field with two elements.
module Nimbers where

import Data.Set qualified as S

type Natural = Int

-- | A finite nimber is represented as a sum of distinct 2-powers, each of which is represented as a product of distinct Fermat 2-powers.
--   Hence @'Nimber' {'getNimber' = s}@ represents \(\sum\limits_{t \in s} \prod\limits_{n \in t} 2^{2^n}\).  This representation makes sums and products easy to calculate.
newtype Nimber = Nimber {getNimber :: S.Set (S.Set Natural)}
  deriving (Show, Eq)

nimberToNatural :: Nimber -> Natural
-- nimberToInteger = sum . S.map ((^) @_ @Integer 2 . sum . S.map (2^)) . getNimber
nimberToNatural = sum . S.map (product . S.map ((^) @_ @Natural 2 . (2 ^))) . getNimber

-- | Nimbers are ordinals, so they are ordered.  They also form a field, but they are not an ordered field.
instance Ord Nimber where
  n `compare` m = nimberToNatural n `compare` nimberToNatural m

instance Enum Nimber where
  toEnum = fromInteger . fromIntegral
  fromEnum = fromIntegral . nimberToNatural

twoPowers :: Natural -> S.Set Natural
twoPowers 0 = S.empty
twoPowers m =
  if even m
    then S.map (+ 1) $ twoPowers (m `div` 2)
    else S.insert 0 . S.map (+ 1) $ twoPowers (m `div` 2)

delta :: (Ord a) => S.Set a -> S.Set a -> S.Set a
delta x y = (x S.\\ y) `S.union` (y S.\\ x)

instance Num Nimber where
  fromInteger = Nimber . S.map twoPowers . twoPowers . abs . fromIntegral

  -- \| Finite nimber addition is calculated as follows: the nimber sum of a two-power and itself is 0, while the nimber sum of two distinct two-powers is their ordinary sum.
  Nimber a + Nimber b = Nimber $ (a S.\\ b) `S.union` (b S.\\ a)
  (-) = (+)

  -- \| Finite nimber multiplication is calculated as follows: the nimber square of a Fermat two-power is its sesquimultiple, while the nimber product of two distinct Fermat two-powers is their ordinary product.
  a * b
    | a == 1 = b
    | b == 1 = a
    | otherwise = sum $ do
        x <- S.toList $ getNimber a
        y <- S.toList $ getNimber b
        let cs = x `S.intersection` y
            p = product $ S.map (\c -> Nimber $ S.fromList [S.singleton c, S.fromList [0 .. c - 1]]) cs
            d = Nimber $ S.singleton $ x `delta` y
        -- (*d) . Nimber . S.singleton <$> S.toList (getNimber p)
        pure $ p * d
  negate = id
  abs = id
  signum = id

mex :: S.Set Int -> Int
mex s = if 0 `notElem` s then 0 else 1 + mex (S.map (+ (-1)) s)

-- | Compute nimber sum directly from the definition.  This is very slow.
nimberAdd :: Int -> Int -> Int
nimberAdd = (!!) . (nimberSumTable !!)

nimberSumTable :: [[Int]]
nimberSumTable = fmap add <$> [(i,) <$> [0 ..] | i <- [0 ..]]
  where
    add (a, b) = mex $ S.fromList [nimberSumTable !! a' !! b | a' <- [0 .. a - 1]] `S.union` S.fromList [nimberSumTable !! a !! b' | b' <- [0 .. b - 1]]

-- | Compute nimber product directly from the definition.  This is very slow.
nimberMul :: Int -> Int -> Int
nimberMul = (!!) . (nimberProdTable !!)

nimberProdTable :: [[Int]]
nimberProdTable = fmap mul <$> [(i,) <$> [0 ..] | i <- [0 ..]]
  where
    mul (a, b) = mex $ S.fromList [(nimberProdTable !! a' !! b) `nimberAdd` (nimberProdTable !! a !! b') `nimberAdd` (nimberProdTable !! a' !! b') | a' <- [0 .. a - 1], b' <- [0 .. b - 1]]
