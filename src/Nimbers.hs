{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | In combinatorial game theory, nimbers represent the values of impartial games.  They are the simplest way of making the ordinals into a Field.
--   See /On Numbers and Games/ by John Conway.
--
--   Nimber addition is defined by \(\alpha+\beta = \operatorname{mex}\{\alpha'+\beta, \alpha+\beta'\}\), where \(\operatorname{mex} S\) is the smallest ordinal not in \(S\).
--
--   Nimber multiplication is defined by \(\alpha\cdot\beta = \operatorname{mex}\{\alpha'\cdot\beta + \alpha\cdot\beta' - \alpha'\cdot\beta'\}\).
--
--   This module implements /finite/ nimbers.  The set of finite nimbers is the quadratic closure of the field with two elements.
module Nimbers where

import Control.Monad
import Data.Bits
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as M

type Natural = Integer

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

-- | Finite nimber addition is calculated as follows: the nimber sum of a two-power and itself is 0, while the nimber sum of two distinct two-powers is their ordinary sum.
--
--   Finite nimber multiplication is calculated as follows: the nimber square of a Fermat two-power is its sesquimultiple, while the nimber product of two distinct Fermat two-powers is their ordinary product.
--   The sesquimultiple of a Fermat two-power is equal to itself plus the product of all smaller Fermat two-powers.
--
--  @'abs'@ and @'signum'@ don't really make sense for nimbers.  They are defined as @'id'@ here.
instance Num Nimber where
  fromInteger = Nimber . S.map twoPowers . twoPowers . abs . fromIntegral
  Nimber a + Nimber b = Nimber $ (a S.\\ b) `S.union` (b S.\\ a)
  (-) = (+)
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

instance Fractional Nimber where
  fromRational _ = error "Cannot map from field of characteristic 0 to characteristic 2"
  recip 0 = error "Divide by zero"
  recip n@Nimber {getNimber = s} =
    let m = 1 + foldl max 0 (S.unions s) -- 2^2^m is the order of the smallest field containing n
        sw i j u v = do
          a <- M.read v i
          b <- M.read v j
          if b `testBit` (2 ^ m - 1 - i)
            then do M.write v i b
                    M.write v j a
                    a' <- M.read u i
                    b' <- M.read u j
                    M.write u i b'
                    M.write u j a'
            else sw i (j + 1) u v
        pivot :: (Bits a, M.PrimMonad m) => M.MVector (M.PrimState m) a -> M.MVector (M.PrimState m) a -> Int -> m ()
        pivot u v i = do sw i i u v
                         p <- M.read u i
                         q <- M.read v i
                         forM_ [i+1..2^m-1] $ \j -> do a <- M.read v j
                                                       when (a `testBit` (2^m-1-i)) $ M.modify v (xor q) j >> M.modify u (xor p) j
        r = V.create $
          do
            tab1 <- M.generate (2 ^ m) (2 ^) -- All two-powers less than 2^2^m
            tab2 <- M.generate (2 ^ m) (nimberToNatural . (n *) . fromIntegral . (2 ^)) -- n times all two-powers less than 2^2^m
            forM_ [0..2^m-1] $ pivot tab1 tab2
            pure tab1
     in fromIntegral $ r V.! (2 ^ m - 1)

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
