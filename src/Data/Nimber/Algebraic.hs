{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

--   This module implements the set of nimbers below \(\left[\omega^{\omega^\omega}\right]\), which are the smallest algebraically closed field of characteristic two.
--   See /On the algebraic closure of two/ by H. W. Lenstra, Jr: <https://www.sciencedirect.com/science/article/pii/1385725877900531>.
module Data.Nimber.Algebraic
  (
  )
where

import Data.Nimber.Finite
import Numeric.Natural

data AlgebraicNimber where
