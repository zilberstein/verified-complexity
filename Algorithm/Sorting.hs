{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Algorithm.Sorting
  ( insertionSort
  , quicksort
  , mergesort
  ) where

import qualified Prelude
import Prelude (($), Ord(..), otherwise)
import GHC.TypeLits as G
import Unsafe.Coerce

import Algorithm.Complexity
import Algorithm.Data.List

-- Insertion Sort --------------------------------------------------------------

-- | Insert an element into a sorted list
insertSorted :: Ord a => a -> List n a -> O(n + 1) (List (n + 1) a)
insertSorted x Nil = cons x Nil
insertSorted x l@(Cons y ys)
  | x <= y = roundUp $ cons x l
  | otherwise = do
      xs <- insertSorted x ys
      cons y xs

-- | Sort a list in O(n^2) time
insertionSort :: Ord a => List n a -> O(n ^ 2) (List n a)
insertionSort Nil = emptyCase Nil
insertionSort (Cons x xs) = roundUp $ do
  ys <- insertionSort xs
  insertSorted x ys

-- Quicksort (Worst Case Runtime) ----------------------------------------------

-- | Two lists of unknown lengths whose total lengths sum to n
data SplitList n a =
  forall l1 l2. ((l1 + l2) ~ n) => SplitList (List l1 a) (List l2 a)

-- | Given an element, partition a list into the elements less than and greater
-- than that element in O(n) time
partition :: Ord a => a -> List n a -> O(n) (SplitList n a)
partition _ Nil = emptyCase $ SplitList Nil Nil
partition x (Cons y ys) = do
  res <- partition x ys
  return $ case res of
    SplitList l1 l2
      | y <= x    -> SplitList (Cons y l1) l2
      | otherwise -> SplitList l1 (Cons y l2)

-- | Sort a list in O(n^2) time (although the average case is faster)
quicksort :: forall n a. Ord a => List n a -> O(n ^ 2) (List n a)
quicksort Nil = emptyCase Nil
quicksort (Cons x xs) = roundUp $ do
  res <- partition x xs
  (case res of
     SplitList l1 l2 -> roundUp $ do
       s1 <- quicksort l1
       s2 <- quicksort l2
       s1 `append` Cons x s2
    ) :: O(n^2 - n) (List n a)

-- Merge Sort ------------------------------------------------------------------

-- | A proof that two numbers are approximately equal
data ApproxEq (n :: Nat) (m :: Nat) where
  Equal :: ApproxEq n n
  OffByOne :: ApproxEq (n+1) n
deriving instance Prelude.Show (ApproxEq n m)

-- | Two lists whose lengths sum to n that are approximately equal in length
data HalfList n a =
  forall l1 l2. (l1 + l2) ~ n => HalfList (ApproxEq l1 l2) (List l1 a) (List l2 a)
deriving instance Prelude.Show a => Prelude.Show (HalfList n a)

ae_plus1 :: ApproxEq n m -> ApproxEq (n + 1) (m + 1)
ae_plus1 Equal = Equal
ae_plus1 OffByOne = OffByOne

-- | Split a list into two sections that are approximately equal in length
split :: List n a -> O(n) (HalfList n a)
split Nil = emptyCase $ HalfList Equal Nil Nil
split (Cons x Nil) = return $ HalfList OffByOne (Cons x Nil) Nil
split (Cons x1 (Cons x2 xs)) = do
  res <- split xs
  case res of
    HalfList pf l1 l2 ->
      HalfList (ae_plus1 pf) <$> cons x1 l1 <*> cons x2 l2

-- | Merge two sorted lists
merge :: Ord a => List n a -> List m a -> O(n + m) (List (n + m) a)
merge Nil ys = roundUp $ emptyCase ys
merge xs Nil = roundUp $ emptyCase xs
merge (Cons x xs) (Cons y ys)
  | x < y = do
      tl <- merge xs (Cons y ys)
      cons x tl
  | otherwise = do
      tl <- merge (Cons x xs) ys
      cons y tl

-- | Sort a list in O(n log n) time
mergesort :: forall n a. Ord a => List n a -> O(n G.* Log2 n) (List n a)
mergesort Nil = emptyCase Nil
mergesort l@(Cons _ Nil) = emptyCase l
mergesort l = do
  sublists <- split l
  (case sublists of
    -- TODO: Prove the runtime is correct
    HalfList _ l1 l2 -> unsafeCoerce $ do
      s1 <- mergesort l1
      s2 <- mergesort l2
      merge s1 s2
    ) :: O(n G.* Log2 n - n) (List n a)
