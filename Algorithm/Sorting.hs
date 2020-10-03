{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Algorithm.Sorting
  ( insertionSort
  ) where

import Prelude (($), Ord(..), otherwise)
import Data.Type.Equality
import GHC.TypeLits as G

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
