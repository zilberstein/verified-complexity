{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Algorithm.Sorting
  ( insertionSort
  ) where

import Prelude (($), Ord(..), otherwise)
import GHC.TypeLits

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
