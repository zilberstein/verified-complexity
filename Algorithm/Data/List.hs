{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Algorithm.Data.List
  ( List(..)
  , cons, length, map, append
  ) where

import qualified Prelude
import Prelude (Num(..), Int, ($))
import GHC.TypeLits as G

import Algorithm.Complexity

-- | A length-index list type
data List (n :: Nat) a where
  Nil :: List 0 a
  Cons :: a -> List n a -> List (1 + n) a

deriving instance Prelude.Show a => Prelude.Show (List n a)

-- | Constant time function for appending an element to the head of a list
cons :: a -> List n a -> O(1) (List (1 + n) a)
cons x xs = return $ Cons x xs

length :: List n a -> O(n) Int
length Nil = emptyCase 0
length (Cons _ xs) = do
  l <- length xs
  return $ l + 1

map :: (a -> O(t) b) -> List n a -> O(n G.* t) (List n b)
map _ Nil = emptyCase Nil
map f (Cons x xs) = Cons <$> f x <*> map f xs

-- | Appending two lists is linear time in the length of the first list
append :: List n a -> List m a -> O(n) (List (n + m) a)
append Nil ys = emptyCase ys
append (Cons x xs) ys = do
  tl <- append xs ys
  cons x tl

