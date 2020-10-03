{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Algorithm.Complexity
  ( O
  , Functor(..)
  , Applicative(..)
  , Monad(..)
  , (<$>), emptyCase, roundUp, ifThenElse
  ) where

import qualified Prelude
import GHC.TypeLits

-- | Datatype for expressing the runtime complexity of some computation. The Nat
-- is a phantom type expressing the (asymptotic) amount of time that it took to
-- compute the result of type a. The type is called O in reference to big-O
-- notation which is used to express asymptotic runtimes
newtype O(t :: Nat) a = Result a
  deriving (Prelude.Show)

-- This library redefines functors and monads. Instead of having kind (* -> *), a
-- functor/monad now must have kind (Nat -> * -> *). That is, the computational
-- complexity is also part of the type.

-- In this library, we take a "pure" function to mean some computation which does
-- not change the asymptotic runtime of the program.

-- | A Functor maps a pure function over some computation without altering the
-- runtime
class Functor f where
  fmap :: (a -> b) -> f (t :: Nat) a -> f t b

instance Functor O where
  fmap f (Result x) = Result (f x)

(<$>) :: Functor f => (a -> b) -> f t a -> f t b
(<$>) = fmap

class Applicative f where
  -- | Pure is used to return a value that took constant time O(1) to compute
  pure :: a -> f 1 a
  (<*>) :: f t1 (a -> b) -> f t2 a -> f (t1 + t2) b

instance Applicative O where
  pure = Result
  Result f <*> Result x = Result (f x)

-- | Monads are used to chain computations that each have some runtime
-- complexity. The complexity of the result is the sum of the individual
-- runtimes
class Monad m where
  return :: a -> m 1 a
  (>>=) :: m t1 a -> (a -> m t2 b) -> m (t1 + t2) b

instance Monad O where
  return = Result
  Result x >>= f = case f x of
    Result y -> Result y

-- | In the base case of most algorithms, we return an empty result. In order to
-- get the runtime to work out, we say this result takes no time to compute
emptyCase :: a -> O(0) a
emptyCase = Result

-- | It is always allowed to consider something more expensive than it really is
roundUp :: forall m n a. (n <= m) => O(n) a -> O(m) a
roundUp (Result x) = Result x

-- | Needed for RebindableSyntax
ifThenElse :: Prelude.Bool -> a -> a -> a
ifThenElse b x y = if b then x else y
