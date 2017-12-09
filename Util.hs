{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Util where

import Control.Applicative
import qualified Data.List as L
import Data.Semigroup
import Numeric.Natural

replicateA :: Applicative p => Natural -> p a -> p [a]
replicateA n = sequenceA . L.genericReplicate n

mtimesA :: (Applicative p, Semigroup a, Monoid a) => Natural -> p a -> p a
mtimesA n = unAp . stimes n . Ap

newtype Ap p a = Ap { unAp :: p a } deriving (Functor, Applicative)
instance (Applicative p, Semigroup a) => Semigroup (Ap p a) where (<>) = liftA2 (<>)
instance (Applicative p, Semigroup a, Monoid a) => Monoid (Ap p a) where
    mempty = pure mempty
    mappend = (<>)

card :: ∀ a . (Bounded a, Enum a) => Natural
card = L.genericLength [minBound @a..]

fromEnum' :: (Bounded a, Enum a) => a -> Natural
fromEnum' a = L.genericLength [minBound..a] - 1

toEnum' :: (Bounded a, Enum a) => Natural -> a
toEnum' = L.genericIndex [minBound..]
