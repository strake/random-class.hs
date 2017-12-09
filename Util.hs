{-# LANGUAGE AllowAmbiguousTypes #-}

module Util where

import qualified Data.List as L
import Numeric.Natural

replicateA :: Applicative p => Natural -> p a -> p [a]
replicateA n = sequenceA . L.genericReplicate n

card :: ∀ a . (Bounded a, Enum a) => Natural
card = L.genericLength [minBound @a..]

fromEnum' :: (Bounded a, Enum a) => a -> Natural
fromEnum' a = L.genericLength [minBound..a] - 1

toEnum' :: (Bounded a, Enum a) => Natural -> a
toEnum' = L.genericIndex [minBound..]
