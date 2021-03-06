{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Util (card, fromEnum', module U) where

import qualified Data.List as L
import Numeric.Natural
import "util" Util as U

card :: ∀ a . (Bounded a, Enum a) => Natural
card = L.genericLength [minBound @a..]

fromEnum' :: (Bounded a, Enum a) => a -> Natural
fromEnum' a = L.genericLength [minBound..a] - 1
