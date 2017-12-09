{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Random (Gen (..), Split (..), Uniform (..),
               uniform,  range,  weighted,
               uniformM, rangeM, weightedM) where

import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State as M
import Data.Bool
import Data.Foldable
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Primitive.MutVar
import Data.Ratio
import Data.Semigroup
import Data.Tuple (swap)
import Numeric.Natural

import Util

class Gen g where
    type Mut s g = m | m -> s g
    type instance Mut s g = MutVar s g
    type Native g
    uniformNative :: M.State g (Native g)
    uniformNativeM :: PrimMonad m => ReaderT (Mut (PrimState m) g) m (Native g)
    skip :: Natural -> g -> g
    skipM :: PrimMonad m => Natural -> ReaderT (Mut (PrimState m) g) m ()

    default uniformNativeM :: (Mut (PrimState m) g ~ MutVar (PrimState m) g,
                               PrimMonad m) => ReaderT (Mut (PrimState m) g) m (Native g)
    uniformNativeM = ReaderT $ flip atomicModifyMutVar $ swap . M.runState uniformNative

    skip n = appEndo . stimes n . Endo $ M.execState uniformNative

    default skipM :: (Mut (PrimState m) g ~ MutVar (PrimState m) g,
                      PrimMonad m) => Natural -> ReaderT (Mut (PrimState m) g) m ()
    skipM = flip mtimesA (() <$ uniformNativeM)

class Split g where
    split :: g -> (g, g)

class Uniform a where
    liftUniform :: (Bounded b, Enum b, Monad m) => m b -> m a

uniform :: (Gen g, Bounded (Native g), Enum (Native g), Uniform a) => M.State g a
uniform = liftUniform uniformNative

uniformM :: (Gen g, Bounded (Native g), Enum (Native g), Uniform a, PrimMonad m)
         => ReaderT (Mut (PrimState m) g) m a
uniformM = liftUniform uniformNativeM

instance {-# OVERLAPPABLE #-} (Bounded a, Enum a) => Uniform a where
    liftUniform = range' (minBound, maxBound)

instance Uniform () where
    liftUniform _ = pure ()

range :: (Gen g, Bounded (Native g), Enum (Native g), Enum a) => (a, a) -> M.State g a
range = flip range' uniformNative

rangeM :: (Gen g, Bounded (Native g), Enum (Native g), Enum a, PrimMonad m)
       => (a, a) -> ReaderT (Mut (PrimState m) g) m a
rangeM = flip range' uniformNativeM

range' :: ∀ a b m . (Enum a, Bounded b, Enum b, Monad m) => (a, a) -> m b -> m a
range' (a, b) = untilJust
              . fmap (toEnumMayWrap' . foldr (\ m n -> card @b * n + fromEnum' m) 0)
              . replicateA @_ @[] r
  where toEnumMayWrap' :: Natural -> Maybe a
        toEnumMayWrap' n | n > r * card @b `div` card_a * card_a = Nothing
                         | otherwise = [a..b] !!? (n `div` card_a)

        r = (card_a + card @b - 1) `div` card @b

        card_a = L.genericLength [a..b]

{-# INLINE[1] range' #-}
{-# RULES "range'" range' = pure id #-}

weighted :: (Gen g, Bounded (Native g), Enum (Native g), Uniform a)
         => NonEmpty (a, Ratio Natural) -> M.State g a
weighted = weighted' range

weightedM :: (Gen g, Bounded (Native g), Enum (Native g), Uniform a, PrimMonad m)
          => NonEmpty (a, Ratio Natural) -> ReaderT (Mut (PrimState m) g) m a
weightedM = weighted' rangeM

weighted' :: Functor f
          => ((Natural, Natural) -> f Natural) -> NonEmpty (a, Ratio Natural) -> f a
weighted' range aps = flip go aps . (% b) <$> range (0, b - 1)
  where b = lcms $ denominator . snd <$> aps
        go x = NE.uncons & \ case ((a, _), Nothing) -> a
                                  ((a, p), Just aps) -> bool (go (x - p) aps) a (x < p)

lcms :: NonEmpty Natural -> Natural
lcms = liftA2 div product (foldr' gcd 0)
