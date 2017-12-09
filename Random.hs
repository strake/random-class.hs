{-# LANGUAGE DefaultSignatures #-}

module Random where

import Control.Monad.Primitive
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State as M
import Data.Primitive.MutVar
import Data.Semigroup
import Data.Tuple (swap)
import Data.Void
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

class Uniform b a where
    liftUniform :: Monad m => m b -> m a

instance Uniform a a where liftUniform = id

uniform :: (Gen g, Uniform (Native g) a) => M.State g a
uniform = liftUniform uniformNative

uniformM :: (Gen g, Uniform (Native g) a, PrimMonad m) => ReaderT (Mut (PrimState m) g) m a
uniformM = liftUniform uniformNativeM

instance {-# OVERLAPPABLE #-} (Bounded a, Enum a, Bounded b, Enum b) => Uniform b a where
    liftUniform = fmap (toEnum' . foldr (\ m n -> card @b * n + fromEnum' m) 0)
                . replicateA ((card @a + card @b - 1) `div` card @b)

instance Uniform Void a where
    liftUniform = fmap $ \ case

instance Uniform a () where
    liftUniform = (() <$)
