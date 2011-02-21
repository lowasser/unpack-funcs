module Control.Monad.Unpack.Class where

import Control.Applicative
import Control.Monad.Trans.Class

import Data.Functor.Identity

import GHC.Exts

type (:~>) arg = UnpackedReaderT arg Identity

infixr 0 :~>

class Unpackable arg where
  data UnpackedReaderT arg :: (* -> *) -> * -> *
  runUnpackedReaderT :: UnpackedReaderT arg m a -> arg -> m a
  unpackedReaderT :: (arg -> m a) -> UnpackedReaderT arg m a

{-# INLINE ask #-}
ask :: (Monad m, Unpackable arg) => UnpackedReaderT arg m arg
ask = unpackedReaderT return

{-# INLINE local #-}
local :: (Monad m, Unpackable arg) => (arg -> arg) -> UnpackedReaderT arg m a -> UnpackedReaderT arg m a
local f m = unpackedReaderT $ runUnpackedReaderT m . f

instance Unpackable arg => MonadTrans (UnpackedReaderT arg) where
  {-# INLINE lift #-}
  lift m = unpackedReaderT $ \ _ -> m

instance (Unpackable arg, Monad m) => Monad (UnpackedReaderT arg m) where
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}
  return x = lift $ return x
  m >>= k = unpackedReaderT $ \ arg ->
    do	a <- runUnpackedReaderT m arg
	runUnpackedReaderT (k a) arg

instance (Unpackable arg, Functor f) => Functor (UnpackedReaderT arg f) where
  {-# INLINE fmap #-}
  fmap f m = unpackedReaderT $ \ arg -> fmap f (runUnpackedReaderT m arg)

instance (Unpackable arg, Applicative f) => Applicative (UnpackedReaderT arg f) where
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
  pure f = unpackedReaderT $ \ _ -> pure f
  f <*> x = unpackedReaderT $ \ arg -> runUnpackedReaderT f arg <*> runUnpackedReaderT x arg