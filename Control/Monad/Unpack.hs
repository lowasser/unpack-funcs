{-# LANGUAGE TypeOperators, TypeFamilies, TemplateHaskell #-}
module Control.Monad.Unpack (module Control.Monad.Unpack.Class, (:~>), unpack, ($~)) where

import Data.Functor.Identity

import Control.Monad.Unpack.Class
import Control.Monad.Unpack.TH

import Data.ByteString (ByteString)
import Data.Primitive.Addr
import Data.Primitive.Array
import Data.Primitive.ByteArray
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Storable as S
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import Data.Int

$(unpack1Instance ''Int)
$(unpack1Instance ''Int8)
$(unpack1Instance ''Int16)
$(unpack1Instance ''Int32)
$(unpack1Instance ''Int64)
$(unpack1Instance ''Word)
$(unpack1Instance ''Word8)
$(unpack1Instance ''Word16)
$(unpack1Instance ''Word32)
$(unpack1Instance ''Word64)
$(unpack1Instance ''Char)
$(unpack1Instance ''Array)
$(unpack1Instance ''MutableArray)
$(unpack1Instance ''ByteArray)
$(unpack1Instance ''MutableByteArray)
$(unpack1Instance ''Ptr)
$(unpack1Instance ''Addr)
$(unpack1Instance ''ForeignPtr)
$(unpackInstance ''ByteString)
$(unpackInstance ''V.Vector)
$(unpackInstance ''P.Vector)
$(unpackInstance ''S.Vector)
$(unpackInstance ''V.MVector)
$(unpackInstance ''P.MVector)
$(unpackInstance ''S.MVector)
$(noUnpackInstance ''Bool)
$(noUnpackInstance ''Maybe)
$(noUnpackInstance ''Either)

instance Unpackable () where
  newtype UnpackedReaderT () m a = Result {runResult :: m a}
  runUnpackedReaderT func _ = runResult func
  unpackedReaderT func = Result $ func ()

type (:~>) arg = UnpackedReaderT arg Identity

infixr 0 :~>

{-# INLINE ($~) #-}
($~) :: Unpackable arg => (arg :~> a) -> arg -> a
f $~ arg = runIdentity (f `runUnpackedReaderT` arg)

{-# INLINE unpack #-}
unpack :: Unpackable arg => (arg -> a) -> (arg :~> a)
unpack f = unpackedReaderT $ Identity . f