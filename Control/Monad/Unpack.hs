{-# LANGUAGE TypeOperators, TypeFamilies, TemplateHaskell #-}
module Control.Monad.Unpack (module Control.Monad.Unpack.Class) where

import Control.Monad.Unpack.Class
import Control.Monad.Unpack.TH

import Data.Primitive.Array
import Data.Primitive.ByteArray
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

$(unpack1 ''Int)
$(unpack1 ''Char)
$(unpack1 ''Array)
$(unpack1 ''MutableArray)
$(unpack1 ''ByteArray)
$(unpack1 ''MutableByteArray)
$(unpack ''V.Vector)
$(unpack ''P.Vector)
$(unpack ''V.MVector)
$(unpack ''P.MVector)