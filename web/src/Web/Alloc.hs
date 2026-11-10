{-|
Memory for the page (side track W): the desktop libraries take their
inputs (ROMs, the key array, the handle's out pointer) as C pointers, so
the page asks for room here, writes into it, and passes the address.
-}
module Web.Alloc () where

import Prelude

import Data.Word (Word8)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (Ptr)

foreign export ccall web_alloc :: CInt -> IO (Ptr Word8)
foreign export ccall web_free :: Ptr Word8 -> IO ()

web_alloc :: CInt -> IO (Ptr Word8)
web_alloc n = mallocBytes (fromIntegral (max 1 n))

web_free :: Ptr Word8 -> IO ()
web_free = free
