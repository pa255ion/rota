{-|
The browser build's stand-in for "Gg.Screen" (side track W): the two
functions "Sms.Desktop" uses, copied exactly; the real module's PNG and
WAV writers (JuicyPixels) are left out of the WebAssembly build.
-}
module Gg.Screen (frameRgb, frameHash) where

import Prelude
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word64, Word8)

frameRgb :: Word16 -> (Word8, Word8, Word8)
frameRgb c =
  let ch s = fromIntegral ((c `shiftR` s) .&. 0x0F) * 17
  in (ch 0, ch 4, ch 8)

frameHash :: U.Vector Word16 -> Word64
frameHash = U.foldl' step 0xcbf29ce484222325
 where
  step h c = ((h `xor` fromIntegral (c .&. 0xFF)) * 0x100000001b3 `xor` fromIntegral (c `shiftR` 8)) * 0x100000001b3
