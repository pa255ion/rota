{-|
The generator's output files: include files for @rgbasm@ (Game Boy) and
@ca65@ (NES), and the CHIP-8 data. Each file names its generator; none is
edited by hand. 'outputs' lists every file with its path under the output
directory (build/demos/gen/ by default, `make demos`).
-}
module Rota.Emit
  ( outputs
  , binaryOutputs
  , rgbdsBytes
  , ca65Bytes
  ) where

import Data.Bits ((.&.))
import Data.List (intercalate)
import Data.Word (Word8)
import Numeric (showHex)

import Rota.Emit.Util
import Rota.Pic.Chip8 (chip8Program)
import Rota.Gb.Rom (gbRomInc)
import Rota.A800.Rom (a800RomInc)
import Rota.Gg.Rom (ggRomInc)
import Rota.Sms.Rom (smsRomInc)
import Rota.Snes.Rom (snesRomInc, snesSpcInc)
import Rota.Nes.Rom (Layout (..), nesRomInc)
import Rota.Sound
import Rota.Sound.Chip8
import Rota.Sound.Gb
import Rota.Sound.Nes
import Rota.Sound.A800

-- | (path, contents).
outputs :: [(FilePath, String)]
outputs =
  [ ("gb/rota_sound.inc", gbSound)
  , ("gb/rota_pic.inc", gbRomInc)
  , ("nes/rota_sound.inc", nesSound)
  , ("nes/rota_pic.inc", nesRomInc OneTable)
  , ("nes/rota_pic_split.inc", nesRomInc Split)
  , ("a800xl/rota_pic.inc", a800RomInc)
  , ("gg/rota_data.inc", ggRomInc)
  , ("sms/rota_data.inc", smsRomInc)
  , ("snes/rota_data.inc", snesRomInc)
  , ("snes/rota_spc.inc", snesSpcInc)
  , ("a800xl/rota_sound.inc", a800Sound)
  , ("chip8/beeps.txt", chip8BeepList)
  , ("chip8/rota.hex", header "#" ["The CHIP-8 demo's bytes from 0x200 (Rota.Pic.Chip8), as hex."] <> unlines [ unwords (map (drop 1 . hex2) c) | c <- chunks 16 chip8Program ]) ]

-- | (path, bytes): the CHIP-8 program itself.
binaryOutputs :: [(FilePath, [Word8])]
binaryOutputs = [ ("chip8/rota.ch8", chip8Program) ]

streamNote :: [String]
streamNote =
  [ "The chime's sound stream (Rota.Sound): per frame a count, then that"
  , "many (register, value) pairs; a count of $FF ends the stream." ]

gbSound :: String
gbSound = header ";" (streamNote <> ["Registers are the low byte of $FFxx. " <> show (length bytes) <> " bytes, " <> show (length gbStream) <> " frames."])
  <> "RotaSound::\n" <> rgbdsBytes bytes
 where
  bytes = encodeStream (fromIntegral . (.&. 0xFF)) gbStream

nesSound :: String
nesSound = header ";" (streamNote <> ["Registers are offsets from $4000. " <> show (length bytes) <> " bytes, " <> show (length nesStream) <> " frames."])
  <> "rota_sound:\n" <> ca65Bytes bytes
 where
  bytes = encodeStream (\r -> fromIntegral (r - 0x4000)) nesStream

chip8BeepList :: String
chip8BeepList = header "#" ["The buzzer: (60 Hz frame, sound timer) for each lead note."]
  <> unlines [ show f <> " " <> show st | (f, st) <- chip8Beeps ]

a800Sound :: String
a800Sound = header ";" (streamNote <> ["Registers are offsets from $D200 (POKEY). " <> show (length bytes) <> " bytes, " <> show (length a800Stream) <> " frames."])
  <> "rota_sound:\n" <> ca65Bytes bytes
 where
  bytes = encodeStream (\r -> fromIntegral (r - 0xD200)) a800Stream
