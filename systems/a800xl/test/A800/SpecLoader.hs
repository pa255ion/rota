-- | Loading from the PS ("A800.Loader", M4 step 4.8): files streamed as
-- the PS does (in order from link address 0, held), what lands in the
-- block RAMs and what hold's fall decides; the keyboard writes.
module A800.SpecLoader (loaderTests) where

import Prelude

import qualified Data.ByteString as BS
import Data.Bits (testBit)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Word
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Prelude as C
import A800.Core (CartKind (..), CoreCfg (..))
import A800.Loader
import A800.System (ramClearStep)
import A800.Media (casImageFile, parseCas)
import A800.TestRoms

-- | Stream a file (hold rises, then each byte at its offset): the state
-- after it and the two RAMs as written.
stream :: BS.ByteString -> (LoadState, M.Map Int Word8, M.Map Int Word8)
stream file = foldl' one (loadStep loadInit (True, True, Nothing), M.empty, M.empty) (zip [0 ..] (BS.unpack file))
 where
  one (s, xex, cart) (i, b) =
    let w = Just (fromIntegral (i :: Int), fromIntegral b)
        LoadWrite wx wc = loadRoute s True w
        put m = maybe m (\(a, d) -> M.insert (fromIntegral a) (fromIntegral d) m)
    in (loadStep s (False, True, w), put xex wx, put cart wc)

image :: M.Map Int Word8 -> Int -> BS.ByteString
image m n = BS.pack [ M.findWithDefault 0 i m | i <- [0 .. n - 1] ]

loaderTests :: TestTree
loaderTests = testGroup "loading from the PS (4.8)"
  [ testCase "an executable (scene.xex): into the executable RAM as it is, XEX with its length" $ do
      f <- loadFile (asmRoot </> "scene.xex")
      let (s, xex, _) = stream f
      image xex (BS.length f) @?= f
      loadedCfg s @?= Just (CoreCfg False CkNone True (fromIntegral (BS.length f)))
  , testCase "Scorch (56 KiB): an executable too" $ do
      f <- loadFile scorchXex
      let (s, xex, _) = stream f
      image xex (BS.length f) @?= f
      loadedCfg s @?= Just (CoreCfg False CkNone True (fromIntegral (BS.length f)))
  , testCase "a raw 8 KiB cartridge (cart_pass.rom): into the cartridge RAM as it is" $ do
      f <- loadFile (asmRoot </> "cart_pass.rom")
      let (s, _, cart) = stream f
      image cart 0x2000 @?= f
      loadedCfg s @?= Just (CoreCfg False Ck8 False 0)
  , testCase "a CAR file (header + 16 KiB, type 2): the image without its header" $ do
      let img = BS.pack [ fromIntegral (i * 7 + 3) | i <- [0 .. 0x3FFF :: Int] ]
          car = BS.concat [BS.pack [0x43, 0x41, 0x52, 0x54, 0, 0, 0, 2, 1, 2, 3, 4, 0, 0, 0, 0], img]
          (s, _, cart) = stream car
      image cart 0x4000 @?= img
      loadedCfg s @?= Just (CoreCfg False Ck16 False 0)
  , testCase "a new file clears RAM before the core starts (ramClearStep; the board kept RAM over loads: a warm start)" $ do
      -- started by a held load of a file ending, one address a clock, then done
      let run = take 70000 (iterate (ramClearStep False False) (ramClearStep True True Nothing))
          addrs = [ a | Just a <- run ]
      length addrs @?= 65536
      addrs @?= [0 .. maxBound]
      ramClearStep False False (Just maxBound) @?= Nothing
      -- a hold that wrote no file (a save state's load) keeps RAM
      ramClearStep True False Nothing @?= Nothing
  , testCase "a tape image (casImageFile, FUJI): a tape with the cartridge in DDR, refused in block RAM" $ do
      c <- loadFile (asmRoot </> "cas_boot.cas")
      cas <- either fail pure (parseCas c)
      let (s, _, _) = stream (casImageFile cas)
      loadedCfgFor True s @?= Just (CoreCfg False CkCas False 0)
      loadedCfg s @?= Nothing
  , testCase "refused: a CAR of another type, an executable over 64 KiB, a raw image of another size" $ do
      let car5 = BS.concat [BS.pack [0x43, 0x41, 0x52, 0x54, 0, 0, 0, 5], BS.replicate (8 + 0x2000) 0]
          bigXex = BS.concat [BS.pack [0xFF, 0xFF], BS.replicate 0x10000 0]
          odd' = BS.replicate 1000 0
          cfgOf = (\(s, _, _) -> loadedCfg s) . stream
      map cfgOf [car5, bigXex, odd'] @?= [Nothing, Nothing, Nothing]
  , testCase "writes while the core runs don't load, writes while held don't type" $ do
      let (s, _, _) = stream (BS.pack [0xFF, 0xFF])
      loadRoute s False (Just (0, 0x12)) @?= LoadWrite Nothing Nothing
      keysStep keysInit (True, Just (keysBase, 0xFF)) @?= keysInit
  , testCase "the keyboard: byte n/8 bit n mod 8 is key code n; modifiers, console, joystick" $ do
      let ws = [(keysBase + 0, 0x01), (keysBase + 7, 0x80), (keysBase + 3, 0x04), (keysBase + 8, 0x05), (keysBase + 9, 0x02), (keysBase + 10, 0x11)]
          k = foldl' (\acc (a, d) -> keysStep acc (False, Just (a, d))) keysInit ws
          held = [ n | n <- [0 .. 63 :: Int], testBit (kKeys k) n ]
      held @?= [0, 26, 63]
      (kMods k, kConsole k, kJoy k) @?= (5, 2, 0x11)
  ]
