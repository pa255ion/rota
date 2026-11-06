-- | Layer 0, side track L (PLAN.md L.5): save states in the reference
-- model. Run a ROM, save at step n, go on k steps; a second machine that
-- has run elsewhere loads the state and must go on exactly the same:
-- registers, M-cycles and the sound after every instruction, then the
-- screen (both frames) and the serial output. States are refused for
-- another cartridge or another model.
module Gb.SpecSaveState (saveStateTests) where

import Prelude

import Control.Monad (replicateM, replicateM_)
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit

import Data.STRef (writeSTRef)
import qualified Data.Vector.Unboxed.Mutable as MU

import Gb.Hooks (Serial (..))
import Gb.Screen (Screen)
import Gb.Spec
import Gb.Spec.SaveState
import qualified Gb.Spec.State as S
import Gb.TestRoms

type Step = (Regs, Int, (Int, Int))

-- | k instructions: the registers, the M-cycle count and the sound after
-- each; then the screen, the colour frame and the serial output.
-- The whole state at the end too (as 'saveState' sees it), so state
-- that is never read back during the k steps (cartridge RAM) is checked.
goOn :: Machine s -> Int -> ST s ([Step], Screen, U.Vector Word16, (String, Snapshot))
goOn m k = do
  ts <- replicateM k (step m >> ((,,) <$> regs m <*> cycles m <*> audioSample m))
  (,,,) ts <$> screen m <*> screenRgb m <*> ((,) <$> serialOut m <*> saveState m)

-- | Every piece of state the machine has, overwritten with junk (the ROM
-- and the instrumentation aside), so a load that misses one shows.
scramble :: S.Machine s -> ST s ()
scramble m = do
  MU.set (S.mRegs m) 0xA5
  MU.set (S.mRam m) 0x5A
  MU.set (S.mCram m) 0xC3
  MU.set (S.mFb m) 3
  MU.set (S.mLast m) 2
  MU.set (S.mApu m) 7
  MU.set (S.mSpr m) 99
  MU.set (S.mVram1 m) 0x3C
  MU.set (S.mWramX m) 0x96
  MU.set (S.mPal m) 0x69
  MU.set (S.mFbC m) 0x1234
  MU.set (S.mLastC m) 0x4321
  trace <- S.getSt m S.iTRACE
  MU.set (S.mSt m) 1
  S.setSt m S.iTRACE trace
  writeSTRef (S.mSerial m) (Serial 0x77 [1, 2, 3])

-- | Saved at n, both ways on for k: the first machine's run and the
-- second's, which ran `other` steps of its own and then had its state
-- scrambled before loading.
roundTrip :: Model -> BS.ByteString -> Int -> Int -> Int
          -> Either String (([Step], Screen, U.Vector Word16, (String, Snapshot)), ([Step], Screen, U.Vector Word16, (String, Snapshot)))
roundTrip model rom n other k = runST $ do
  e1 <- newMachineModel model rom
  e2 <- newMachineModel model rom
  case (e1, e2) of
    (Right m1, Right m2) -> do
      replicateM_ n (step m1)
      snap <- saveState m1
      a <- goOn m1 k
      replicateM_ other (step m2)
      scramble m2
      r <- loadState m2 snap
      case r of
        Left why -> pure (Left why)
        Right () -> Right . (,) a <$> goOn m2 k
    (Left e, _) -> pure (Left e)
    (_, Left e) -> pure (Left e)

sameAfterLoad :: Model -> FilePath -> Int -> Int -> Int -> Assertion
sameAfterLoad model path n other k = do
  rom <- loadRomFile path
  case roundTrip model rom n other k of
    Left e -> assertFailure e
    Right ((t1, s1, c1, o1), (t2, s2, c2, o2)) -> do
      assertBool "the steps differ" (t1 == t2)
      assertBool "the screen differs" (s1 == s2)
      assertBool "the colour frame differs" (c1 == c2)
      fst o2 @?= fst o1
      assertBool "the state at the end differs" (snd o1 == snd o2)
      -- and the state moved on: not a run that stood still
      assertBool "no progress" (length t1 == k)

refused :: Model -> BS.ByteString -> Model -> BS.ByteString -> Either String ()
refused ma ra mb rb = runST $ do
  ea <- newMachineModel ma ra
  eb <- newMachineModel mb rb
  case (ea, eb) of
    (Right a, Right b) -> do
      replicateM_ 1000 (step a)
      s <- saveState a
      loadState b s
    _ -> pure (Left "no machine")

saveStateTests :: TestTree
saveStateTests = testGroup "save states (side track L, L.5)"
  [ testCase "tone.gb: the sound goes on the same" $
      sameAfterLoad Dmg (asmRoot </> "tone.gb") 30000 5000 20000
  , testCase "dmg-acid2: mid-frame, the screen the same" $
      sameAfterLoad Dmg (romRoot </> "dmg-acid2" </> "dmg-acid2.gb") 40000 123 60000
  , testCase "Blargg cpu_instrs 02 (interrupts): the serial output the same" $
      sameAfterLoad Dmg (romRoot </> "blargg" </> "cpu_instrs" </> "02-interrupts.gb") 200000 7 300000
  , testCase "save.gb: cartridge RAM restored" $
      sameAfterLoad Dmg (asmRoot </> "save.gb") 20000 50000 20000
  , testCase "mbc3.gb: the clock restored" $
      sameAfterLoad Dmg (asmRoot </> "mbc3.gb") 20000 3000 20000
  , testCase "libbet (a game): the same" $
      sameAfterLoad Dmg (romRoot </> "games" </> "libbet.gb") 150000 1000 50000
  , testCase "GBC: hdma.gb in colour" $
      sameAfterLoad Cgb (asmRoot </> "hdma.gb") 20000 500 30000
  , testCase "GBC: sound in double speed (sound_ds.gb)" $
      sameAfterLoad Cgb (asmRoot </> "sound_ds.gb") 20000 500 30000
  , testCase "GBC: a DMG cartridge (compatibility mode)" $
      sameAfterLoad Cgb (asmRoot </> "tone.gb") 20000 500 10000
  , testProperty "tone.gb and dmg-acid2 saved at any step" $ H.withTests 12 $ H.property $ do
      path <- H.forAll (Gen.element [asmRoot </> "tone.gb", romRoot </> "dmg-acid2" </> "dmg-acid2.gb"])
      n <- H.forAll (Gen.int (Range.linear 0 80000))
      other <- H.forAll (Gen.int (Range.linear 0 20000))
      rom <- H.evalIO (loadRomFile path)
      case roundTrip Dmg rom n other 4000 of
        Left e -> H.annotate e >> H.failure
        Right ((t1, s1, c1, o1), (t2, s2, c2, o2)) -> do
          H.assert (t1 == t2 && s1 == s2 && c1 == c2 && o1 == o2)
  , testCase "refused: another cartridge, another model" $ do
      tone <- loadRomFile (asmRoot </> "tone.gb")
      save <- loadRomFile (asmRoot </> "save.gb")
      hdma <- loadRomFile (asmRoot </> "hdma.gb")
      refused Dmg tone Dmg save @?= Left "a state of another cartridge"
      refused Dmg tone Cgb tone @?= Left "a state of the Dmg, this is the Cgb"
      refused Cgb hdma Cgb tone @?= Left "a state of the other cartridge mode"
      refused Dmg tone Dmg tone @?= Right ()
  ]
