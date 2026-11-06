module Main (main) where

import Prelude
import Test.Tasty

import qualified Test.Platform.Clocks
import qualified Test.Platform.I2c
import qualified Test.Platform.Adv7511
import qualified Test.Platform.Ycbcr422
import qualified Test.Platform.Board
import qualified Test.Platform.PsLink
import qualified Test.Platform.Input
import qualified Test.Platform.Leds
import qualified Test.Platform.Oled
import qualified Test.Platform.VgaTiming
import qualified Test.Platform.Spdif
import qualified Test.Platform.Ddr
import qualified Test.Platform.Tmds
import qualified Test.Platform.Pwm
import qualified Test.Platform.Volume
import qualified Test.Platform.Speed
import qualified Test.Platform.HdmiPacket
import qualified Test.Platform.Hdmi
import qualified Test.Platform.Features
import qualified Test.Platform.FrameInput
import qualified Test.Platform.Capture
import qualified Test.Platform.Snapshot

main :: IO ()
main = defaultMain $ testGroup "platform"
  [ Test.Platform.VgaTiming.tests
  , Test.Platform.Snapshot.tests
  , Test.Platform.I2c.tests
  , Test.Platform.Adv7511.tests
  , Test.Platform.Ycbcr422.tests
  , Test.Platform.Board.tests
  , Test.Platform.PsLink.tests
  , Test.Platform.Input.tests
  , Test.Platform.Leds.tests
  , Test.Platform.Clocks.tests
  , Test.Platform.Oled.tests
  , Test.Platform.Spdif.tests
  , Test.Platform.Ddr.tests
  , Test.Platform.Tmds.tests
  , Test.Platform.Pwm.tests
  , Test.Platform.Volume.tests
  , Test.Platform.Speed.tests
  , Test.Platform.HdmiPacket.tests
  , Test.Platform.Hdmi.tests
  , Test.Platform.Features.tests
  , Test.Platform.FrameInput.tests
  , Test.Platform.Capture.tests
  ]
