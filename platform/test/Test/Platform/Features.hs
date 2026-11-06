module Test.Platform.Features (tests) where

import Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Platform.Features

-- | The cabal build passes neither -DPLATFORM_NO_MOVIES nor
-- -DPLATFORM_NO_SCREENSHOTS: every suite tests the default build, with
-- both features in (side track O, O.0).
tests :: TestTree
tests = testGroup "Platform.Features"
  [ testCase "movies and screenshots are on by default" $ do
      moviesEnabled @?= True
      screenshotsEnabled @?= True
  ]
