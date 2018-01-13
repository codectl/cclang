module ProcessorSpec (
  main,
  spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Processor
import Stack

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "processData" $ do
    it "processes a token sequence" $ do
      processData "0\n 1 1 + ." `shouldBe` ([], Stack [], "2")