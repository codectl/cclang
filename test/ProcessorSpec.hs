module ProcessorSpec (
  main,
  spec
  ) where

import Test.Hspec
import Control.Exception

import Processor
import Stack

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "runCommand" $ do
    it "processes a token sequence" $ do
      runCommand "0\n 1 1 + ." `shouldReturn` "2"
      runCommand "0\n 1 1 - ." `shouldReturn` "0"
      runCommand "0\n 1 1 * ." `shouldReturn` "1"
      runCommand "0\n 1 1 / ." `shouldReturn` "1"
      runCommand "0\n 1 1 % ." `shouldReturn` "0"
      runCommand "0\n 1 2 < ." `shouldReturn` "1"
      runCommand "0\n 2 1 < ." `shouldReturn` "0"
      runCommand "0\n 1 1 <= ." `shouldReturn` "1"
      runCommand "0\n 2 1 <= ." `shouldReturn` "0"
      runCommand "0\n 2 1 > ." `shouldReturn` "1"
      runCommand "0\n 1 2 > ." `shouldReturn` "0"
      runCommand "0\n 1 1 >= ." `shouldReturn` "1"
      runCommand "0\n 1 2 >= ." `shouldReturn` "0"
      runCommand "0\n 1 1 == ." `shouldReturn` "1"
      runCommand "0\n 2 1 == ." `shouldReturn` "0"
      runCommand "0\n 1 1 & ." `shouldReturn` "1"
      runCommand "0\n 1 0 & ." `shouldReturn` "0"
      runCommand "0\n 0 0 & ." `shouldReturn` "0"
      runCommand "0\n 1 1 | ." `shouldReturn` "1"
      runCommand "0\n 1 0 | ." `shouldReturn` "1"
      runCommand "0\n 0 0 | ." `shouldReturn` "0"
      runCommand "0\n 0 ! ." `shouldReturn` "1"
      runCommand "0\n 1 ! ." `shouldReturn` "0"
      runCommand "0\n 1 dup , ." `shouldReturn` "1 1"
      runCommand "0\n 1 2 swap , ." `shouldReturn` "1 2"
      runCommand "0\n 1 peek ." `shouldReturn` "1"
      runCommand "0\n 1 2 pop ." `shouldReturn` "1"
      runCommand "0\n 1 size , ." `shouldReturn` "1 1"
      runCommand "0\n 1 nil ." `shouldReturn` "1"

    context "throws error when" $ do
      it "peek is used with empty stack" $ do
        runCommand "0\n peek" `shouldThrow` errorCall "Peek on empty stack"

      it "pop is used with empty stack" $ do
        runCommand "0\n pop" `shouldThrow` errorCall "Pop on empty stack"

  describe "run" $ do
    run "resources/fibonacci.txt" `shouldReturn` "89 55 34 21 13 8 5 3 2 1 1"