module StackSpec (
  main,
  spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import Control.Exception (evaluate)

import Stack

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "dup" $ do
    it "returns stack with duplicated top element" $ do
      dup (Stack [1]) `shouldBe` Stack [1,1]

  describe "swap" $ do
    context "when stack has 2 or more elements" $ do
      it "returns stack with top two elements swapped" $ do
        swap (Stack [1,2]) `shouldBe` Stack [2,1]

    context "when stack has single element" $ do
      it "returns stack with single element" $ do
        swap (Stack [1]) `shouldBe` Stack [1]

    context "when stack is empty" $ do
      it "returns empty stack" $ do
        swap (Stack []) `shouldBe` (Stack [] :: Stack Integer)

  describe "peek" $ do
    context "when stack is not empty" $ do
      it "returns the top of the stack" $ do
        peek (Stack [1]) `shouldBe` Just 1

    context "when stack is empty" $ do
      it "returns nothing" $ do
        peek (Stack []) `shouldBe` (Nothing :: Maybe Int)

  describe "push" $ do
    it "returns stack with added element on top" $ do
      push 2 (Stack [1]) `shouldBe` Stack [2,1]

  describe "pop" $ do
    context "when stack is not empty" $ do
      it "returns pair with popped element and stack without top element" $ do
        pop (Stack [1,2]) `shouldBe` (Just 1, Stack [2])

    context "when stack is empty" $ do
      it "returns pair with nothing and empty stack" $ do
        pop (Stack []) `shouldBe` ((Nothing :: Maybe Int), Stack [])

  describe "size" $ do
    it "returns number of elements in the stack" $ do
       size (Stack [1,1]) `shouldBe` 2

  describe "nil" $ do
    it "returns unchanged stack" $ do
      nil (Stack [1]) `shouldBe` Stack [1]