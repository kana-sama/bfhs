module Test.Brainfuck.ParserSpec
  ( spec
  ) where

import           Test.Hspec       (Spec, it, shouldBe)

import           Brainfuck.AST    (Token (..))
import           Brainfuck.Parser (parse)

spec :: Spec
spec = do
  it "should return empty ast on empty source" $ do
    parse "" `shouldBe` []
  it "should return loop" $ do
    parse "+++[-]" `shouldBe` [Increment, Increment, Increment, Loop [Decrement]]
  it "should return loop" $ do
    parse "[+-<>.,]" `shouldBe` [Loop [Increment, Decrement, MoveLeft, MoveRight, Output, Input]]
