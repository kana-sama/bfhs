module Test.BrainfuckSpec
  ( spec
  ) where

import           Test.Hspec (Spec, it, shouldBe)

import           Brainfuck  (eval)

spec :: Spec
spec = do
  it "cat" $ do
    eval "352" ",[.,]" `shouldBe` "352"
  it "hello" $ do
    let source = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+."
    eval "" source `shouldBe` "Hello World!"
