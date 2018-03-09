module Test.Brainfuck.TapeSpec
    ( spec
    ) where

import           Test.Hspec          (Spec, it, shouldBe)

import           Lens.Micro.Platform ((%~), (&), (^.))

import           Brainfuck.Tape      (Tape)
import qualified Brainfuck.Tape      as Tape

spec :: Spec
spec = do
  it "should make tape with one initial value" $ do
    let tape = Tape.make 0
    tape ^. Tape.value `shouldBe` 0
  it "value should refer to current value" $ do
    let tape0 = Tape.make 0
    let tape1 = tape0 & Tape.value %~ succ
    tape1 ^. Tape.value `shouldBe` 1
  it "value should be initial after moving left and revert" $ do
    let tape0 = Tape.make 0
    let tape1 = tape0 & Tape.value %~ succ
    tape1 ^. Tape.value `shouldBe` 1
    let tape2 = Tape.moveLeft tape1
    tape2 ^. Tape.value `shouldBe` 0
    let tape3 = Tape.moveRight tape2
    tape3 ^. Tape.value `shouldBe` 1

