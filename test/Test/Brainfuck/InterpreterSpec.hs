module Test.Brainfuck.InterpreterSpec
  ( spec
  ) where

import           Test.Hspec            (Spec, it, shouldBe)

import           Brainfuck.AST         (Token (..))
import qualified Brainfuck.Interpreter as Interpreter

spec :: Spec
spec = do
  it "should output nothing" $ do
    Interpreter.execFromAST "" [] `shouldBe` ""
  it "should output 01" $ do
    let source = replicate 48 Increment ++ [Output, Increment, Output]
    Interpreter.execFromAST "" source `shouldBe` "01"
  it "should echo" $ do
    let source = [Loop [Input, Output]]
    Interpreter.execFromAST "qwe" source `shouldBe` "qwe"

