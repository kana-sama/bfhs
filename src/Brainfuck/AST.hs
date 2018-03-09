module Brainfuck.AST
  ( Token(..)
  , AST
  ) where

data Token
  = Increment
  | Decrement
  | MoveLeft
  | MoveRight
  | Output
  | Input
  | Loop AST
  deriving (Eq, Show)

type AST = [Token]
