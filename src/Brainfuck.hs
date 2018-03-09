module Brainfuck
  ( eval
  ) where

import           Brainfuck.Interpreter (execFromAST)
import           Brainfuck.Parser      (parse)

eval :: String -> String -> String
eval input source = execFromAST input (parse source)
