{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Brainfuck.Interpreter
  ( execFromAST
  ) where

import           Lens.Micro.Platform  (makeLenses, to, use, view, (%=), (+=),
                                       (-=), (.=), (<~), _head, _tail)

import qualified Data.Char            as Char

import           Control.Monad        (when)

import           Control.Monad.State  (MonadState, State)
import qualified Control.Monad.State  as State

import           Control.Monad.Except (ExceptT, MonadError)
import qualified Control.Monad.Except as Except

import           Brainfuck.AST        (AST, Token (..))

import           Brainfuck.Tape       (Tape, value)
import qualified Brainfuck.Tape       as Tape

data InterpreterState = InterpreterState
  { _tape   :: Tape Int
  , _input  :: String
  , _output :: String
  }

makeLenses ''InterpreterState

data InterpreterError
  = EndOfInput
  deriving (Show)

newtype Interpreter a = Interpreter (ExceptT InterpreterError (State InterpreterState) a)
  deriving (Functor, Applicative, Monad, MonadState InterpreterState, MonadError InterpreterError)

execInterpreter :: Interpreter a -> String -> String
execInterpreter (Interpreter m) _input
  = view output
  . flip State.execState InterpreterState{..}
  . Except.runExceptT
  $ m
  where
    _tape = Tape.make 0
    _output = mempty

interpret :: AST -> Interpreter ()
interpret [] = pure ()
interpret (token:tokens) = run token >> interpret tokens where
  run Increment  = tape.value += 1
  run Decrement  = tape.value -= 1
  run MoveLeft   = tape %= Tape.moveLeft
  run MoveRight  = tape %= Tape.moveRight
  run (Loop ast) = do
    interpret ast
    value <- use $ tape.value
    when (value /= 0) $ do
      run (Loop ast)
  run Input      = do
    use input >>= \case
      []   -> Except.throwError EndOfInput
      c:cs -> do
        input .= cs
        tape.value .= Char.ord c
  run Output     = do
    code <- use $ tape.value
    output %= (++ [Char.chr code])

execFromAST :: String -> AST -> String
execFromAST input = flip execInterpreter input . interpret
