module Brainfuck.Parser
  ( parse
  ) where

import           Data.Void            (Void)

import           Control.Applicative  (many, (<|>))

import           Text.Megaparsec      (Parsec, between, runParser)
import           Text.Megaparsec.Char (char)

import           Brainfuck.AST        (AST, Token (..))

type Parser = Parsec Void String

program :: Parser AST
program = many token

token :: Parser Token
token = Increment <$  char '+'
    <|> Decrement <$  char '-'
    <|> MoveLeft  <$  char '<'
    <|> MoveRight <$  char '>'
    <|> Output    <$  char '.'
    <|> Input     <$  char ','
    <|> Loop      <$> (char '[' `between` char ']') program

parse :: String -> AST
parse source = case runParser program "" source of
  Right ast -> ast
  Left _    -> []
