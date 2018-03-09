module Brainfuck.Parser
  ( parse
  ) where

import           Data.Void                  (Void)

import           Control.Applicative        (many, (<|>))

import           Text.Megaparsec            (Parsec, between, runParser)
import           Text.Megaparsec.Char       (char)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Brainfuck.AST              (AST, Token (..))

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
    <|> Loop      <$> between (char '[') (char ']') program
  where
    token c v = v <$ char c

parse :: String -> AST
parse source = case runParser program "" source of
  Right ast -> ast
  Left _    -> []
