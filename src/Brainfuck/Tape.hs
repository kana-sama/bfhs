{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Brainfuck.Tape
  ( Tape
  , make
  , moveLeft
  , moveRight
  , value
  ) where

import           Lens.Micro.Platform (Lens', lens)

data Tape a = Tape
  { tapeEmpty :: a
  , tapeLeft  :: [a]
  , tapeValue :: a
  , tapeRight :: [a]
  }

value :: Lens' (Tape a) a
value = lens tapeValue set where
  set Tape{ tapeValue = _, .. } tapeValue = Tape{..}

make :: a -> Tape a
make tapeEmpty = Tape{..} where
  tapeLeft = []
  tapeValue = tapeEmpty
  tapeRight = []

moveLeft :: Tape a -> Tape a
moveLeft tape = case tapeLeft tape of
  []                   -> newTape{ tapeValue = tapeEmpty tape }
  tapeValue : tapeLeft -> newTape{ tapeLeft, tapeValue }
  where
    newTape = tape{ tapeRight = tapeValue tape : tapeRight tape }

moveRight :: Tape a -> Tape a
moveRight tape = case tapeRight tape of
  []                    -> newTape{ tapeValue = tapeEmpty tape }
  tapeValue : tapeRight -> newTape{ tapeValue, tapeRight }
  where
    newTape = tape{ tapeLeft = tapeValue tape : tapeLeft tape }
