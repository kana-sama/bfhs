module Brainfuck.Tape
  ( Tape
  , make
  , moveLeft
  , moveRight
  , value
  ) where

import           Data.Stream         (Stream (..))
import qualified Data.Stream         as Stream

import           Lens.Micro.Platform (Lens', lens)

data Tape a = Tape (Stream a) a (Stream a)

value :: Lens' (Tape a) a
value = lens get set where
  get (Tape _ v _) = v
  set (Tape l _ r) v = Tape l v r

make :: a -> Tape a
make e = Tape (Stream.repeat e) e (Stream.repeat e)

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l :<: ls) v rs) = Tape ls l (v :<: rs)

moveRight :: Tape a -> Tape a
moveRight (Tape ls v (r :<: rs)) = Tape (v :<: ls) r rs
