module Data.Stream
  ( Stream (..)
  , repeat
  ) where

import           Prelude hiding (repeat)

data Stream a = a :<: Stream a

repeat :: a -> Stream a
repeat a = let s = a :<: s in s
