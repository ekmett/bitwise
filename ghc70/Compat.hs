module Compat
  ( unsafeIOToST
  , uncons
  , packCStringLen
  , unsafeUseAsCStringLen
  ) where

import Control.Monad.ST (unsafeIOToST)
import Data.ByteString (uncons, packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
