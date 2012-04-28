module Compat
  ( unsafeIOToST
  , uncons
  , packCStringLen
  , unsafeUseAsCStringLen
  ) where

import Control.Monad.ST (unsafeIOToST)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Foreign.C.String (CStringLen)

uncons :: BS.ByteString -> Maybe (Word8, BS.ByteString)
uncons s = if BS.null s then Nothing else Just (BS.head s, BS.tail s)

unsafeUseAsCStringLen :: BS.ByteString -> (CStringLen -> IO a) -> IO a
unsafeUseAsCStringLen = BS.useAsCStringLen

packCStringLen :: CStringLen -> IO BS.ByteString
packCStringLen = return . BS.packCStringLen
