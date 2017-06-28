module Galua.Util.String where

import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8With,encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.ByteString(ByteString)


unpackUtf8 :: ByteString -> String
unpackUtf8 = Text.unpack . decodeUtf8With lenientDecode

packUtf8 :: String -> ByteString
packUtf8 = encodeUtf8 . Text.pack


