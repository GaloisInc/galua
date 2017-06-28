{-# Language ForeignFunctionInterface #-}
module Galua.LuaString
  ( LuaString
  , toByteString
  , fromByteString
  , luaStringPtr
  , peekLuaString0
  , peekLuaString
  , luaStringLen
  , unsafeFromByteString
  ) where

import Control.Monad (unless)
import qualified Data.ByteString as B
import Data.ByteString.Unsafe as U
import Data.ByteString.Internal as I
import Foreign.C
import Foreign
import Data.Hashable(Hashable(..))

-- | Wrapper for 'ByteString' values where the internal representation
-- is definitely null-terminated to make it appropriate to export
-- the internal representation to C as a null-terminated string.
--
-- Additional we only expose the IO-wrapped methods for constructing
-- a 'LuaString' to ensure that we know when allocation is happening.
newtype LuaString = LuaString ByteString
  deriving (Eq, Ord)

instance Hashable LuaString where
  hashWithSalt s (LuaString x) = hashWithSalt s x

toByteString :: LuaString -> ByteString
toByteString (LuaString bs) = bs

fromByteString :: ByteString -> IO LuaString
fromByteString bs = U.unsafeUseAsCStringLen bs peekLuaString
-- peekLuaString copies which makes this use of unsafeUseAsCStringLen safe

-- | This is not suitable for most things---in particular it should never
-- be passed out to Lua.  We use it when we are querrying the current state,
-- and we need to turn a ByteString to a LuaString temporarily.
-- We also use it to define constants for the names of the various
-- metamethods (see Galua.Overloading)
unsafeFromByteString :: ByteString -> LuaString
unsafeFromByteString = LuaString

-- | This function exposes the internal representation of the ByteString
-- for export to C. The pointer should NOT be modified. The pointer will
-- only be valid while the 'LuaString' value has not been gargabe collected.
-- In our use case the 'LuaString' should be left on the Lua stack while
-- using this pointer.
luaStringPtr :: LuaString -> IO CString
luaStringPtr (LuaString b) = U.unsafeUseAsCString b return

-- | Returns the length of the 'LuaString' not including its null-terminator.
luaStringLen :: LuaString -> Int
luaStringLen (LuaString bs) = B.length bs

-- | Copy a null-terminated 'CString' into a 'LuaString'
peekLuaString0 :: CString -> IO LuaString
peekLuaString0 ptr =
  do n <- I.c_strlen ptr
     str0 <- B.packCStringLen (ptr, fromIntegral n+1) -- include the null
     return (LuaString (B.init str0)) -- hide the null

-- | Copy a 'CStringLen' into a 'LuaString' ensuring that a null-terminator
-- is added to the resulting 'LuaString'
peekLuaString :: CStringLen -> IO LuaString
peekLuaString (ptr,len) =
  do unless (0 <= len && len < maxBound) (fail "peekLuaString: Bad length")

     str0 <- I.create (len+1) $ \out ->
               do I.memcpy out (castPtr ptr) len
                  poke (advancePtr out len) 0 -- ensure null termination

     return (LuaString (U.unsafeInit str0)) -- hide the null
