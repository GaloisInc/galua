module Galua.Micro.ExecEnv where

import Data.IORef(IORef)
import Data.Vector.Mutable(IOVector)
import Data.Map(Map)

import Galua.Value(Value)
import Galua.Code(Function)
import Galua.FunId(FunId)
import Galua.Micro.AST(BlockName,Block)

data MLuaExecEnv = MLuaExecEnv
  { mluaExecRegsValue :: {-# UNPACK #-} !(IOVector Value)
  , mluaExecRegsRefs  :: {-# UNPACK #-} !(IOVector (IORef Value))
    -- ^ Local values, split across two arrays.
    -- INVARIANT: if an index is defined in one of the arrays, it
    -- will be undefined in the other.

  , mluaExecRegsTMP :: {-# UNPACK #-} !(IOVector Value)
    -- ^ Additional--temporary--registers

  , mluaExecArgReg  :: {-# UNPACK #-} !(IORef [Value]) -- varars
  , mluaExecListReg :: {-# UNPACK #-} !(IORef [Value])  -- varres


  , mluaExecUpvals :: {-# UNPACK #-} !(IOVector (IORef Value))
  , mluaExecCode   :: !(Map BlockName Block) -- ^ Our CFG


  -- The current functions
  , mluaExecClosure   :: !Value
  , mluaExecFID       :: !FunId                         -- ^ Our functoin ID
  , mluaExecFunction  :: !Function
  }

