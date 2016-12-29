{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Galua.Debugger.CommandQueue
  ( CommandQueue
  , newCommandQueue
  , sendCommand
  , waitForCommand, waitForCommandSTM, peekCmd
  , getCommandCount
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad
import Data.Word

import Galua.Util.IOURef

data CommandQueue a = CommandQueue
  { cmdLen    :: !(MVar Int)
    -- ^ How many commands are waiting to be processed.

  , cmdCheck  :: {-# UNPACK #-} !(IOURef Bool)
    -- ^ Should we be checking for commands.
    -- INVARINAT: This should be `False` iff `cmdLen` is 0

  , cmdQueue  :: !(TQueue (a,Bool))
    {- ^ Commands from the outside world, telling the debugger what to do next.
         The boolean indicates if this command modifis the state, and as such
         should modify `cmdCount`.
         INVARIANT: the length of this queue should be exactly `cmdLen`.
     -}

  , cmdCount :: {-# UNPACK #-} !(IOURef Word64)
    {- ^ Every time we do a command, we should increment this counter.
    This is useful to implement polling from the outside world,
    where the client can know if there have been any commands executed
    since last time they looked. -}
  }

getCommandCount :: CommandQueue a -> IO Word64
getCommandCount CommandQueue { cmdCount } = readIOURef cmdCount

newCommandQueue :: IO (CommandQueue a)
newCommandQueue =
  do cmdLen   <- newMVar 0
     cmdCheck <- newIOURef False
     cmdQueue <- atomically newTQueue
     cmdCount <- newIOURef 0
     return CommandQueue { .. }

{- | The boolan indicates if the command should contribute towards the
global command counter.  Generally, commands that might change something
in the state should affect this, while "read-only" commands may be invisible. -}
sendCommand :: CommandQueue a -> a -> Bool -> IO ()
sendCommand CommandQueue { cmdLen, cmdQueue, cmdCheck } a vis =
  modifyMVar_ cmdLen $ \n ->
    do atomically (writeTQueue cmdQueue (a,vis))
       writeIOURef cmdCheck True
       return $! n + 1

peekCmd :: CommandQueue a -> IO (Maybe a)
peekCmd cmd@CommandQueue { cmdQueue, cmdCheck } =
  do check <- readIOURef cmdCheck
     if check
      then join $ atomically $
             do notready <- isEmptyTQueue cmdQueue
                if notready
                  then return (return Nothing)
                  else fmap Just <$> waitForCommandSTM cmd
      else return Nothing

waitForCommand :: CommandQueue a -> IO a
waitForCommand = join . atomically . waitForCommandSTM

waitForCommandSTM :: CommandQueue a -> STM (IO a)
waitForCommandSTM CommandQueue { cmdLen, cmdQueue, cmdCount, cmdCheck } =
  do (c,vis) <- readTQueue cmdQueue
     return $ modifyMVar cmdLen $ \n ->
        do when vis (modifyIOURef cmdCount (+ 1))
           let n' = n - 1
           when (n' == 0) (writeIOURef cmdCheck False)
           return (n', c)






