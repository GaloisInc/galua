module Galua.Micro.JIT (jit) where

import           Galua.Mach
import           Galua.Micro.AST
import qualified Galua.Micro.Compile as GHC (compile)

type CompiledFunction = VM -> IO NextStep

jit :: MicroFunction -> IO CompiledFunction
jit = GHC.compile imports . compile
  where
  imports = [ "Galua.Mach" ] -- XXX

{- The Plan
   --------

The state:

data State = State
  { reg_0 :: Value
  , ...
  , ref_0 :: IORef Value
  , ...
  , tmp_0 :: Value
  , ...
  , args  :: [Value]
  , list  :: [Value]
  }

Read only values:

  * upvalues :: IOVector (IORef Value)
  * closure  :: Value
  * fid      :: Value
  * fun      :: Function

Code:

\fid closure vm ->
  do let clo = referenceVal kClosure { cloFun = fun, cloUpvalues = upvalues } = referenceVal closure

 entryBlock s0

  where

  s0 = State { reg0 = error "unsued" ... }

  entryBlock :: State -> IO NextStep
  entryBlock !s =
    do s <- stmt1 s
       s <- stmt2 s
       s <- stmt3 s
       block1 s

  block1 s =
    do s <- stmt1 s
       s <- stmt2 s
       s <- stmt3 s
       block2 s


-}
compile :: MicroFunction -> [String]
compile _ = []


{-
generateState :: MicroFunction -> Q (Name, Name, Dec)
generateState mf =
  do st <- newName "State"
     sd <- newName "State"
     d <- dataD (cxt []) s [] Nothing [con] (cxt [])
     return (st,sd,d)
-}



