-- | Specialize the code for a function with the given type information.
module Galua.Micro.Translate.Specialize (specialize) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe(maybeToList)

import Galua.Code(literalType)
import Galua.ValueType(allTypes)
import Galua.Micro.AST
import Galua.Micro.Type.Value( LocalState(..),SingleV(..),Type(..), RegVal(..)
                             , valueCases)

{- Observation:

If at the end of block `B` we have a case:

    case type of R of
      Nil -> goto C
      ...

and we've already computed that at the start of `C` register `R` is
of type `Number`, then this branch of the case cannot be taken.

Similarly:

  case type of R of
    Nil -> goto C
    _   -> goto D

If at the beginning of `D` `R` is known to be `Nil` then the default
case is not needed, as the `Nil` case will always win.

-}



type Info = Map BlockName LocalState

specialize :: Info -> MicroFunction -> MicroFunction
specialize i f = f { functionCode = newMap }
  where newMap = dfs i (functionCode f) Map.empty [EntryBlock]

type Blocks = Map BlockName Block

dfs :: Info -> Blocks -> Blocks -> [BlockName] -> Blocks
dfs i blocks done todo =
  case todo of
    [] -> done
    bn : bns
      | bn `Map.member` done -> dfs i blocks done bns
      | otherwise ->
        case Map.lookup bn blocks of
          Nothing -> error "Specialize.dfs: missing block"
          Just b  -> let (xs, b1) = simplifyBlock i b
                     in dfs i blocks (Map.insert bn b1 done) (xs ++ todo)


simplifyBlock :: Info -> Block -> ([BlockName], Block)
simplifyBlock i b = (xs, b { blockEnd = b1 })
  where (xs, b1) = simplifyBlockEnd i (blockEnd b)

simplifyBlockEnd
  :: Info -> BlockStmt EndStmt -> ([BlockName], BlockStmt EndStmt)
simplifyBlockEnd i b = (xs, b { stmtCode = s1 })
  where (xs,s1) = specializeCase i (stmtCode b)


specializeCase :: Info -> EndStmt -> ([BlockName], EndStmt)
specializeCase info stmt =
  case stmt of
    Case e as d ->
      case e of
        ELit l ->
          case lookup (literalType l) as of
            Just l1 -> ([l1], Goto l1)
            Nothing -> useDefault d

        EUp {} -> error "specializeCase: upvalue?"

        EReg r ->
          case filter (plausible r) as of
            []                          -> useDefault d
            [(t,l)] | skipDefault r t d -> ([l], Goto l)
            as' -> (maybeToList d ++ map snd as',  Case e as' d)

    Goto l      -> ([l], stmt)
    If _ l1 l2  -> ([l1,l2],stmt)

    TailCall _  -> ([], stmt)
    Return      -> ([], stmt)
    Raise _     -> ([], stmt)

  where
  useDefault d = case d of
                   Just l  -> ([l], Goto l)
                   Nothing ->  error "specializeCase: incomplete case?"

  plausible r (t,otherBlock) =
    case Map.lookup otherBlock info of
      Nothing  -> False -- block must be unreachable
      Just tys -> regMatches r t tys

  skipDefault r t mb =
    case mb of
      Nothing -> True
      Just b  ->
        case Map.lookup b info of
          Nothing -> True   -- block must be unreachable
          Just s ->
            case Map.lookup r (env s) of
              Nothing -> True   -- implicit bottom?
              Just rv ->
                case rv of
                  RegBottom -> True
                  RegVal rv ->
                    let cases = valueCases rv
                        ok ty = all (not . matchesSingle ty) cases
                    in all ok (filter (/= t) allTypes)
                  RegRef {} -> error "skipDefault: ref?"


regMatches :: Reg -> ValueType -> LocalState -> Bool
regMatches r t s =
  case Map.lookup r (env s) of
    Nothing -> True
    Just v ->
      case v of
        RegBottom -> False
        RegVal rv -> any (matchesSingle t) (valueCases rv)
        RegRef {} -> error "exprMatches: RegRef"
          -- This shouldn't happen: we cased on something that is
          -- supposed to be a reference

matchesSingle :: ValueType -> SingleV -> Bool
matchesSingle t v =
  case (t,v) of
    (NumberType,        BasicValue Number)        -> True
    (StringType,        StringValue _)            -> True
    (FunctionType,      FunctionValue _)          -> True
    (TableType,         TableValue _)             -> True
    (BoolType,          BooleanValue _)           -> True
    (NilType,           BasicValue Nil)           -> True
    (UserDataType,      BasicValue UserData)      -> True
    (LightUserDataType, BasicValue LightUserData) -> True
    (ThreadType,        BasicValue Thread)        -> True
    _                                             -> False


 

