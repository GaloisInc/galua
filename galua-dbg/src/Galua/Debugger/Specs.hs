module Galua.Debugger.Specs where

import           Data.ByteString (ByteString)
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Maybe(mapMaybe)
import           Data.Text.Encoding(encodeUtf8)

import qualified Galua.Spec.AST    as Spec
import qualified Galua.Spec.Parser as Spec

type SpecType = Spec.ValDecl Spec.Parsed
type SpecDecl = Spec.Decl Spec.Parsed


newtype GlobalTypeMap = GTM (Map ByteString GlobalTypeEntry)
data GlobalTypeEntry  = GlobalType [SpecType] | GlobalNamespace GlobalTypeMap


makeGlobalTypeMap :: [SpecDecl] -> GlobalTypeMap
makeGlobalTypeMap = mk . mapMaybe decl
  where
  nm x = encodeUtf8 (Spec.nameText x)

  decl d = case d of
             Spec.DClass {}     -> Nothing
             Spec.DType {}      -> Nothing
             Spec.DNamespace ns -> Just (namespace ns)
             Spec.DValDecl vd   -> Just (valDecl vd)

  valDecl vd = (nm (Spec.valName vd), GlobalType [vd])

  jn (GlobalType xs) (GlobalType ys) = GlobalType (xs ++ ys)
  jn (GlobalNamespace (GTM x)) (GlobalNamespace (GTM y)) =
      GlobalNamespace (GTM (Map.unionWith jn x y))
  jn x _ = x -- XXX: shouldn't happen

  mk = GTM . Map.fromListWith jn

  namespace x =
    ( nm (Spec.namespaceName x)
    , GlobalNamespace $ mk $ map valDecl (Spec.namespaceMembers x) ++
                             map namespace (Spec.namespaceNested x)
    )




lookupGlobal ::
  GlobalTypeMap -> ByteString -> [ByteString] -> Maybe GlobalTypeEntry
lookupGlobal (GTM mp) x ls =
  do ent <- Map.lookup x mp
     case ls of
       [] -> Just ent
       l : more ->
          case ent of
            GlobalType _        -> Nothing
            GlobalNamespace mp1 -> lookupGlobal mp1 l more






