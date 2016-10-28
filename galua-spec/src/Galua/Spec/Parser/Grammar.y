{
{-# LANGUAGE TypeFamilies #-}
module Galua.Spec.Parser.Grammar(Parsed,parseSpec) where

import Data.Either(partitionEithers)
import Data.Void(Void)
import Galua.Spec.AST
import Galua.Spec.Parser.Lexer
import Galua.Spec.Parser.Monad

}

%token
  'class'     { Lexeme { lexemeToken = KW_class       } }
  'extends'   { Lexeme { lexemeToken = KW_extends     } }
  'namespace' { Lexeme { lexemeToken = KW_namespace   } }
  'type'      { Lexeme { lexemeToken = KW_type        } }
  'mutable'   { Lexeme { lexemeToken = KW_mutable     } }

  'boolean'   { Lexeme { lexemeToken = KW_boolean     } }
  'string'    { Lexeme { lexemeToken = KW_string      } }
  'number'    { Lexeme { lexemeToken = KW_number      } }
  'integer'   { Lexeme { lexemeToken = KW_integer     } }

  '?'         { Lexeme { lexemeToken = KW_quest       } }
  '*'         { Lexeme { lexemeToken = KW_star        } }
  ':'         { Lexeme { lexemeToken = KW_colon       } }
  ';'         { Lexeme { lexemeToken = KW_semi        } }
  '{'         { Lexeme { lexemeToken = KW_open_brace  } }
  '}'         { Lexeme { lexemeToken = KW_close_brace } }
  '('         { Lexeme { lexemeToken = KW_open_paren  } }
  ')'         { Lexeme { lexemeToken = KW_close_paren } }
  '<'         { Lexeme { lexemeToken = KW_open_angle  } }
  '>'         { Lexeme { lexemeToken = KW_close_angle } }
  '|'         { Lexeme { lexemeToken = KW_pipe        } }
  '='         { Lexeme { lexemeToken = KW_equals      } }
  ','         { Lexeme { lexemeToken = KW_comma       } }
  '->'        { Lexeme { lexemeToken = KW_arrow       } }

  IDENT       { Lexeme { lexemeToken = Ident          } }


%name parseSpec spec

%tokentype  { Lexeme Token }
%error      { parseError }
%monad      { Parser }
%lexer      { lexerM } { Lexeme { lexemeToken = EOF } }

%%


spec         :: { Spec Parsed }
  : block(decl) { Spec { specAnnot = fst $1, specDecls = snd $1 } }


decl :: { Decl Parsed }

  : 'class' type_name block(class_member)

    { let (es,ms) = partitionEithers (snd $3)
      in DClass ClassDecl { classAnnot   = $1 <-> fst $3
                          , className    = $2
                          , classExtends = es
                          , classMembers = ms
                          } }

  | 'type' type_name '=' type
    { DType TypeDecl { typeDeclAnnot  = $1 <-> $4
                     , typeDeclName   = $2
                     , typeDeclDef    = $4
                     } }

  | namespace_decl
    { case $1 of
        Left n  -> DNamespace n
        Right v -> DValDecl v }



namespace_decl :: { Either (NamespaceDecl Parsed) (ValDecl Parsed) }

  : 'namespace' name block(namespace_decl)
    { let (ns,vs) = partitionEithers (snd $3)
      in Left NamespaceDecl { namespaceAnnot    = $1 <-> fst $3
                            , namespaceName     = $2
                            , namespaceNested   = ns
                            , namespaceMembers  = vs
                            } }

  | val_decl  { Right $1 }



class_member :: { Either Name (ValDecl Parsed) }
  : 'extends' type_name { Left $2  }
  | val_decl            { Right $1 }


generic_vars ::      { [ Name ]   }
  : '<' var_list '>' { reverse $2 }
  |                  { []         }

var_list ::           { [ Name ] }
  : name              { [ $1 ]   }
  | var_list ',' name { $3 : $1  }

val_decl                 :: { ValDecl Parsed }
  : 'mutable' name generic_vars ':' type
                              { ValDecl { valAnnot   = $1 <-> $5
                                        , valName    = $2
                                        , valType    = $5
                                        , valVars    = $3
                                        , valMutable = True } }
  |           name generic_vars ':' type
                              { ValDecl { valAnnot   = $1 <-> $4
                                        , valName    = $1
                                        , valType    = $4
                                        , valVars    = $2
                                        , valMutable = False } }


type_name    :: { Name }
  : IDENT       { name $1 }
  | 'class'     { name $1 }
  | 'namespace' { name $1 }
  | 'extends'   { name $1 }
  | 'type'      { name $1 }
  | 'mutable'   { name $1 }

name         :: { Name }
  : type_name   { $1 }
  | 'boolean'   { name $1 }
  | 'string'    { name $1 }
  | 'number'    { name $1 }
  | 'integer'   { name $1 }




atype                          :: { Type Parsed }
  : 'boolean'                     { tPrim $1 TBoolean }
  | 'string'                      { tPrim $1 TString  }
  | 'number'                      { tPrim $1 TNumber  }
  | 'integer'                     { tPrim $1 TInteger }
  | type_name                     { tUser $1 }
  | opt_mut '{' type '}'          { tArray $1 ($2 <-> $4) $3 }
  | opt_mut '{' type ':' type '}' { tMap $1 ($2 <-> $6) $3 $5 }
  | '(' type ')'                  { $2 }
  | '(' ')'                       { tTuple ($1 <-> $2) [] }
  | '(' tuple_types ')'           { tTuple ($1 <-> $3) $2 }
  -- XXX: RECORDS

opt_mut                        :: { Maybe SourceRange }
  : 'mutable'                     { Just (range $1) }
  | {- empty -}                   { Nothing }

btype                          :: { Type Parsed }
  : atype                         { $1 }
  | atype '?'                     { tMaybe $1 $2 }
  | atype '*'                     { tMany $1 $2 }

ctype                          :: { Type Parsed }
  : btype                         { $1 }
  | ctype '|' btype               { tUnion $1 $3 }

type                           :: { Type Parsed }
  : ctype                         { $1 }
  | ctype '->' type               { tFun $1 $3 }

tuple_types                    :: { [ Type Parsed ] }
  : type ',' type                 { [ $1, $3 ] }
  | type ',' tuple_types          { $1 : $3 }


sep(p,s)
  : {- empty -}               { [] }
  | sep1(p,s)                 { $1 }

sep1(p,s)
  : p                         { [$1] }
  | p s sep1(p,s)             { $1 : $3 }

block(p)
  : '{' sep(p,';') end_block  { ($1 <-> $3, $2) }

end_block                  :: { SourceRange }
  : '}'                       { range $1 }
  | error                     {% onMissingClose }


{
data Parsed
type instance Annot Parsed = SourceRange
type instance TVar  Parsed = Name

name :: Lexeme Token -> Name
name l = Name { nameRange = range l, nameText = lexemeText l }

tPrim :: Lexeme Token -> TCon -> Type Parsed
tPrim a tc = TCon (range a) tc []

tUser :: Name -> Type Parsed
tUser x = TCon (range x) (TUser x) []

tMut :: Maybe SourceRange -> SourceRange -> Type Parsed
tMut mut rng =
  case mut of
    Nothing -> TCon (range (sourceFrom rng)) (TMutable False) []
    Just r  -> TCon r (TMutable True) []

tArray :: Maybe SourceRange -> SourceRange -> Type Parsed -> Type Parsed
tArray mut rng t = TCon (mut ?-> rng) TArray [tMut mut rng, t]

tMap :: Maybe SourceRange -> SourceRange ->
                    Type Parsed -> Type Parsed -> Type Parsed
tMap mut rng k t = TCon (mut ?-> rng) TMap [tMut mut rng, k, t]

tTuple :: SourceRange -> [Type Parsed] -> Type Parsed
tTuple r ts = TCon r (TTuple (length ts)) ts

tFun :: Type Parsed -> Type Parsed -> Type Parsed
tFun s t = TCon (s <-> t) TFun [s,t]

tMaybe :: Type Parsed -> Lexeme Token -> Type Parsed
tMaybe t r = TCon (t <-> r) TMaybe [t]

tMany :: Type Parsed -> Lexeme Token -> Type Parsed
tMany t r = TCon (t <-> r) TMany [t]

tUnion :: Type Parsed -> Type Parsed -> Type Parsed
tUnion t u = TCon (t <-> u) TUnion [t,u]


(?->) :: HasRange a => Maybe SourceRange -> a -> SourceRange
Nothing ?-> y = range y
Just x  ?-> y = x <-> y

instance HasRange (Type Parsed) where
  range (TCon a _ _) = a
  range (TVar _)     = error "[bug] TVar in Parsed"


}

