{-# Language OverloadedStrings #-}
module Galua.Micro.Translate.Simplify where


import Galua.Code
import Galua.Micro.AST

simplify :: MicroFunction -> MicroFunction
simplify f = f { functionCode = fmap simplifyBlock (functionCode f) }

simplifyBlock :: Block -> Block
simplifyBlock b = b { blockEnd = simplifyBlockStmt simpEnd (blockEnd b) }

simplifyBlockStmt :: (a -> a) -> BlockStmt a -> BlockStmt a
simplifyBlockStmt f b = b { stmtCode = f (stmtCode b) }

simpEnd :: EndStmt -> EndStmt
simpEnd stmt =
  case stmt of
    Case (ELit l) as d ->
      case lookup (literalType l) as of
        Just l1 -> Goto l1
        Nothing -> case d of
                     Just l1 -> Goto l1
                     Nothing -> Raise (ELit (LStr "pattern match failure"))

    _ -> stmt
