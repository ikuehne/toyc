--------------------------------------------------------------------
-- | AST
-- Module    :  Syntax
-- Copyright :  (c) Ian Kuehne 2016
-- License   :  GPL3
-- Maintainer:  ikuehne@caltech.edu
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Syntax ( Expr (..) ) where

data Expr = IntDef { tname :: String
                   , nbits :: Int }
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Div Expr Expr
          | Assign String Expr
          | IntLiteral Integer
          | VarName String
          | FuncDef { retType :: String
                    , name    :: String
                    , args    :: [(String, String)]
                    , body    :: [Expr] }
          | Return Expr
  deriving Show
