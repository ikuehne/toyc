--------------------------------------------------------------------
--
-- Module    :  Parser
-- Copyright :  (c) Ian Kuehne 2016
-- License   :  GPL3
-- Maintainer:  ikuehne@caltech.edu
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Parser ( parse ) where

import           Control.Monad      (liftM2)
import           Data.Functor       ((<$>))

import           Text.Parsec        ((<?>), (<|>), many)
import           Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Expr

import Lexer
import Syntax

-- | Top-level parser.
parse :: Parser Expr
parse = func

------------------------------------------------------------------------------
-- Parsing function definitions.
------------------------------------------------------------------------------

-- | Parse a full function definition.
func :: Parser Expr
func = do retType <- typeName
          name <- ident
          args <- parens argList
          body <- braces . many $ do
            expr <- arithmeticExpr
            semi
            return expr
          return $ FuncDef retType name args body
  where argList = commaSep arg
        arg = liftM2 (,) typeName ident

------------------------------------------------------------------------------
-- Parsing arithmetic expressions.
------------------------------------------------------------------------------

-- | Parse an arithmetic expression with infix operators.
arithmeticExpr :: Parser Expr
arithmeticExpr = Expr.buildExpressionParser opTable term

-- | Parse a sub-expression to be separated by infix operators.
-- 
-- Mutually recursive with @arithmeticExpr@.
term :: Parser Expr
term =  parens arithmeticExpr
    <|> IntLiteral <$> integer
    <|> VarName <$> ident

arithmeticOp s c = Expr.Infix (operator s >> return c) Expr.AssocLeft

opTable = [ [ arithmeticOp "*" Times
            , arithmeticOp "/" Div ]
          , [ arithmeticOp "+" Plus
            , arithmeticOp "-" Minus ] ]

------------------------------------------------------------------------------
-- Parsing type declarations.
------------------------------------------------------------------------------

-- | Parse a "type type", which determines how a user-defined type will be
-- interpreted.
typeType :: Parser (String -> Int -> Expr)
typeType = reserved "int" >> return IntDef

-- | Parse a type definition.
intDef :: Parser Expr
intDef =
  do reserved "type"
     name <- typeName
     operator "="
     tt <- typeType
     nbits <- integer
     semi
     return $ tt name (fromIntegral nbits)
