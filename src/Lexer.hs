--------------------------------------------------------------------
-- | Parsers for individual tokens.
--
-- Module    :  Lexer
-- Copyright :  (c) Ian Kuehne 2016
-- License   :  GPL3
-- Maintainer:  ikuehne@caltech.edu
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Lexer ( typeName
             , parens
             , reserved
             , operator
             , ident
             , integer
             , braces
             , semi 
             , semiSep
             , commaSep ) where

import           Text.Parsec        ((<?>))
import qualified Text.Parsec.Char  as C
import qualified Text.Parsec.Language as Language
import           Text.Parsec.Prim (many)
import           Text.Parsec.String (Parser)
import qualified Text.Parsec.Token    as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser language
  where language = 
          Language.emptyDef {
              Token.commentLine = "#"
            , Token.identStart = C.lower
            , Token.reservedOpNames = ["+", "-", "*", "/", "="]
            , Token.reservedNames = ["type", "int"]
          }

typeName :: Parser String
typeName = lexeme $ do
  firstLetter <- C.upper
  rest        <- many $ Token.identLetter Language.emptyDef
  return $ firstLetter:rest

-- These are just shorthands for the lexemes defined in the language
-- definition.
braces   = Token.braces     lexer
commaSep = Token.commaSep   lexer 
parens   = Token.parens     lexer
reserved = Token.reserved   lexer
integer  = Token.integer    lexer <?> "integer"
operator = Token.reservedOp lexer
lexeme   = Token.lexeme     lexer
semi     = Token.semi       lexer <?> "semicolon"
semiSep  = Token.semiSep    lexer
ident    = Token.identifier lexer <?> "variable name"
