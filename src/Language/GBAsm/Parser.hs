-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.Parser
where

import Control.Applicative
import Data.Char
import Data.Functor.Identity
import Text.Parsec as P hiding
  ( many
  , (<|>)
  )
import Text.Parsec.Expr

import Language.GBAsm.Lexer
import Language.GBAsm.Types as GBA

-- |Helper for getting the current position.
pos :: (Monad m) => ParsecT s u m GBA.SourcePos
pos = do
  p <- getPosition
  return $ GBA.SourcePos
    { GBA.sourceName = P.sourceName p
    , GBA.sourceLine = P.sourceLine p
    , GBA.sourceCol  = P.sourceColumn p
    }

-- |The type of these parsers.
type P = ParsecT Lex () Identity

-- |Keyword operands.
opKeyword :: P Operand
opKeyword = choice
  [ kw "af" >> return AF
  , kw "bc" >> return BC
  , kw "de" >> return DE
  , kw "hl" >> return HL
  , kw "sp" >> return SP
  , kw "nz" >> return NZ
  , kw "nc" >> return NC
  , kw "z"  >> return Z
  , kw "a"  >> return A
  , kw "b"  >> return B
  , kw "c"  >> return C
  , kw "d"  >> return D
  , kw "e"  >> return E
  , kw "h"  >> return H
  , kw "l"  >> return L
  ]

-- |Macro reference.
opMac :: P Operand
opMac = Mac <$> (sym '@' *> bw)

-- |Global reference.
opGbl :: P Operand
opGbl = Gbl <$> bw

-- |Local reference.
opLoc :: P Operand
opLoc = Loc <$> (sym '.' *> bw)

-- |Global bank reference.
opBGbl :: P Operand
opBGbl = BGbl <$> (kw "bank" *> sym '(' *> bw <* sym ')')

-- |Local bank reference.
opBLoc :: P Operand
opBLoc = BLoc <$> (kw "bank" *> sym '(' *> sym ',' *> bw <* sym ')')

-- |Absolute address.
opAbs :: P Operand
opAbs = Abs <$> num

-- |Indirect operand.
opInd :: P Operand
opInd = Ind <$> (sym '[' *> operand <* sym ']')

-- |Parenthesis.
opParen :: P Operand
opParen = sym '(' *> operand <* sym ')'

-- |Any term operand.
opTerm :: P Operand
opTerm = choice $ map try
  [ opKeyword
  , opMac
  , opGbl
  , opLoc
  , opBGbl
  , opBLoc
  , opAbs
  , opInd
  , opParen
  ]

-- |Any operand.
operand :: P Operand
operand = flip buildExpressionParser opTerm
  [ [ Prefix $ sym '~' *> pure Not
    , Prefix $ sym '?' *> pure Rel
    ]
  , [ flip Infix AssocLeft $ sym '*' *> pure Mul
    , flip Infix AssocLeft $ sym '/' *> pure Div
    , flip Infix AssocLeft $ sym '%' *> pure Mod
    ]
  , [ flip Infix AssocLeft $ sym '+' *> pure Add
    , flip Infix AssocLeft $ sym '-' *> pure Sub
    ]
  , [ flip Infix AssocLeft $ sym '<' *> pure LShift
    , flip Infix AssocLeft $ sym '>' *> pure RShift
    ]
  , [ flip Infix AssocLeft $ sym '&' *> pure And
    ]
  , [ flip Infix AssocLeft $ sym '^' *> pure Xor
    ]
  , [ flip Infix AssocLeft $ sym '|' *> pure Or
    ]
  ]

-- |Include external file as assembly.
astFile :: P SourceAST
astFile = File <$> pos <*> (kw "include" *> str <* nl)

-- |Scoping marks.
astScope :: P SourceAST
astScope = Scope <$> pos <*> (sym '{' *> many (ast <|> astErrNotScope) <* skipMany nl <* sym '}')

-- |Macro definition.
astMacro :: P SourceAST
astMacro = Macro <$> pos <*> (sym '@' *> bw) <*> (sym '=' *> operand <* nl)

-- |Global label.
astGlobal :: P SourceAST
astGlobal = Global <$> pos <*> (bw <* sym ':')

-- |Local label.
astLocal :: P SourceAST
astLocal = Local <$> pos <*> (sym '.' *> bw)

-- |Bank switch.
astBank :: P SourceAST
astBank = OutBank <$> pos <*> (kw "bank" *> sym '=' *> operand <* nl)

-- |Address switch.
astPos :: P SourceAST
astPos = OutPos <$> pos <*> (kw "bank" *> sym '=' *> operand <* sym ',') <*> (kw "addr" *> sym '=' *> operand <* nl)

-- |Raw bytes.
astByte :: P SourceAST
astByte = Raw <$> pos <*> (kw "byte" *> sepBy operand (sym ',') <* nl)

-- |Raw words.
astWord :: P SourceAST
astWord = Raw <$> pos <*> (kw "word" *> (mkWords <$> sepBy operand (sym ',')) <* nl)
  where
    mkWords [] = []
    mkWords (w:ws) = RShift w (Abs 8) : w : mkWords ws

-- |ASCII string.
astASCII :: P SourceAST
astASCII = Raw <$> pos <*> (kw "ascii" *> (map (Abs . ord) <$> str) <* nl)

-- |Include external file as binary.
astIncBin :: P SourceAST
astIncBin = Inc <$> pos <*> (kw "incbin" *> str <* sym ',') <*> (operand <* sym ',') <*> (operand <* nl)

-- |Nullary opcode.
astOp0 :: P SourceAST
astOp0 = Op0 <$> pos <*> (bw <* nl)

-- |Unary opcode.
astOp1 :: P SourceAST
astOp1 = Op1 <$> pos <*> bw <*> (operand <* nl)

-- |Binary opcode.
astOp2 :: P SourceAST
astOp2 = Op2 <$> pos <*> bw <*> (operand <* sym ',') <*> (operand <* nl)

-- |Parser error.
astErr :: P SourceAST
astErr = Err <$> pos <*> (("Unexpected lexeme during parse: " ++) . ppLexeme <$> next)

-- |Parser error, but *not* the '}' symbol.
-- We need this so that astScope gets a chance to find its closing brace.
astErrNotScope :: P SourceAST
astErrNotScope = Err <$> pos <*> (("Unexpected lexeme during parse: " ++) . ppLexeme <$> nextNotSym '}')

-- |Any AST node.
ast :: P SourceAST
ast = try $ skipMany nl *> (choice $ map try
  [ astFile
  , astScope
  , astMacro
  , astGlobal
  , astLocal
  , astBank
  , astPos
  , astByte
  , astWord
  , astASCII
  , astIncBin
  , astOp0
  , astOp1
  , astOp2
  ])

-- |The complete file parser.
fileParser :: P SourceAST
fileParser = Scope <$> pos <*> (many (ast <|> astErr) <* skipMany nl <* eof)
