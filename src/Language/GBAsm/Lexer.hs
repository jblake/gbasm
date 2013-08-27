-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.Lexer
where

import Control.Monad
import Data.Char
import Data.Data
import Data.List
import Text.Parsec.Pos
import Text.Parsec.Prim

-- |The primitime lexemes.
data Lexeme
  = NL -- ^Newline.
  | N !Int -- ^Numeric literal.
  | S !Char -- ^Symbol.
  | W !String -- ^Bareword.
  | Q !String -- ^Quoted string.
  | LexE !Char -- ^Unlexable character.
  deriving (Data, Eq, Ord, Read, Show, Typeable)

-- |For pretty-printing.
ppLexeme :: Lexeme -> String
ppLexeme NL = "newline"
ppLexeme (N i) = "numeric literal " ++ show i
ppLexeme (S c) = "symbol " ++ show c
ppLexeme (W w) = "bareword " ++ show w
ppLexeme (Q s) = "string " ++ show s
ppLexeme (LexE c) = "unknown character " ++ show c

-- |When we run the lexer, we do so with this continuation structure.
data Lex
  = LexDone !SourcePos -- ^No more lexemes.
  | LexIs !Lexeme !SourcePos Lex -- ^A lexeme, and a continuation to build more lexemes.
  deriving (Data, Typeable)

-- |Lex is a coherent Stream for Parsec.
instance (Monad m) => Stream Lex m Lexeme where

  uncons (LexDone _) = return Nothing
  uncons (LexIs t _ l) = return $ Just (t, l)

-- |Get next position from lex.
getPos :: SourcePos -> Lexeme -> Lex -> SourcePos
getPos _ _ (LexDone sp) = sp
getPos _ _ (LexIs _ sp _) = sp

-- |Get the next lexeme.
next :: (Monad m) => ParsecT Lex u m Lexeme
next = tokenPrim ppLexeme getPos return

-- |Get a newline.
nl :: (Monad m) => ParsecT Lex u m ()
nl = tokenPrim ppLexeme getPos $ \t -> do { NL <- Just t; return () }

-- |Get a numeric literal.
num :: (Monad m) => ParsecT Lex u m Int
num = tokenPrim ppLexeme getPos $ \t -> do { N i <- Just t; return i }

-- |Match a particular symbol.
sym :: (Monad m) => Char -> ParsecT Lex u m ()
sym s = tokenPrim ppLexeme getPos $ \t -> do { S s' <- Just t; guard $ s' == s }

-- |Match anything but a particular symbol.
nextNotSym :: (Monad m) => Char -> ParsecT Lex u m Lexeme
nextNotSym s = tokenPrim ppLexeme getPos $ \t -> if t == S s then Nothing else Just t

-- |Match a particular keyword.
kw :: (Monad m) => String -> ParsecT Lex u m ()
kw s = tokenPrim ppLexeme getPos $ \t -> do { W s' <- Just t; guard $ map toLower s' == map toLower s }

-- |Get a bareword.
bw :: (Monad m) => ParsecT Lex u m String
bw = tokenPrim ppLexeme getPos $ \t -> do { W s <- Just t; return s }

-- |Get a quoted string.
str :: (Monad m) => ParsecT Lex u m String
str = tokenPrim ppLexeme getPos $ \t -> do { Q s <- Just t; return s }

-- |Helper function for parsing integers written in an arbitrary base.
fromBase :: [Char] -> String -> Int
fromBase cs l = fb $ reverse l
  where
    fb []     = 0
    fb (d:ds) =
      case findIndex (== d) cs of
        Nothing -> error $ "Invalid digit " ++ show d ++ " in fromBase " ++ show cs
        Just i -> (fb ds * length cs) + i

-- |The lexer.
lexer :: String -> String -> Lex
lexer name = rl $ initialPos name
  where

    -- End-of-input.
    rl sp ""                          = LexDone sp

    -- Newlines.
    rl sp ('\n':s)                    = let
                                        np = updatePosChar sp '\n'
                                        in LexIs NL sp $ rl np s

    -- Other whitespace.
    rl sp (c:s)    | isSpace c        = rl (updatePosChar sp c) s

    -- Comments.
    rl sp (';':s)                     = let
                                        cs = takeWhile (/= '\n') s
                                        s' = drop (length cs) s
                                        in rl (updatePosString sp $ ';':cs) s'

    -- Base-2 numbers.
    rl sp ('\'':s)                    = let
                                        ds = takeWhile (\c -> c `elem` "01") s 
                                        s' = drop (length ds) s
                                        np = updatePosString sp $ '\'':ds
                                        in LexIs (N $ fromBase "01" ds) sp $ rl np s'

    -- Base-4 numbers.
    rl sp ('`':s)                     = let
                                        ds = takeWhile (\c -> c `elem` "0123") s
                                        s' = drop (length ds) s
                                        np = updatePosString sp $ '`':ds
                                        in LexIs (N $ fromBase "0123" ds) sp $ rl np s'

    -- Base-8 numbers.
    rl sp ('0':s)                     = let
                                        ds = takeWhile isOctDigit s
                                        s' = drop (length ds) s
                                        np = updatePosString sp $ '0':ds
                                        in LexIs (N $ fromBase "01234567" ds) sp $ rl np s'

    -- Base-10 numbers.
    rl sp s@(c:_)  | isDigit c        = let
                                        ds = takeWhile isDigit s
                                        s' = drop (length ds) s
                                        np = updatePosString sp ds
                                        in LexIs (N $ fromBase "0123456789" ds) sp $ rl np s'

    -- Base-16 numbers.
    rl sp ('$':s)                     = let
                                        ds = takeWhile isHexDigit s
                                        s' = drop (length ds) s
                                        np = updatePosString sp $ '$':ds
                                        in LexIs (N $ fromBase "0123456789abcdef" $ map toLower ds) sp $ rl np s'

    -- Quoted strings.
    rl sp ('"':s)                     = let
                                        cs = takeWhile (/= '"') s
                                        s' = drop (length cs + 1) s
                                        np = updatePosString sp $ '"':'"':cs
                                        in LexIs (Q cs) sp $ rl np s'

    -- Symbols.
    rl sp (c:s)    | isPunctuation c  = let
                                        np = updatePosChar sp c
                                        in LexIs (S c) sp $ rl np s
                   | isSymbol c       = let
                                        np = updatePosChar sp c
                                        in LexIs (S c) sp $ rl np s

    -- Barewords.
    rl sp (c:s)    | isAlpha c        = let
                                        cs = takeWhile isAlphaNum s
                                        s' = drop (length cs) s
                                        np = updatePosString sp $ c:cs
                                        in LexIs (W $ c:cs) sp $ rl np s'

    -- Anything else.
    rl sp (c:s)                       = let
                                        np = updatePosChar sp c
                                        in LexIs (LexE c) sp $ rl np s
