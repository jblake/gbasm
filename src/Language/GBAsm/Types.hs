-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.Types
where

import Control.DeepSeq
import Data.ByteString.Lazy
import Data.Data
import Data.Generics.Uniplate.Data ()

-- |Arguments passed to an opcode.
-- All possible operands are enumerated here; we pattern match to decide if an opcode is valid.
data Operand
  = Mac !String -- ^Undefined reference to a macro.
  | Gbl !String -- ^Undefined reference to a global address.
  | Loc !String -- ^Undefined reference to a local address.
  | BGbl !String -- ^Undefined reference to the bank of a global address.
  | BLoc !String -- ^Undefined reference to the bank of a local address.
  | Abs !Int -- ^Absolute address.
  | Ind !Operand -- ^Indirect addressing through another operand.
  | Add !Operand !Operand -- ^Compile-time addition.
  | Sub !Operand !Operand -- ^Compile-time subtraction.
  | Mul !Operand !Operand -- ^Compile-time multiplication.
  | Div !Operand !Operand -- ^Compile-time division.
  | Mod !Operand !Operand -- ^Compile-time modulus.
  | LShift !Operand !Operand -- ^Compile-time left-shift.
  | RShift !Operand !Operand -- ^Compile-time right-shift.
  | And !Operand !Operand -- ^Compile-time bitwise and.
  | Xor !Operand !Operand -- ^Compile-time bitwise xor.
  | Or !Operand !Operand -- ^Compile-time bitwise or.
  | Not !Operand -- ^Compile-time bitwise not.
  | Rel !Operand -- ^Relative addressing fixup.
  | AF -- ^16-bit register.
  | BC -- ^16-bit register.
  | DE -- ^16-bit register.
  | HL -- ^16-bit register.
  | SP -- ^16-bit register.
  | NZ -- ^Nonzero flag.
  | NC -- ^Noncarry flag.
  | Z -- ^Zero flag.
  | A -- ^8-bit register.
  | B -- ^8-bit register.
  | C -- ^8-bit register.
  | D -- ^8-bit register.
  | E -- ^8-bit register.
  | H -- ^8-bit register.
  | L -- ^8-bit register.
  deriving (Data, Eq, Ord, Read, Show, Typeable)

instance NFData Operand where
  rnf (Mac l) = rnf l
  rnf (Gbl l) = rnf l
  rnf (Loc l) = rnf l
  rnf (BGbl l) = rnf l
  rnf (BLoc l) = rnf l
  rnf x = x `seq` ()

-- |Prints the syntactical representation of an Operand.
prettyOperand :: Operand -> String
prettyOperand op = pr (0 :: Int) op ""
  where
    pr _ (Mac a)      =                     showChar '@' . showString a
    pr _ (Gbl a)      =                     showString a
    pr _ (Loc a)      =                     showChar '.' . showString a
    pr _ (BGbl a)     =                     showString "bank(" . showString a . showChar ')'
    pr _ (BLoc a)     =                     showString "bank(." . showString a . showChar ')'
    pr _ (Abs a)      =                     shows a
    pr _ (Ind o)      =                     showChar '[' . pr 0 o . showChar ']'
    pr p (LShift l r) = showParen (p > 4) $ pr 4 l . showChar '<' . pr 5 r
    pr p (RShift l r) = showParen (p > 4) $ pr 4 l . showChar '>' . pr 5 r
    pr p (Add    l r) = showParen (p > 5) $ pr 5 l . showChar '+' . pr 6 r
    pr p (Sub    l r) = showParen (p > 5) $ pr 5 l . showChar '-' . pr 6 r
    pr p (Mul    l r) = showParen (p > 6) $ pr 6 l . showChar '*' . pr 7 r
    pr p (Div    l r) = showParen (p > 6) $ pr 6 l . showChar '/' . pr 7 r
    pr p (Mod    l r) = showParen (p > 6) $ pr 6 l . showChar '%' . pr 7 r
    pr p (And    l r) = showParen (p > 3) $ pr 3 l . showChar '&' . pr 4 r
    pr p (Xor    l r) = showParen (p > 2) $ pr 2 l . showChar '^' . pr 3 r
    pr p (Or     l r) = showParen (p > 1) $ pr 1 l . showChar '|' . pr 2 r
    pr _ (Not o)      =                     showChar '~' . pr 7 o
    pr _ (Rel o)      =                     showChar '?' . pr 7 o
    pr _ o            =                     shows o

-- |Abstract representation of the assembler input.
-- The type parameter is to allow for decoration with an annotation type.
data AST dec
  = Scope !dec ![AST dec] -- ^Scoping mark.
  | Macro !dec !String !Operand -- ^Macro.
  | Global !dec !String -- ^Global label.
  | Local !dec !String -- ^Local label.
  | OutBank !dec !Operand -- ^Position declaration with only bank.
  | OutPos !dec !Operand !Operand -- ^Position declaration with bank and address.
  | Raw !dec ![Operand] -- ^Embedded literals.
  | Bin !dec !ByteString -- ^Embedded bytestring.
  | Inc !dec !String !Operand !Operand -- ^Include an external file, with offset and length.
  | Op0 !dec !String -- ^Nullary opcode.
  | Op1 !dec !String !Operand -- ^Unary opcode.
  | Op2 !dec !String !Operand !Operand -- ^Binary opcode.
  | Err !dec !String -- ^Embedded errors.
  deriving (Data, Eq, Functor, Ord, Read, Show, Typeable)

instance (NFData dec) => NFData (AST dec) where
  rnf (Scope d a) = rnf d `seq` rnf a
  rnf (Macro d l a) = rnf d `seq` rnf l `seq` rnf a
  rnf (Global d l) = rnf d `seq` rnf l
  rnf (Local d l) = rnf d `seq` rnf l
  rnf (OutBank d b) = rnf d `seq` rnf b
  rnf (OutPos d b a) = rnf d `seq` rnf b `seq` rnf a
  rnf (Raw d os) = rnf d `seq` rnf os
  rnf (Bin d bs) = rnf d `seq` rnf bs
  rnf (Inc d f o l) = rnf d `seq` rnf f `seq` rnf o `seq` rnf l
  rnf (Op0 d o) = rnf d `seq` rnf o
  rnf (Op1 d o a) = rnf d `seq` rnf o `seq` rnf a
  rnf (Op2 d o a b) = rnf d `seq` rnf o `seq` rnf a `seq` rnf b
  rnf (Err d m) = rnf d `seq` rnf m

-- |Undecorated AST.
type PlainAST = AST ()

-- |Source code positions.
data SourcePos = SourcePos
  { sourceName :: !String -- ^Name of the source file.
  , sourceLine :: !Int -- ^Line of the source code.
  , sourceCol :: !Int -- ^Column of the source code.
  }
  deriving (Data, Eq, Ord, Read, Show, Typeable)

instance NFData SourcePos where
  rnf (SourcePos n l c) = rnf n `seq` rnf l `seq` rnf c

-- |Source code position decorated AST.
type SourceAST = AST SourcePos

-- |Code output positions.
data OutputPos = OutputPos
  { outputBank    :: !Int -- ^Index of the output bank.
  , outputAddress :: !Int -- ^Address within the output segment.
  , outputSize    :: !Int -- ^The size of this AST node. Note that we only set this on nodes which actually generate output; in particular, Scope nodes have outputSize == 0.
  }
  deriving (Data, Eq, Ord, Read, Show, Typeable)

instance NFData OutputPos where
  rnf (OutputPos s a z) = rnf s `seq` rnf a `seq` rnf z

-- |Output position decorated AST.
type OutputAST = AST OutputPos

-- |AST decorated by both source and output positions.
type FullAST = AST (SourcePos, OutputPos)

-- |Helper function for increasing OutputPos.
incrOutputPos :: Int -> OutputPos -> OutputPos
incrOutputPos sz op = op { outputAddress = outputAddress op + sz }
