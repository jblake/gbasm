-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.Relative
where

import Data.Generics.Uniplate.Operations

import Language.GBAsm.Types

-- |Transform operands by replacing relative addressing with subtraction operations on absolute addresses.
relativePass :: FullAST -> FullAST
relativePass = transform fixAST
  where

    fixAST (Raw dec@(_,pos) ops)    = Raw dec $ map (tOperand pos) ops
    fixAST (Inc dec@(_,pos) fn a b) = Inc dec fn (tOperand pos a) (tOperand pos b)
    fixAST (Op1 dec@(_,pos) op a)   = Op1 dec op (tOperand pos a)
    fixAST (Op2 dec@(_,pos) op a b) = Op2 dec op (tOperand pos a) (tOperand pos b)
    fixAST n                        = n

    tOperand pos = transform fixOperand
      where
        fixOperand (Rel a) = Sub a $ Abs $ outputAddress pos + outputSize pos
        fixOperand o       = o
