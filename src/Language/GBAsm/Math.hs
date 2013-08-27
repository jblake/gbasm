-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.Math
where

import Data.Bits
import Data.Generics.Uniplate.Operations

import Language.GBAsm.Types

-- |Simplify compile-time math in operands as much as possible.
mathPass :: (Biplate a Operand) => a -> a
mathPass = transformBi simpl

  where

    simpl :: Operand -> Operand
    simpl (Add    (Abs l) (Abs r))          = Abs $ l + r
    simpl (Sub    (Abs l) (Abs r))          = Abs $ l - r
    simpl (Mul    (Abs l) (Abs r))          = Abs $ l * r
    simpl (Div    (Abs l) (Abs r)) | r /= 0 = Abs $ l `div` r
    simpl (Mod    (Abs l) (Abs r)) | r /= 0 = Abs $ l `mod` r
    simpl (LShift (Abs l) (Abs r))          = Abs $ l `shiftL` r
    simpl (RShift (Abs l) (Abs r))          = Abs $ l `shiftR` r
    simpl (And    (Abs l) (Abs r))          = Abs $ l .&. r
    simpl (Xor    (Abs l) (Abs r))          = Abs $ l `xor` r
    simpl (Or     (Abs l) (Abs r))          = Abs $ l .|. r
    simpl (Not    (Abs o))                  = Abs $ complement o
    simpl o                                 = o
