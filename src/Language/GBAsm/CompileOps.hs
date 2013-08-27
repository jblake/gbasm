-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.CompileOps
where

import qualified Data.ByteString.Lazy as BS
import Data.Generics.Uniplate.Operations
import Data.List

import Language.GBAsm.Opcodes
import Language.GBAsm.Types

-- |Transform opcodes with resolved references into individual Bin nodes.
compileOpsPass :: FullAST -> FullAST
compileOpsPass = transform compileOp
  where

    compileOp (Raw dec os) =
      let
        vals = [ fromIntegral n | Abs n <- os ]
      in if length vals == length os
        then Bin dec $ BS.pack vals
        else Err dec $ "Raw opcode list cannot be resolved to constants: " ++ intercalate ", " (map prettyOperand os)

    compileOp (Op0 dec op) =
      case (opc op) of
        Nothing -> Err dec $ "Unrecognized nullary opcode: " ++ op
        Just bs -> Bin dec bs

    compileOp (Op1 dec op a) =
      case (opc op a) of
        Nothing -> Err dec $ "Unrecognized unary opcode: " ++ op ++ " " ++ prettyOperand a
        Just bs -> Bin dec bs

    compileOp (Op2 dec op a b) =
      case (opc op a b) of
        Nothing -> Err dec $ "Unrecognized binary opcode: " ++ op ++ " " ++ prettyOperand a ++ ", " ++ prettyOperand b
        Just bs -> Bin dec bs

    compileOp n = n
