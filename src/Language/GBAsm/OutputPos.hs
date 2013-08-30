-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.OutputPos
where

import Control.Monad.Trans.State
import qualified Data.ByteString.Lazy as BS
import Data.Generics.Uniplate.Operations
import qualified Data.IntMap as M

import Language.GBAsm.Math
import Language.GBAsm.Opcodes
import Language.GBAsm.Types

-- |This transforms Operands such that there are no unresolved references.
-- Note that we just substitute (Abs 0xff00), so you can only use the result as a way to compute lengths.
-- Why that particular address? Because some opcodes only work on high addresses.
-- It's important that you *not* do this to OutBank/OutPos nodes.
fakeOperands :: (Biplate a Operand) => a -> a
fakeOperands = mathPass . transformBi replaceRef
  where
    replaceRef (Gbl  _) = Abs 0xff00
    replaceRef (Loc  _) = Abs 0xff00
    replaceRef (BGbl _) = Abs 0xff00
    replaceRef (BLoc _) = Abs 0xff00
    replaceRef (Rel  o) = o
    replaceRef o        = o

-- |Running state for this pass.
data ST = ST
  { here  :: !OutputPos
  , banks :: !(M.IntMap OutputPos)
  }
  deriving (Read, Show)

-- |Initial state.
newST :: ST
newST = ST
  { here = OutputPos
    { outputBank    = 0
    , outputAddress = 0
    , outputSize    = 0
    }
  , banks = M.empty
  }

-- |The actual pass
outputPosPass :: SourceAST -> FullAST
outputPosPass theAST = evalState (pass theAST) newST

  where

    pass :: SourceAST -> State ST FullAST

    pass (Scope dec asts) = do
      pos <- gets here
      asts' <- mapM pass asts
      return $ Scope (dec, pos) asts'

    pass (Macro dec label addr) = do
      pos <- gets here
      return $ Macro (dec, pos) label addr

    pass (Global dec label) = do
      pos <- gets here
      return $ Global (dec, pos) label

    pass (Local dec label) = do
      pos <- gets here
      return $ Local (dec, pos) label

    pass (OutBank dec (Abs bank)) = do
      pos <- gets $ M.findWithDefault (OutputPos
          { outputBank    = bank
          , outputAddress = if bank == 0 then 0 else 0x4000
          , outputSize    = 0
          }) bank . banks
      modify $ \st -> st { here = pos, banks = M.insert (outputBank $ here st) (here st) (banks st) }
      return $ OutPos (dec, pos) (Abs bank) (Abs $ outputAddress pos)

    pass (OutBank dec bank) = do
      pos <- gets here
      return $ Err (dec, pos) $ "Unable to resolve position " ++ prettyOperand bank ++ ":* to a constant address."

    pass (OutPos dec (Abs bank) (Abs addr)) = do
      let
        pos = OutputPos
          { outputBank    = bank
          , outputAddress = addr
          , outputSize    = 0
          }
      modify $ \st -> st { here = pos, banks = M.insert (outputBank $ here st) (here st) (banks st) }
      return $ OutPos (dec, pos) (Abs bank) (Abs addr)

    pass (OutPos dec bank op) = do
      pos <- gets here
      return $ Err (dec, pos) $ "Unable to resolve position " ++ prettyOperand bank ++ ":" ++ prettyOperand op ++ " to a constant address."

    pass (Raw dec os) = do
      pos <- gets here
      let sz = length os
      modify $ \st -> st { here = incrOutputPos sz pos }
      return $ Raw (dec, pos { outputSize = sz }) os

    pass (Bin dec bs) = do
      pos <- gets here
      modify $ \st -> st { here = incrOutputPos (fromIntegral $ BS.length bs) pos }
      return $ Bin (dec, pos { outputSize = fromIntegral $ BS.length bs }) bs

    pass (Inc dec file offset lo@(Abs len)) = do
      pos <- gets here
      modify $ \st -> st { here = incrOutputPos len pos }
      return $ Inc (dec, pos { outputSize = len }) file offset lo

    pass (Inc dec file _ lo) = do
      pos <- gets here
      return $ Err (dec, pos) $ "Unable to resolve length " ++ prettyOperand lo ++ " to a constant for inclusion of " ++ show file ++ "."

    pass (Op0 dec op) = do
      pos <- gets here
      case (opc op) of
        Nothing -> return $ Err (dec, pos) $ "Unrecognized nullary opcode: " ++ op
        Just bs -> do
          modify $ \st -> st { here = incrOutputPos (fromIntegral $ BS.length bs) pos }
          return $ Op0 (dec, pos { outputSize = fromIntegral $ BS.length bs }) op

    pass (Op1 dec op a) = do
      pos <- gets here
      case (opc op (fakeOperands a)) of
        Nothing -> return $ Err (dec, pos) $ "Unrecognized unary opcode: " ++ op ++ " " ++ prettyOperand a
        Just bs -> do
          modify $ \st -> st { here = incrOutputPos (fromIntegral $ BS.length bs) pos }
          return $ Op1 (dec, pos { outputSize = fromIntegral $ BS.length bs }) op a

    pass (Op2 dec op a b) = do
      pos <- gets here
      case (opc op (fakeOperands a) (fakeOperands b)) of
        Nothing -> return $ Err (dec, pos) $ "Unrecognized binary opcode: " ++ op ++ " " ++ prettyOperand a ++ ", " ++ prettyOperand b
        Just bs -> do
          modify $ \st -> st { here = incrOutputPos (fromIntegral $ BS.length bs) pos }
          return $ Op2 (dec, pos { outputSize = fromIntegral $ BS.length bs }) op a b

    pass (Err dec msg) = do
      pos <- gets here
      return $ Err (dec, pos) msg
