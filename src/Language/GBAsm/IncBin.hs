-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.IncBin
where

import Control.Exception
import Data.ByteString.Lazy as BS
import Data.Generics.Uniplate.Operations

import Language.GBAsm.Types

-- |Walk an AST, replacing Inc nodes with Bin nodes.
incBinPass :: FullAST -> IO FullAST
incBinPass = transformM pass

  where

    pass (Inc dec name (Abs offset) (Abs len)) = do
      flip catch (\e -> return $ Err dec $ "The file " ++ show name ++ " could not be read: " ++ show (e :: SomeException)) $ do
        fdata <- BS.readFile name
        let fdata' = BS.take (fromIntegral len) $ BS.drop (fromIntegral offset) fdata
        if BS.length fdata' /= fromIntegral len
          then return $ Err dec $ "The file " ++ show name ++ ", starting at offset " ++ show offset ++ ", does not have at least " ++ show len ++ " bytes."
          else return $ Bin dec fdata'

    pass (Inc dec name offset len) = return $ Err dec $ "Unable to resolve offset " ++ prettyOperand offset ++ " and/or length " ++ prettyOperand len ++ " to constants for inclusion of file " ++ show name ++ "."

    pass n = return n
