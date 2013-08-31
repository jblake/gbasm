-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.File
where

import Control.Exception
import Data.Generics.Uniplate.Operations
import qualified Text.Parsec as P

import Language.GBAsm.Lexer
import Language.GBAsm.Parser
import Language.GBAsm.Types

-- This is a top-down IO transformation which finds File nodes, reads the
-- referenced file, lexes, parses, and substitutes in the resulting AST.
filePass :: SourceAST -> IO SourceAST filePass (File d file) = flip catch (\e
filePass (File d file) = flip catch (\e -> return $ Err d $ "Could not load file " ++ show file ++ ": " ++ show (e :: SomeException)) $ do

  fileData <- readFile file

  let fileLex = lexer file fileData

  case P.parse fileParser file fileLex of

    -- Ideally, the parser never fails, it just produces internal Err nodes.
    -- But we catch failure anyway, just in case.
    Left e -> return $ Err d $ "Could not parse file " ++ show file ++ ":\n\n" ++ show e

    -- We have to recurse to catch nested references.
    Right parsedAST -> filePass parsedAST

filePass n = descendM filePass n
