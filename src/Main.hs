-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-unused-imports #-}

module Main
where

import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.Data
import Data.Generics.Uniplate.Operations
import System.Console.CmdArgs.Implicit
import System.Exit
import qualified Text.Parsec as P

import Language.GBAsm.ByteGen
import Language.GBAsm.CompileOps
import Language.GBAsm.Globals
import Language.GBAsm.IncBin
import Language.GBAsm.Lexer
import Language.GBAsm.Locals
import Language.GBAsm.Math
import Language.GBAsm.Macros
import Language.GBAsm.Opcodes
import Language.GBAsm.OutputPos
import Language.GBAsm.Parser
import Language.GBAsm.Relative
import Language.GBAsm.Types
import Language.GBAsm.Unresolved
import Language.GBAsm.UnresolvedMacros

data GBAsm = GBAsm
  { inputFiles :: [String]
  }
  deriving (Data, Eq, Ord, Read, Show, Typeable)

main :: IO ()
main = do

  params <- cmdArgs $ GBAsm
    { inputFiles = [] &= args
    }

  forM_ (inputFiles params) $ \file -> do

    fd <- readFile file

    let lexed = lexer file fd

    case P.parse fileParser file lexed of
      Left e -> do
        putStrLn $ "Fatal parsing error: (this shouldn't happen!)\n\n" ++ show e
        exitFailure
      Right a -> do

        a' <- incBinPass $ compileOpsPass $ mathPass $ unresolvedPass $ localsPass $ globalsPass $ relativePass $ outputPosPass $ unresolvedMacrosPass $ macrosPass $ mathPass a

        case [ (p, msg) | Err (p, _) msg <- universe a' ] of
          [] -> BS.writeFile (file ++ ".bin") $ byteGenPass a'
          errs -> do

            putStrLn $ "Unable to compile " ++ show file ++ ".\n"

            forM_ errs $ \(p, msg) -> putStrLn $ show (sourceName p) ++ " (line " ++ show (sourceLine p) ++ ", column " ++ show (sourceCol p) ++ "):\n  " ++ msg ++ "\n"

            exitFailure
