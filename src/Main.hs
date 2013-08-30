-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-unused-imports #-}

module Main
where

import Control.DeepSeq
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.Data
import Data.Generics.Uniplate.Operations
import System.Console.CmdArgs.Implicit
import System.Exit

import Language.GBAsm.ByteGen
import Language.GBAsm.CompileOps
import Language.GBAsm.File
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
  , outputFile :: String
  }
  deriving (Data, Eq, Ord, Read, Show, Typeable)

main :: IO ()
main = do

  params <- cmdArgs $ GBAsm
    { inputFiles = [] &= args &= typ "FILE ..."
    , outputFile = "out.gbc" &= explicit &= name "o" &= name "output" &= typFile &= help "The output file to create."
    } &= summary "Assembler for GameBoy"

  let cmdLineSP = SourcePos "<command line>" 0 0
  let initialAST = Scope cmdLineSP [ File cmdLineSP file | file <- inputFiles params ]

  parsedAST <- filePass initialAST

  compiledAST <-
    incBinPass $!!
    compileOpsPass $!!
    mathPass $!!
    unresolvedPass $!!
    localsPass $!!
    globalsPass $!!
    relativePass $!!
    outputPosPass $!!
    mathPass $!!
    unresolvedMacrosPass $!!
    macrosPass $!!
    parsedAST

  case [ (p, msg) | Err (p, _) msg <- universe compiledAST ] of
    [] -> BS.writeFile (outputFile params) $ byteGenPass compiledAST
    errs -> do

      putStrLn "Unable to compile!\n"

      forM_ errs $ \(p, msg) -> putStrLn $ show (sourceName p) ++ " (line " ++ show (sourceLine p) ++ ", column " ++ show (sourceCol p) ++ "):\n  " ++ msg ++ "\n"

      exitFailure
