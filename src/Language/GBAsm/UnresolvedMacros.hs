-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.UnresolvedMacros
where

import Data.Generics.Uniplate.Operations
import Data.List

import Language.GBAsm.Types

-- |We transform AST nodes, such that any node with an Operand that is unresolved is replaced.
unresolvedMacrosPass :: SourceAST -> SourceAST
unresolvedMacrosPass = transform removeRefs

  where

    removeRefs a@(Op1 d _ l) =
      case [ '@' : n | Mac n <- universe l ] of
        [] -> a
        ns -> Err d $ "Undefined macros: " ++ intercalate ", " ns

    removeRefs a@(Op2 d _ l r) =
      case [ '@' : n | Mac n <- universe l ] ++
           [ '@' : n | Mac n <- universe r ] of
        [] -> a
        ns -> Err d $ "Undefined macros: " ++ intercalate ", " ns

    removeRefs a = a
