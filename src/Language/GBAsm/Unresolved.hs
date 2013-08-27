-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.Unresolved
where

import Data.Generics.Uniplate.Operations
import Data.List

import Language.GBAsm.Types

-- |We transform AST nodes, such that any node with an Operand that is unresolved is replaced.
unresolvedPass :: FullAST -> FullAST
unresolvedPass = transform removeRefs

  where

    removeRefs a@(Op1 d _ l) =
      case [ n | Gbl  n <- universe l ] ++ [ '.' : n | Loc  n <- universe l ] ++
           [ n | BGbl n <- universe l ] ++ [ ',' : n | BLoc n <- universe l ] of
        [] -> a
        ns -> Err d $ "Unresolved references: " ++ intercalate ", " ns

    removeRefs a@(Op2 d _ l r) =
      case [ n | Gbl  n <- universe l ] ++ [ '.' : n | Loc  n <- universe l ] ++
           [ n | Gbl  n <- universe r ] ++ [ '.' : n | Loc  n <- universe r ] ++
           [ n | BGbl n <- universe l ] ++ [ '.' : n | BLoc n <- universe l ] ++
           [ n | BGbl n <- universe r ] ++ [ '.' : n | BLoc n <- universe r ] of
        [] -> a
        ns -> Err d $ "Unresolved references: " ++ intercalate ", " ns

    removeRefs a = a
