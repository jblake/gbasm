-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.Locals
where

import Data.Generics.Uniplate.Operations
import qualified Data.Map as M

import Language.GBAsm.Types

-- |This follows approximately the same pattern as globals: collect labels,
-- replace duplicate definitions with errors, replace references. We have to do
-- a little bit of extra work because we're tracking nested scopes at the same
-- time; an inner scope can override labels from an outer scope. Note that the
-- parser inserts a Scope node at the root of the AST.
localsPass :: FullAST -> FullAST
localsPass = withScope M.empty
  where

    withScope parentEnv (Scope dec ns) = Scope dec $ map (withScope myEnv) $ map resolve $ map removeDups ns
      where

        -- The reverse here is so that we catch the first definition of a
        -- label, and identify all further definitions as duplicates.
        localEnv = M.fromList $ reverse [ (l, d) | Local d l <- ns ]

        myEnv = localEnv `M.union` parentEnv

        -- Change this to reference myEnv if you don't want inner scopes to be
        -- able to override outer scopes.
        removeDups (Local d l) | localEnv M.! l /= d = Err d $ "Duplicate definition of local label " ++ show l ++ " (previous definition on line " ++ show (sourceLine $ fst $ localEnv M.! l) ++ ")"
        removeDups n = n

        -- It's important that we don't recurse into nested Scopes here; we'll
        -- catch them with the recursive call to withScope.
        resolve s@(Scope _ _) = s
        resolve n             = transformBi resolveLoc n

        resolveLoc (Loc  l) | l `M.member` myEnv = Abs $ outputAddress $ snd $ myEnv M.! l
        resolveLoc (BLoc l) | l `M.member` myEnv = Abs $ outputBank $ snd $ myEnv M.! l
        resolveLoc o = o

    withScope _ n = n
