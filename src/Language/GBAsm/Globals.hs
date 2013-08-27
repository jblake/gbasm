-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.Globals
where

import Data.Generics.Uniplate.Operations
import qualified Data.Map as M

import Language.GBAsm.Types

-- |First we collect all the global labels, then we replace duplicate
-- definitions with errors, then we replace references.
globalsPass :: FullAST -> FullAST
globalsPass ast = transformBi resolveGbl $ transform removeDups ast

  where

    -- The reverse here is so that we catch the first definition of a label,
    -- and identify all further definitions as duplicates.
    gblMap = M.fromList $ reverse [ (l, d) | Global d l <- universe ast ]

    removeDups (Global d l) | gblMap M.! l /= d = Err d $ "Duplicate definition of global label " ++ show l ++ " (previous definition on line " ++ show (sourceLine $ fst $ gblMap M.! l) ++ ")"
    removeDups n = n

    resolveGbl (Gbl  l) | l `M.member` gblMap = Abs $ outputAddress $ snd $ gblMap M.! l
    resolveGbl (BGbl l) | l `M.member` gblMap = Abs $ outputBank $ snd $ gblMap M.! l
    resolveGbl o = o
