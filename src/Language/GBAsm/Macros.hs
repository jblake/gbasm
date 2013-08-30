-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.Macros
where

import Data.Generics.Uniplate.Operations
import qualified Data.Map as M

import Language.GBAsm.Types

-- |First we collect all the macro definitions, then we replace duplicate
-- definitions with errors, then we replace references.
macrosPass :: SourceAST -> SourceAST
macrosPass ast = transformBi resolveMac $ transform removeDups ast

  where

    -- The reverse here is so that we catch the first definition of a macro,
    -- and identify all further definitions as duplicates.
    macMap = M.fromList $ reverse [ (l, (d, a)) | Macro d l a <- universe ast ]

    removeDups (Macro d l a) | macMap M.! l /= (d, a) = Err d $ "Duplicate definition of macro " ++ show l ++ " (previous definition on line " ++ show (sourceLine $ fst $ macMap M.! l) ++ ")"
    removeDups n = n

    resolveMac (Mac l) | l `M.member` macMap = snd $ macMap M.! l
    resolveMac o = o