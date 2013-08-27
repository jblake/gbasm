-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror #-}

module Language.GBAsm.ByteGen
where

import Control.Monad
import Control.Monad.Trans.State
import Data.ByteString.Lazy as BS

import Language.GBAsm.Types

-- |Given an AST which only has empty nodes and Bin nodes, flatten it into a final ByteString.
-- The passed AST should exclusively have Scope, Global, Local, OutPos, and Bin nodes.
byteGenPass :: FullAST -> ByteString
byteGenPass theAST = execState (pass theAST) empty

  where

    pass :: FullAST -> State ByteString ()

    pass (Scope _ asts) = mapM_ pass asts

    pass (Global _ _) = return ()

    pass (Local _ _) = return ()

    pass (OutPos _ _ _) = return ()

    pass (Bin (_,pos) bs) = when (not $ BS.null bs) $ do
      let
        fileOffset = fromIntegral $
          if outputBank pos == 0
            then outputAddress pos
            else outputAddress pos + 0x4000 * (outputBank pos - 1)
      padding <- gets $ \s -> fileOffset - BS.length s
      when (padding > 0) $ modify $ \s -> append s $ BS.replicate padding 0
      modify $ \s -> BS.take fileOffset s `append` bs `append` BS.drop (fileOffset + BS.length bs) s

    pass e@(Err _ _) = error $ "Unhandled error (" ++ show e ++ ") somehow made it to byteGenPass!"

    pass n = error $ "Uncompiled AST node (" ++ show n ++ ") somehow made it to byteGenPass!"
