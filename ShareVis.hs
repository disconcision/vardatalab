{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}

module ShareVis where

import Control.Applicative
import Control.Monad
import Data.List 
import Data.Maybe
import Control.Exception
import Control.Parallel.Strategies
import System.Mem.StableName
import System.IO.Unsafe


a = 5
b = 6
c = b

l0 = 0 : []
l1 = 1 : l0
l2 = tail l1

getName :: a -> Int
-- TODO: understand why we need bangpatter and unsafePerformIO
-- without bang pattern, this always just gives 1
getName !d = unsafePerformIO $ do
    n <- makeStableName $! d
    return $ hashStableName n

main = do
    print $ getName l0
    print $ getName l1
    print $ getName l2
    --print $ getName c
        