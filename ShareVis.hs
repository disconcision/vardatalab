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

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G





type ID = Int
type Node = ID 

type V = Node
type E = (Node,Node)
type Graph = ([V], [E])

--toGraph :: [String] -> Graph
--toGraph [] = ([("null", 0)],[])

type MemoryTable = [(ID, (String, ID))]

--initialMemoryTable :: MemoryTable
--initialMemoryTable = [(0, ("/", 0))]


lx = []
l0 = "0" : lx
l1 = "1" : l0
l2 = "2" : l1


l0name :: String
l0name = "l0"
l1name = "l1"
l2name = "l2"

testLists :: [(String, [String])]
testLists = [(l0name, l0), (l1name, l1), (l2name, l2)]


variableTable :: MemoryTable
variableTable = concatMap
  (\ (variableName, list) ->
      [(getName variableName, (variableName, getName list))]
    ) testLists  

memoryTable :: MemoryTable
memoryTable = mapMem l2 []

mapMem :: [String] -> MemoryTable -> MemoryTable
mapMem [] _ = [(getName [], ("null", getName []))]
mapMem c@(x: xs) table =
    concat [[(getName c, (x, getName xs))], (mapMem xs table)]

graph :: Graph
graph = (map (\ (loc, (label, next)) -> loc) memoryTable,
         map (\ (loc, (label, next)) -> (loc, next)) memoryTable)

main = do
    print $ concat [memoryTable, variableTable]
    print graph
    --print $ getName l0
    --print $ getName l1
    --print $ getName l2








getName :: a -> Int
-- TODO: understand why we need bangpatter and unsafePerformIO
-- without bang pattern, this always just gives 1
getName !d = unsafePerformIO $ do
    n <- makeStableName $! d
    return $ hashStableName n
{-
main = do
    print $ getName l0
    print $ getName l1
    print $ getName l2
    --print $ getName c
-}