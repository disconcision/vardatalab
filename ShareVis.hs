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
import Data.Word8

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL



type ID = Int
type V = (ID, String)
type E = (ID, ID, String)
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
memoryTable = concat [mapMem l2 [], variableTable]

mapMem :: [String] -> MemoryTable -> MemoryTable
mapMem [] _ = [(getName [], ("null", getName []))]
mapMem c@(x: xs) table =
    concat [[(getName c, (x, getName xs))], (mapMem xs table)]

graph :: Graph
graph = (map (\ (loc, (label, next)) -> (loc, label)) memoryTable,
         map (\ (loc, (label, next)) -> (loc, next, "next")) memoryTable)

type VLabel = TL.Text
type ELabel = TL.Text

newMT = map (\ (loc, (label, next)) -> (loc, (TL.pack label, next))) memoryTable

fileGraphParams :: G.GraphvizParams ID String String () String
fileGraphParams = G.defaultParams {
  G.fmtNode = \(v, vl) -> case vl of
      _ -> colorAttribute $ G.RGB 0 0 0,
  G.fmtEdge = \(from, to, el) -> case el of
      "next" -> colorAttribute $ G.RGB 200 0 0
      }  
  where
    colorAttribute color = [ G.Color $ G.toColorList [ color ] ]
    
{-
ex1Params :: G.GraphvizParams ID VLabel ELabel () TL.Text
ex1Params = G.nonClusteredParams { G.globalAttributes = ga
                               , G.fmtNode          =  fn
                               , G.fmtEdge          =  fe
                               }
  where fn (_,l)   = [G.textLabel l]
        fe (_,_,l) = [G.textLabel l]

        ga = [ G.GraphAttrs [ G.RankDir   G.FromLeft
                          , G.BgColor   [G.toWColor G.White]
                          ]
             , G.NodeAttrs  [ G.shape     G.BoxShape
                          , G.FillColor (myColorCL 2)
                          , G.style     G.filled
                          ]
             ]

myColorCL :: Word8 -> G.ColorList
myColorCL n | n == 1 = c $ (G.RGB 127 108 138)
            | n == 2 = c $ (G.RGB 175 177 112)
            | n == 3 = c $ (G.RGB 226 206 179)
            | n == 4 = c $ (G.RGB 172 126 100)
 where c rgb = G.toColorList [rgb]

myColor :: Word8 -> G.Attribute
myColor n = G.Color $ myColorCL n             
-}

main :: IO ()  
main = do
    --(vs, es) <- graph
    print $ concat [memoryTable, variableTable]
    print graph
    
    let vs = fst graph
        es = snd graph
        dotGraph = G.graphElemsToDot fileGraphParams vs es :: G.DotGraph ID
        dotText = G.printDotGraph dotGraph  :: TL.Text
    TL.writeFile "vargraph-files.dot" $ dotText








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