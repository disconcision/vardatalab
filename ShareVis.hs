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

type VLabel = String
type ELabel = String

type MemoryTable = [(ID, (String, ID))]


-- ACTUAL TEST DATA
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
-- END TEST DATA


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


graphStyleParams :: G.GraphvizParams ID String String () String
graphStyleParams = G.defaultParams {
  G.globalAttributes =
    [ G.GraphAttrs [ G.RankDir   G.FromLeft
                   , G.BgColor   [G.toWColor G.White]]
    , G.NodeAttrs  [ G.shape     G.BoxShape
                   , G.FontColor fontColor
                   , G.FillColor (G.toColorList $ [fillColor])
                   , G.style     G.filled
                   ]
    ],
  G.fmtNode = \(v, vl) -> case vl of
      _ -> [G.textLabel (TL.pack vl),
            G.Color $ G.toColorList [ G.RGB 0 0 0 ]],
  G.fmtEdge = \(from, to, el) -> case el of
      "next" -> [{-G.textLabel (TL.pack el),-}
                 G.Color $ G.toColorList [ G.RGB 255 0 0 ]]
      }  
  where
    fillColor = G.RGB 200 200 200
    fontColor = G.RGB 255 0 0
    


main :: IO ()  
main = do
    --print $ concat [memoryTable, variableTable]
    --print graph
    let (vs, es) =  graph
        dotGraph = G.graphElemsToDot graphStyleParams vs es :: G.DotGraph ID
        dotText = G.printDotGraph dotGraph  :: TL.Text
    TL.writeFile "vargraphfile.dot" $ dotText








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