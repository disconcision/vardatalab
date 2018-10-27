{-# LANGUAGE BangPatterns #-}

module ShareVis where

import System.Mem.StableName
import System.IO.Unsafe
import System.Process 

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL



type ID = Int
type TestData a = [(String, [a])]
type MemoryTable = [(ID, (String, ID))]

type VLabel = String
type ELabel = String
type V = (ID, String)
type E = (ID, ID, String)
type Graph = ([V], [E])



-- TODO: make sure I understand why we need bangpattern
-- and unsafePerformIO. Without bang pattern => const 1
getName :: a -> Int
getName !d = unsafePerformIO $ do
    n <- makeStableName $! d
    return $ hashStableName n


-- makes a table of memory locations of the provided test data
makeMemoryTable :: (Show a) =>  TestData a -> MemoryTable
makeMemoryTable =
    concatMap (\ (variableName, ls) ->
        concat [[(getName variableName, (variableName, getName ls))],
                mapMem ls []]) 


-- takes a list of a's and makes a map of list element memory locations                
mapMem :: (Show a) => [a] -> MemoryTable -> MemoryTable
mapMem [] _ = [(getName [], ("null", getName []))]
mapMem c@(x: xs) table =
    concat [[(getName c, ((show x), getName xs))], (mapMem xs table)]


-- generates a set of vertices and edges from test data
graph :: (Show a) => TestData a -> Graph
graph d = (map (\ (loc, (label, next)) -> (loc, label)) memoryTable,
           map (\ (loc, (label, next)) -> (loc, next, "next")) memoryTable)
         where memoryTable = makeMemoryTable d


-- Graph style settings         
graphStyleParams :: G.GraphvizParams ID VLabel ELabel () VLabel
graphStyleParams = G.defaultParams {
  G.globalAttributes =
    [ G.GraphAttrs [ G.RankDir   G.FromLeft
                   , G.BgColor   [G.toWColor G.White]]
    , G.NodeAttrs  [ G.shape     G.BoxShape
                   , G.FontColor fontColor
                   , G.FillColor (G.toColorList $ [fillColor])
                   , G.style     G.filled
                   ]],
  G.fmtNode = \(v, vl) -> case vl of
      _ -> [G.textLabel (TL.pack (vl ++ " : " ++ (show v))),
            G.Color $ G.toColorList [ G.RGB 0 0 0 ]],
  G.fmtEdge = \(from, to, el) -> case el of
      "next" -> [{-G.textLabel (TL.pack el),-}
                 G.Color $ G.toColorList [ G.RGB 255 0 0 ]]
      }  
  where
    fillColor = G.RGB 200 200 200
    fontColor = G.RGB 255 0 0
    

-- Writes a graph based on test data to a file and displays it
-- (requires imagemagick to display)
showGraph :: (Show a) =>  TestData a -> IO ()  
showGraph td = do
    let (vs, es) = graph td
        dotGraph = G.graphElemsToDot graphStyleParams vs es :: G.DotGraph ID
        dotText = G.printDotGraph dotGraph  :: TL.Text
    TL.writeFile "temp.dot" $ dotText
    system "dot temp.dot -Tpng > temp.png" >>= \exitCode -> print exitCode
    system "display temp.png" >>= \exitCode -> print exitCode
    system "rm temp.png" >>= \exitCode -> print exitCode
    system "rm temp.dot" >>= \exitCode -> print exitCode


