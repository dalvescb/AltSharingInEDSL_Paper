{-# LANGUAGE OverloadedStrings #-}

module ByteStringSharing where

import Prelude hiding (exp)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.State.Strict
import Graphics.EasyPlot
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Trie (Trie)
import qualified Data.Trie as Trie
import qualified Data.List as List

import qualified HashConsSharing as HCS

-- | Finally Tagless Style Expression DSL
class Exp repr where
  add :: repr Int -> repr Int -> repr Int
  variable :: String -> repr Int
  constant :: Int -> repr Int

-- | A node is identified by a unique Int
type NodeID = Int

-- | Each Node has a constructor that holds either literals or the NodeID of its
-- inputs, making it unique so long as NodeID's are unique
data Node = NAdd NodeID NodeID
          | NVariable String
          | NConstant Int
 deriving (Show,Eq,Ord)


-- | A Directed Acyclic Graph is inside the values of the Trie
data DAG = DAG { dagTrie :: Trie (Node,NodeID) -- | a trie from bytestrings to Node/NodeID
               , dagNumCons :: Int -- | used to track number of triecons performed
               , dagMaxID :: Int -- | used to track max node id
               } deriving Show

-- | DAG construction representation via the State monad
data Graph a = Graph { unGraph :: State DAG NodeID
                     , unStringAST :: ByteString }

-- | Build a AST encoded as a ByteString
buildStringAST :: Node -> [ByteString] -> ByteString
buildStringAST node args =
  let
    opString = case node of
                 NAdd _ _ -> "nadd"
                 NVariable s -> "$" <> ByteString.pack s
                 NConstant c -> "#" <> (ByteString.pack $ show c)
    argsString = "(" <> ByteString.concat (List.intersperse "," args) <> ")"
  in opString <> argsString


-- TODO call me something different then triecons?
triecons :: ByteString -> Node -> State DAG NodeID
triecons sAST node = do
 DAG trie tCnt maxID <- get
 modify (\dag -> dag { dagNumCons = tCnt + 1})
 case Trie.lookup sAST trie of
   Nothing -> let maxID' = maxID+1
                  trie' = Trie.insert sAST (node,maxID+1) trie
               in do modify (\dag -> dag { dagTrie = trie', dagMaxID = maxID' } )
                     return maxID'
   Just (_,nodeID) -> return nodeID


seqArgs :: [Graph a] -> State DAG [NodeID]
seqArgs inps =
  let
    seqArg (Graph sT sAST) =
      do DAG trie _ _ <- get
         case Trie.lookup sAST trie of
           Nothing -> sT
           Just (_,nodeID) -> return nodeID
  in sequence $ map seqArg inps

instance Exp Graph where
  constant x = let
    node = NConstant x
    sAST = buildStringAST node []
    in Graph (triecons sAST $ NConstant x) sAST
  variable x = let
    node = NVariable x
    sAST = buildStringAST node []
    in Graph (triecons sAST $ NVariable x) sAST
  add e1 e2 = let
      sAST = buildStringAST (NAdd undefined undefined) [unStringAST e1,unStringAST e2]
      sT = do ns <- seqArgs [e1,e2]
              case ns of
                [n1,n2] -> triecons sAST $ NAdd n1 n2
                _ -> error "black magic"
    in Graph sT sAST

buildDAG g = runState (unGraph g) (DAG Trie.empty 0 0)

-- | Example of exponential scale of hash-consing
addChains :: Exp repr => Int -> repr Int -> repr Int
addChains n x0 = head $ drop n $ iterate (\x -> add x x) x0

plotAddChains size =
  let
    chainsDataH = map (\(x,y) -> (fromIntegral x,fromIntegral y))
      [ (n,HCS.dagNumCons $ snd $ HCS.buildDAG $ HCS.addChains n $ HCS.variable "x") | n <- [0..size] ]
    chainsDataHS = map (\(x,y) -> (fromIntegral x,fromIntegral y))
      [ (n,HCS.dagNumCons $ snd $ HCS.buildDAG $ HCS.addChainsE n $ HCS.variable "x") | n <- [0..size] ]
    chainsDataT = map (\(x,y) -> (fromIntegral x,fromIntegral y))
      [ (n,dagNumCons $ snd $ buildDAG $ addChains n $ variable "x") | n <- [0..size] ]
  in plot (PNG "plot.png") [Data2D [Title "Triecons",Style Linespoints,Color Blue] [] chainsDataT
                           ,Data2D [Title "Hashcons Explicit",Style Linespoints,Color Orange] [] chainsDataHS
                           ,Data2D [Title "Hashcons",Style Linespoints,Color Red] [] chainsDataH
                           ]
