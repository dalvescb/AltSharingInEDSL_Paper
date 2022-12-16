module Main where

import qualified HashConsSharing as HCS
import qualified ByteStringSharing as BSS
import qualified ByteStringExplicit as BSE

import qualified Data.Trie as Trie
import qualified Data.Map as Map

n = 2000
main :: IO ()
main = let
   -- dag = Map.toList $ HCS.biMap $ HCS.dagMAP $ snd $ HCS.buildDAG $ HCS.addChainsE n $ HCS.variable "x"
   dag = Trie.elems $  BSE.dagTrie $ snd $ BSE.buildDAG $ BSE.addChains n $ BSE.variable "x"
  in putStrLn $ show dag
