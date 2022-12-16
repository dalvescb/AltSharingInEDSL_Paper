{-# LANGUAGE OverloadedStrings #-}

module ByteStringExplicit where

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

-- | Finally Tagless Style Expression DSL
class Exp repr where
  add :: repr Int -> repr Int -> repr Int
  variable :: String -> repr Int
  constant :: Int -> repr Int

-- | Explicit caching of ByteStrings
class Substitute repr where
  subT :: ByteString -> repr a -> repr a

instance Substitute Graph where
  subT s' (Graph g s _) = Graph g s' (Just s)

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
               , dagSubMap :: Map ByteString ByteString -- | a map of labels to their subst.
               , dagMaxID :: Int -- | used to track # of triecons performed
               } deriving Show

-- | DAG construction representation via the State monad
data Graph a = Graph { unGraph :: State DAG NodeID
                     , unStringAST :: ByteString
                     , unSubT :: Maybe ByteString }

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


triecons :: ByteString -> Node -> State DAG NodeID
triecons sAST node = do
 DAG trie dCache maxID <- get
 case Trie.lookup sAST trie of
   Nothing -> let maxID' = maxID+1
                  trie' = Trie.insert sAST (node,maxID+1) trie
               in do put $ DAG trie' dCache maxID'
                     return maxID'
   Just (_,nodeID) -> return nodeID

seqArgs :: [Graph a] -> State DAG [NodeID]
seqArgs inps =
  let
    seqArg (Graph sT sAST mSubt) =
      do DAG trie _ _ <- get
         let sAST' = case mSubt of
                       Just s -> s
                       Nothing -> sAST
         case Trie.lookup sAST' trie of
           Nothing -> sT -- error "missing ast"
           Just (node,nodeID) -> do subTInsert mSubt sAST (node,nodeID)
                                    return nodeID
  in sequence $ map seqArg inps

subTInsert :: Maybe ByteString -> ByteString -> (Node, NodeID) -> State DAG ()
subTInsert Nothing  _ _  = return ()
subTInsert (Just s) sAST nodeID =
  do DAG trie subtMap _ <- get
     case Map.lookup sAST subtMap of
        Just sAST' -> if sAST == sAST'
                      then return ()
                      else error "tried to resubT"
        Nothing -> let cMap' = Map.insert sAST s subtMap
                       trie' = Trie.insert sAST nodeID trie
                  in modify (\dag -> dag { dagTrie = trie', dagSubMap = cMap' })

instance Exp Graph where
  constant x = let
    node = NConstant x
    sAST = buildStringAST node []
    in Graph (triecons sAST $ NConstant x) sAST Nothing
  variable x = let
    node = NVariable x
    sAST = buildStringAST node []
    in Graph (triecons sAST $ NVariable x) sAST Nothing
  add e1 e2 = let
      sAST = buildStringAST (NAdd undefined undefined) [unStringAST e1,unStringAST e2]
      sT = do ns <- seqArgs [e1,e2]
              case ns of
                [n1,n2] -> triecons sAST $ NAdd n1 n2
                _ -> error "black magic"
    in Graph sT sAST Nothing

buildDAG g = runState (unGraph g) (DAG Trie.empty Map.empty 0)


-- | Example of exponential scale of hash-consing
addChains :: (Exp repr,Substitute repr) => Int -> repr Int -> repr Int
addChains n x0 = fst
                 $ head
                 $ drop n
                 $ iterate (\(x,i) -> (subT ("add"<>(ByteString.pack $ show i)) (add x x),i+1)) (x0,0)
