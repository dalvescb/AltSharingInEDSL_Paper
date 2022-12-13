{- This code is based of the work of Oleg, Kiselyov's
 - "Implementing explicit and finding implicit sharing in embedded DSLs"
 -}
module HashConsSharing where

import Prelude hiding (exp)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Debug.Trace
import Data.Word
import Control.Monad.State.Strict

-- * Data Type based DSL

-- | Data Type based Expression DSL
data Exp0
  = Add Exp0 Exp0
  | Variable String
  | Constant Int

-- Example
v0 = Variable "v0"
exp0 = Add v0 (Constant 0)
exp1 = Add exp0 exp0

-- * Finally Tagless Style DSL

-- | Finally Tagless Style Expression DSL
class Exp repr where
  add :: repr Int -> repr Int -> repr Int
  variable :: String -> repr Int
  constant :: Int -> repr Int

-- | AST Representation (no-sharing)
newtype ExpR a = ExpR { unExpR :: Exp0 }

instance Exp ExpR where
  constant = ExpR . Constant
  variable = ExpR . Variable
  add (ExpR x) (ExpR y) = ExpR (Add x y)


-- | Pretty Printing representation
newtype Pretty a = Pretty { unPretty :: String }

instance Exp Pretty where
  add x y = Pretty $ "("++unPretty x++") + ("++unPretty y++")"
  variable x = Pretty x
  constant x = Pretty $ show x

-- | Example usage of finally tagless
exp :: Exp repr => repr Int -> repr Int
exp v0 =
  let
    exp0 = add v0 (constant 0)
  in add exp0 exp0

expR = unExpR $ exp $ variable "v0"
expP = unPretty $ exp $ variable "v0"

-- * Hash Consing

-- | A node is identified by a unique Int
type NodeID = Int

-- | Each Node has a constructor that holds either literals or the NodeID of its
-- inputs, making it unique so long as NodeID's are unique
data Node = NAdd NodeID NodeID
          | NVariable String
          | NConstant Int
 deriving (Show,Eq,Ord)


-- | Implementation of BiMap as a strict Map that also keeps track of the max nodeID
data BiMap a = BiMap { biMap :: Map a NodeID
                     , maxNodeID :: NodeID
                     }
  deriving Show

-- | Look up a NodeID in the BiMap, will return Nothing if the key is not in the Map
lookup_key :: Ord a => a -> BiMap a -> Maybe NodeID
lookup_key node (BiMap biMap _) = Map.lookup node biMap

-- | Insert a new key into the BiMap with the new maximum NodeID (also
-- increments the maximum NodeID)
insert :: Ord a => a -> BiMap a -> (Int,BiMap a)
insert node (BiMap bMap mID) =
  let
    bMap' = Map.insert node mID bMap
  in (mID,BiMap bMap' (mID+1))

-- | A BiMap is an empty Map with maximum NodeID 0
empty :: BiMap a
empty = BiMap Map.empty 0

-- | A Directed Acyclic Graph is a BiMap between Node and NodeID
newtype DAG = DAG (BiMap Node) deriving Show

-- | DAG construction representation via the State monad
newtype Graph a = Graph { unGraph :: State DAG NodeID }

instance Exp Graph where
  constant x = Graph (hashcons $ NConstant x)
  variable x = Graph (hashcons $ NVariable x)
  add e1 e2 = Graph (do
                     h1 <- unGraph e1
                     h2 <- unGraph e2
                     hashcons $ NAdd h1 h2)

-- | naive implementation of hash consing, insert a node into the DAG only if it isn't
-- already there
hashcons :: Node -> State DAG NodeID
hashcons e = do
  DAG m <- get
  traceM $ "hashcons performed"
  case lookup_key e m of
    Nothing -> let (k,m') = insert e m
               in put (DAG m') >> return k
    Just k -> return k
