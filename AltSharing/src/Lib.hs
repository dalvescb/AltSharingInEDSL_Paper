module Lib where

import Data.Word
import Control.Monad.State.Strict

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)

-- * Core DSL

data GPR = GPR { unGPR :: Word64 }
data VR = VR { unVR :: (Word64,Word64) }

class CoreISA repr where
  toGPR :: Word64 -> repr Word64
  add :: repr Word64 -> repr Word64 -> repr Word64

-- * Interpretation

newtype Interp a = Interp { runInterp :: a }
  deriving (Show,Eq)

instance CoreISA Interp where
  toGPR w = Interp w
  add g0 g1 = Interp $ runInterp g0 + runInterp g1

-- * Graph Generation

data OpLabel = GPROp Word64
             | ADDOp

type NodeID = Int

type Graph = Gr OpLabel ()
