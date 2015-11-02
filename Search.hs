{- File: Util.hs
   Author: Janos Tapolczai, 0825077
   Purpose: Depth- and breath-first search functions.
-}

module Search where

import Prelude hiding (succ)

import Stack

-- |Goal-function which determines whether a node is a solution.
type GoalF node = node -> Bool
-- |Successort function which generates a node's successors.
type SuccF node = node -> [node]

-- |Generic search.
search :: (Eq node, DataStructure st) =>
          (st node)
       -> SuccF node
       -> GoalF node
       -> node
       -> [node]
search stk succ goal root = search' (push root stk)
   where
      --no more successors: finish my branch
      search' cur | isEmpty cur     = []
      --current node is a goal -> add solution and continue searching the rest
                  | goal (peek cur) = (peek cur) : (search' (pop cur))
      --otherwise (regular case; no goals found): replace the next node
      --with its successors, add them to the list of nodes to be searched and
      --continue searching
                  | otherwise       = let next = peek cur in
                                      search' (foldr push (pop cur) (succ next))

-- |Depth-first search.
dfs :: (Eq node) =>
       SuccF node
    -> GoalF node
    -> node
    -> [node]
dfs = search (emptyDS::Stack node)

-- |Breath-first search.
bfs :: (Eq node) =>
       SuccF node
    -> GoalF node
    -> node
    -> [node]
bfs = search (emptyDS::Queue node)

