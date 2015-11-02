{- File: Util.hs
   Author: Janos Tapolczai, 0825077
   Purpose: Implementations for stacks and queues.
-}

module Stack (Stack, Queue, DataStructure (..)) where

import Prelude hiding (elem)

newtype Stack a = Stk [a]
--TODO: more efficient implementation
newtype Queue a = Queue [a]

class DataStructure a where
   push :: elem -> a elem -> a elem
   pop :: a elem -> a elem
   peek :: a elem -> elem
   emptyDS :: a elem
   isEmpty :: a elem -> Bool

instance DataStructure Stack where
   push elem (Stk stk) = Stk $ elem:stk

   pop (Stk (_:xs)) = Stk xs
   pop (Stk []) = undefined

   peek (Stk (x:_)) = x
   peek (Stk []) = undefined

   emptyDS = Stk []

   isEmpty (Stk []) = True
   isEmpty _ = False

instance DataStructure Queue where
   push elem (Queue stk) = Queue $ stk ++ [elem]

   pop (Queue (_:xs)) = Queue xs
   pop (Queue []) = undefined

   peek (Queue (x:_)) = x
   peek (Queue []) = undefined

   emptyDS = Queue []

   isEmpty (Queue []) = True
   isEmpty _ = False
