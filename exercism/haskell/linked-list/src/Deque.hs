module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import           Control.Concurrent.STM (STM, atomically, newTVar, readTVar
                                       , writeTVar, TVar, modifyTVar')

-- | Doubly linked list node with head/tail sentinels.
-- Be very careful instantiating and using HeadNode/TailNode because of the
-- partial record selectors.
data Node a = HeadNode { nodeRight :: TVar (Node a) }
            | Node { nodeValue :: a
                   , nodeLeft :: TVar (Node a)
                   , nodeRight :: TVar (Node a)
                   }
            | TailNode { nodeLeft :: TVar (Node a) }

-- | A double-ended queue, with a length, head node, and tail node.
data Deque a = Deque (TVar Int) (Node a) (Node a)

-- | Links the two nodes with the first argument on the left and second on the right.
-- Returns the right node.
linkNodes :: Node a -> Node a -> STM ()
linkNodes l r = do
  writeTVar (nodeRight l) r
  writeTVar (nodeLeft r) l

linkThree :: Node a -> Node a -> Node a -> STM ()
linkThree l x r = do
  linkNodes l x
  linkNodes x r
  pure ()

-- | Inserts the second argument to the right of the first argument.
insertRight :: Node a -> Node a -> STM ()
insertRight node new = do
  right <- readTVar (nodeRight node)
  linkThree node new right

-- | Inserts the second argument to the left of the first argument.
insertLeft :: Node a -> Node a -> STM ()
insertLeft node new = do
  left <- readTVar (nodeLeft node)
  linkThree left new node

-- | Deletes the given node, returning its value.
-- Links the deleted node's left to the deleted node's right.
deleteNode :: Node a -> STM a
deleteNode node = do
  left <- readTVar (nodeLeft node)
  right <- readTVar (nodeRight node)
  linkNodes left right
  pure $ nodeValue node

-- | Constructs a new Node with the given value and undefined left/right.
newNode :: a -> STM (Node a)
newNode x = Node x
  <$> newTVar (error "node left undefined")
  <*> newTVar (error "node right undefined")

-- | Constructs a new empty deque of length zero.
emptyDeque :: STM (Deque a)
emptyDeque = do
  h <- HeadNode <$> newTVar (error "head right undefined")
  t <- TailNode <$> newTVar (error "tail left undefined")
  l <- newTVar 0
  linkNodes h t
  pure $ Deque l h t

-- | Pops from a deque, given a length var and node var.
-- Returns the node's value if length is non-zero and decrements length.
popFromDeque :: TVar Int -> TVar (Node a) -> STM (Maybe a)
popFromDeque len selector = do
  n <- readTVar len
  if n <= 0
    then pure Nothing
    else do
      old <- readTVar selector
      val <- deleteNode old
      modifyTVar' len (subtract 1)
      pure $ Just val

-- | Pushes to a deque, given a length var and function to link nodes.
-- Increments length.
pushToDeque :: TVar Int -> (Node a -> STM b) -> a -> STM b
pushToDeque len inserter x = do
  modifyTVar' len (+1)
  new <- newNode x
  inserter new

-- | Constructs a new empty deque.
mkDeque :: IO (Deque a)
mkDeque = atomically emptyDeque

-- | Pops from the right of the deque, if possible.
pop :: Deque a -> IO (Maybe a)
pop (Deque len _ t) = atomically $ popFromDeque len (nodeLeft t)

-- | Pushes to the right of the deque.
push :: Deque a -> a -> IO ()
push (Deque len _ t) = atomically . pushToDeque len (insertLeft t)

-- | Pushes from the left of the deque.
unshift :: Deque a -> a -> IO ()
unshift (Deque len h _) = atomically . pushToDeque len (insertRight h)

-- | Pops from the left of the deque, if possible.
shift :: Deque a -> IO (Maybe a)
shift (Deque len h _) = atomically $ popFromDeque len (nodeRight h)