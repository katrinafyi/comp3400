module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import           Control.Concurrent.STM (STM, TVar, atomically, newTVar
                                       , readTVar, writeTVar)


-- | Doubly linked list node with head/tail sentinels.
-- Be very careful instantiating and using HeadNode/TailNode because of the
-- partial record selectors.
data Node a = HeadNode { nodeRight :: TVar (Node a) }
            | Node { nodeValue :: a
                   , nodeLeft :: TVar (Node a)
                   , nodeRight :: TVar (Node a)
                   }
            | TailNode { nodeLeft :: TVar (Node a) }

-- | A double-ended queue, with a head node and tail node.
data Deque a = Deque (Node a) (Node a)

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

-- | Deletes the given non head/tail node, returning its old value.
-- Returns Nothing and makes no changes if the given node was a head/tail node.
-- Links the deleted node's left to the deleted node's right.
deleteNode :: Node a -> STM (Maybe a)
deleteNode (Node x l r) = do
  left <- readTVar l
  right <- readTVar r
  linkNodes left right
  pure $ Just x
deleteNode _ = pure Nothing

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
  linkNodes h t
  pure $ Deque h t

-- | Constructs a new empty deque.
mkDeque :: IO (Deque a)
mkDeque = atomically emptyDeque

-- | Pops from the right of the deque, if possible.
pop :: Deque a -> IO (Maybe a)
pop (Deque _ t) = atomically $ readTVar (nodeLeft t) >>= deleteNode

-- | Pushes to the right of the deque.
push :: Deque a -> a -> IO ()
push (Deque _ t) x = atomically $ newNode x >>= insertLeft t

-- | Pushes from the left of the deque.
unshift :: Deque a -> a -> IO ()
unshift (Deque h _) x = atomically $ newNode x >>= insertRight h

-- | Pops from the left of the deque, if possible.
shift :: Deque a -> IO (Maybe a)
shift (Deque h _) = atomically $ readTVar (nodeRight h) >>= deleteNode
