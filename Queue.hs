-- | A single queue.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Queue(
    Op(..)
  , Queue
  , QueueState
  , runQueue, addQueue, getQueue, editQueues, getQueues
  , emptyQueueState
  , emptyQueue, editQueue
  ) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

data Op    = Add String | Remove Integer
             deriving (Show)
data Queue = Q (Map Integer String) Integer
             deriving (Show)

data QueueState = ST (Map String Queue)
                  deriving (Show)

newtype QueueM a = QM (StateT QueueState IO a)
  deriving (Monad, MonadIO)

-- -- runs inside of the queue monad, to edit state.
-- newtype QueueEditM a = QEM (StateT [Queue] QueueM a)
-- editQueues qs (QEM m) = runStateT m qs >>= \(a, q') -> return (q', a)

runQueue :: QueueState -> QueueM a -> IO (QueueState, a)
runQueue st (QM m) = runStateT m st >>= \(a, q') -> return (q', a)

-- handy shortcut.
getST = get >>= \(ST st) -> return st
putST = put . ST

addQueue :: String -> QueueM ()
addQueue name = QM $ getST >>= putST . Map.insert name emptyQueue

getQueue name = QM $ getST >>= return . Map.lookup name

getQueues :: QueueM [String]
getQueues = QM $ getST >>= return . Map.keys

emptyQueueState :: QueueState
emptyQueueState = ST $ Map.empty

-- | Create a new, empty queue.
emptyQueue :: Queue
emptyQueue = Q Map.empty 0

-- all the ops are in monads .. 
editQueues :: [(String, Op)] -> QueueM Bool
editQueues ops = QM $ do
  st <- get
  case editQueues' ops st of 
    (Just st') -> put st' >> return True
    Nothing    -> return False

  where editQueues' :: [(String, Op)] -> QueueState -> Maybe QueueState
        editQueues' [] st                  = Just st
        editQueues' ((qn, op):ops) (ST st) = do
          newStateFrom q'
            where q' = Map.lookup qn st >>= editQueue [op]
                  newStateFrom (Just q) = Just $ ST $ Map.insert qn q st
                  newStateFrom Nothing  = Nothing


-- | Edit a set of queues simultaneously.

-- | Edit queue with the given operations, yielding a modified queue
-- | on a sucessfull edit, or *Nothing* on to perform all edits.
editQueue :: [Op] -> Queue -> Maybe Queue
editQueue []       q = Just q
editQueue (op:ops) q = edit op q >>= editQueue ops
  where
    edit (Add task)     = insert task
    edit (Remove ident) = delete ident

    insert item (Q m i) =
      Just $ Q m' i'
        where i' = i + 1
              m' = Map.insert i' item m
    delete key (Q m i) =
      Map.lookup key m >> (return $ Q m' i)
        where m' = Map.delete key m
