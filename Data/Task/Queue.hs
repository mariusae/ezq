module Data.Task.Queue (
    Queue, Ident
  -- The API on the queues themselves:
  , empty, add, get, take, done, size
  ) where

import           Prelude hiding (take)
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import           System.Time

-- | Task identifier.
type Ident = Int

-- | The queue itself.
data Queue a = Q (Map Ident (a, ClockTime)) Ident
               deriving (Show, Eq)
               -- TODO: a better show instance

-- | An empty task queue.
empty :: Queue a
empty = Q Map.empty 0

-- | Add a task to the queue.
add :: a -> Queue a -> Queue a
add t (Q m next) = Q (Map.insert next (t, zero) m) next'
  where 
    next' = next + 1
    zero  = TOD 0 0

-- | Take a task from the queue, returning a new identifier for it.
take :: Ident                   -- ^ Task identifier.
     -> ClockTime               -- ^ Current time
     -> Queue a                 -- ^ The queue
     -> Maybe (Ident, (Queue a))
take ident until (Q m next) =
  Map.lookup ident m >>= \(task, _) ->
    let m' = Map.insert next (task, until) (Map.delete ident m) in
    return $ (next, (Q m' next'))
  where
    next' = next + 1

-- | Get a task from the queue.
get :: ClockTime                -- ^ Current time
    -> Int                      -- ^ Number of seconds to reserve task
    -> Queue a                  -- ^ The queue
    -> Maybe ((Ident, a), Queue a)
get now howlong q@(Q m next) =
  takeFirst eligible
  where
    until = addToClockTime (noTimeDiff { tdSec = howlong }) now
    -- All tasks with expiration times ``< now'' are eligible:
    eligible = filter ((<now) . snd . snd) (Map.assocs m)
    -- Takes the first task, and returns the task together with the
    -- modified queue. Note that lazyness helps us out here as we only
    -- traverse tasks until the first eligible one is found.
    takeFirst []                  = Nothing
    takeFirst ((ident, (t, _)):_) = 
      let Just (ident', q') = take ident until q 
      in
        Just $ ((ident', t), q')

-- | Declare a task complete.
done :: Ident                   -- ^ Task ident
     -> Queue a                 -- ^ The queue
     -> Maybe (Queue a)
done ident (Q m next) =
  Map.lookup ident m >>= \(task, _) ->
    return $ Q (Map.delete ident m) next

-- | Number of tasks in the queue.
size :: Queue t -> Int
size (Q q _) = Map.size q
