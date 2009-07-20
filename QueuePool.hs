{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error

import Queue

data QueuePool = QP (Map String Queue)

type QueuePoolM = ErrorT String (StateT QueuePool IO)

emptyQueuePool :: QueuePool
emptyQueuePool = QP Map.empty

addQueue :: String -> Queue -> QueuePool -> QueuePool
addQueue name q (QP qp) = QP $ Map.insert name q qp

getQueue :: String -> QueuePool -> Maybe Queue
getQueue name (QP qp) = Map.lookup name qp




-- withQueues qp f =
--   getQueue >>= 

-- -- withQueues just runs in the maybe monad.

-- withQueues qp $ do
--   editQueue "foo" ops
--   editQueue "bar" ops
--   -- etc...
