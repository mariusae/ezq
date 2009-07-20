-- | Parse requests from clients.
module Request(
    GetRequest(..)
  , EditRequest(..)
  ) where

import Text.Printf
import Text.JSON
import Data.Map (Map)
import qualified Data.Map as Map

import Queue(Op(..))

data GetRequest  = GetRequest [String]
                   deriving (Show)
data EditRequest = EditRequest [(String, [Op])]
                   deriving (Show)

instance JSON Op where
  -- This bit is a big ugly: we use the typeclass as a convenience for
  -- encoding the individual operations, but we actually decode them
  -- only in the context of the EditRequest.
  showJSON (Add x)    = showJSON x
  showJSON (Remove x) = showJSON x

  readJSON _ = fail "cannot read ops directly."

instance JSON GetRequest where 
  showJSON (GetRequest r) = showJSON r

  readJSON v = readJSON v >>= mapM readJSON >>= return . GetRequest

instance JSON EditRequest where
  showJSON (EditRequest r) = 
    showJSON . toJSObject $ mapsnd opsToObject r
    where
      mapsnd f = map (\(x, y) -> (x, f y)) 

      opsToObject = toJSObject 
                  . Map.toList 
                  . Map.fromListWith (++)
                  . map opToAssoc

      opToAssoc op@(Add x)    = ("add", [op])
      opToAssoc op@(Remove x) = ("remove", [op])

  readJSON (JSObject v) =
    mapM expandOps assoc >>= return . EditRequest
    where
      assoc = fromJSObject v

      expandOps (queue, (JSObject ops)) = 
        mapM parseOps assoc >>= \ops -> return $ (queue, concat ops)
        where
          assoc = fromJSObject ops

          parseOps ("add", JSArray queues)    = mapM parseAdd queues
          parseOps ("remove", JSArray queues) = mapM parseRemove queues
          parseOps op = fail $ printf "Invalid op %s" (show op)

          parseAdd q = readJSON q >>= return . Add 

          parseRemove q = readJSON q >>= return . Remove

  readJSON _ = fail "edit requests must be JSON objects."
