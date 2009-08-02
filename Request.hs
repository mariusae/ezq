-- | Parse requests from clients.
module Request(
    Request(..)
  , Response(..)
  , Op(..)
  ) where

import           Text.JSON
import           System.Time
import qualified Data.Map as Map
import           Data.Task.QueueSet(QueueSet)
import qualified Data.Task.QueueSet as QS
import qualified Data.Task.Queue as Q

data Op = Add String String 
        | Remove String Q.Ident
          deriving (Show)

data Request = GetRequest [(String, Int)]
             | EditRequest [Op]
               deriving (Show)

data Response = GetOk [(String, Q.Ident, String)]
              | EditOk
              | ResponseError String
                deriving (Show)

instance JSON Op where
  -- This bit is a big ugly: we use the typeclass as a convenience for
  -- encoding the individual operations, but we actually decode them
  -- only in the context of the EditRequest.
  showJSON (Add queue task)     = showJSON (queue, task)
  showJSON (Remove queue ident) = showJSON (queue, ident)

  readJSON _ = fail "cannot read ops directly."

instance JSON Request where
  showJSON (GetRequest r) = showJSON r
  showJSON (EditRequest r) =
    showJSON . toJSObject 
             $ mapsnd opsToObject
             $ collectQueues r
    where
      mapsnd f = map (\(x, y) -> (x, f y))

      concatAssocs = Map.toList . Map.fromListWith (++)

      collectQueues = concatAssocs . map opToAssoc
      opsToObject = toJSObject . concatAssocs

      opToAssoc op@(Add queue task)     = (queue, [("add", [showJSON task])])
      opToAssoc op@(Remove queue ident) = (queue, [("remove", [showJSON ident])])

  -- We're lucky: Get & edit requests are different types of json
  -- objects, so we can pattern match on them for parsing.
  readJSON (JSArray v) = mapM readJSON v >>= return . GetRequest
  readJSON (JSObject v) =
    concatMapM expandOps (fromJSObject v) >>= return . EditRequest
    where
      expandOps (queue, JSObject ops) =
        concatMapM expandOp (fromJSObject ops)
        where 
          expandOp ("add", JSArray vs)    = mapM parseAdd vs
          expandOp ("remove", JSArray vs) = mapM parseRemove vs

          parseAdd q = readJSON q >>= return . Add queue
          parseRemove q = readJSON q >>= return . Remove queue

      -- Kind of weird something like this isn't in the prelude?
      concatMapM f xs = mapM f xs >>= return . concat

  readJSON _ = fail "requests must be either JSON objects or arrays."

instance JSON Response where
  showJSON (GetOk r)            = showJSON r
  showJSON EditOk               = showJSON "ok"
  showJSON (ResponseError what) = showJSON $ "error: " ++ what

  readJSON = fail "cannot decode a GetResponse"
