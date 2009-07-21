module Data.Task.QueueSet (
    QueueSet, Ident
  , empty, add, get, getAny, done, size, names
  ) where

import           System.Time
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Task.Queue as Q

type Ident = (String, Q.Ident)

-- | A QueueSet is a set of named queues.
data QueueSet a = QS (Map String (Q.Queue a))
                  deriving (Show)

empty = QS $ Map.empty

add t name (QS m) =
  QS $ Map.insert name (Q.add t q) m
  where
    q = case Map.lookup name m of
          Just q  -> q
          Nothing -> Q.empty

-- | Get one task from any of the named queues
getAny :: System.Time.ClockTime                      -- ^ Current time
       -> [(String, Int)]                            -- ^ [(Name, howlong)]
       -> QueueSet a                                 -- ^ The queueset
       -> Maybe (((String, Q.Ident), a), QueueSet a)
getAny now names (QS m) =
  takeFirst eligible
  where
    eligible = catMaybes $ map maybeGetTask queues

    queues = mapMaybe maybeGetQueue names

    maybeGetQueue (name, howlong) = 
      Map.lookup name m >>= Just . (,,) name howlong

    takeFirst [] = Nothing
    takeFirst (((ident, t), (name, q)):_) = 
      Just (((name, ident), t), QS (Map.insert name q m))

    maybeGetTask (name, howlong, q) = case Q.get now howlong q of
                                        Nothing      -> Nothing
                                        Just (x, q') -> Just (x, (name, q'))

get now howlong name (QS m) =
  Map.lookup name m >>= Q.get now howlong >>= \((ident, t), q) ->
    return $ (((name, ident), t), QS (Map.insert name q m))

done (name, ident) (QS m) =
  Map.lookup name m >>= Q.done ident >>= \q ->
    return $ QS (Map.insert name q m)

size (QS m) = sum $ map Q.size (Map.elems m)

names (QS m) = Map.keys m
