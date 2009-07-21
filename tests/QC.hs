
import Control.Monad

import qualified Data.Task.Queue    as Q
import qualified Data.Task.QueueSet as QS

import Test.QuickCheck.Batch
import Test.QuickCheck
import System.Time

instance Arbitrary Char where 
  arbitrary = choose (minBound, maxBound)
  coarbitrary = undefined

instance (Arbitrary a) => Arbitrary (Q.Queue a) where
  arbitrary = foldl (flip Q.add) Q.empty `liftM` arbitrary
  coarbitrary = undefined

instance (Arbitrary a) => Arbitrary (QS.QueueSet a) where
  arbitrary = foldl add QS.empty `liftM` arbitrary
              where
                add qs (t, name) = QS.add t name qs
  coarbitrary = undefined


prop_cannot_get_unexpired_tasks q =
  Q.size q' > 0 ==>
       Q.get (future 0)  1 q' == Nothing
    && Q.get (future 1)  1 q' == Nothing
    && Q.get (future 11) 1 q' /= Nothing
  where
    q' = consume 10 q :: Q.Queue String
    -- Set `now' to 1 second ahead of (TOD 0 0) since that's the
    -- default time of queue items.
    now = TOD 1 0
    future secs = addToClockTime (noTimeDiff { tdSec = secs }) now
    consume howlong q = 
      case Q.get now howlong q of
        Nothing      -> q
        Just (_, q') -> consume howlong q'

prop_done_is_really_done q =
  Q.size q' == 0
  where
    q' = takeAll q :: Q.Queue String
    now = TOD 1 0
    takeAll q =
      case Q.get now 0 q of
        Just ((ident, _), q') ->
          let Just q'' = Q.done ident q' 
          in takeAll $ q''
        Nothing               -> q

prop_done_is_really_done_qs :: QS.QueueSet String -> Property
prop_done_is_really_done_qs qs =
  let before = QS.size qs in
  before > 0 ==>
    let Just (_, qs') = QS.get (TOD 1 0) 1 (head (QS.names qs)) qs in
    QS.size qs' == before

options = TestOptions
          { no_of_tests     = 500
          , length_of_tests = 10
          , debug_tests     = False }

main = do
  runTests "simple" options
       [ run prop_cannot_get_unexpired_tasks,
         run prop_done_is_really_done,
         run prop_done_is_really_done_qs
       ]

