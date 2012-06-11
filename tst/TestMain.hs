{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Network.NTP.Types

------------------------------------------------------------------------
-- Tests

main = defaultMain tests

tests = [ testProperty "add_sub_roundtrip" prop_add_sub_roundtrip
        , testProperty "midpoint_is_halfway" prop_midpoint_is_halfway
        , testProperty "toSeconds_fromSeconds_roundtrip" prop_toSeconds_fromSeconds_roundtrip
        , testProperty "fromSeconds_toSeconds_roundtrip" prop_fromSeconds_toSeconds_roundtrip
        ]

prop_add_sub_roundtrip t d = dur >= minDur ==>
                             dur <= maxDur ==>
    t `add` d `sub` t == d
  where
    dur    = unDuration d
    minDur = minTime - time
    maxDur = maxTime - time

    time = fromIntegral (unTime t)
    minTime = fromIntegral (unTime minBound)
    maxTime = fromIntegral (unTime maxBound)

prop_midpoint_is_halfway t1 t2 =
    mid t1 t2 `sub` t1 == half (t2 `sub` t1)

prop_toSeconds_fromSeconds_roundtrip =
    forAll (choose (-1000, 1000)) $ \dn ->
    let d = Duration dn
    in abs (d - fromSeconds (toSeconds d :: Double)) < Duration 1000

prop_fromSeconds_toSeconds_roundtrip =
    forAll (choose (-10, 10)) $ \(secs :: Double) ->
    abs (secs - toSeconds (fromSeconds secs)) < 0.001

------------------------------------------------------------------------
-- Utils

instance Arbitrary Time where
    shrink = map Time . shrinkIntegral . unTime
    arbitrary = fmap Time arbitraryBoundedIntegral

instance Arbitrary Duration where
    shrink = map Duration . shrinkIntegral . unDuration
    arbitrary = fmap Duration (choose (-maxDur, maxDur))
      where
        maxDur = fromIntegral (unTime maxBound)

half :: Duration -> Duration
half d = Duration (unDuration d `div` 2)
