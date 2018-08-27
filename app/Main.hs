{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeApplications, 
    PolyKinds, TypeFamilies, UndecidableInstances,
    ScopedTypeVariables #-}
module Main where

import Lib
import GHC.TypeLits.Extra
import GHC.TypeLits
--import GHC.TypeNats
import Data.Proxy
import qualified Data.Ratio  as R
import qualified Numeric.Interval as I
import Numeric.Natural

test1 = Proxy :: Proxy (20 <= 5)
test2 = Proxy :: Proxy (GCD 20 5)
test3 :: Integer
test3 = natVal (Proxy @3)

data Ratio a b = Ratio

test4 = Ratio @1 @3
type Ex1 = Ratio 1 3


type family RTimes a b where
    RTimes (Ratio n1 d1) (Ratio n2 d2) = Ratio (n1 * n2) (d1 * d2)

type family RPlus a b where
    RPlus (Ratio n1 d1) (Ratio n2 d2) = Ratio ((n1 * d2) + (n2 * d1)) (d1 * d2)

type family Normalize a where
    Normalize (Ratio n d) = Ratio (Div n (GCD n d)) (Div d (GCD n d))

type Ex2 = RTimes Ex1 Ex1
type Ex3 = RPlus Ex1 Ex1
type Ex4 = Normalize Ex3

type family Recip a where
    Recip (Ratio n d) = Ratio d n

type family RCmp a b where
    RCmp (Ratio n1 d1) (Ratio n2 d2) = CmpNat (n1 * d2) (n2 * d1)

type family RNum a where
    RNum (Ratio n d) = n
type family RDen a where
    RDen (Ratio n d) = d


type Ex5 = RCmp Ex2 Ex3

--why don't these work?
--ratVal :: (KnownNat a, KnownNat b) => Proxy (Ratio a b) -> Integer
--ratVal Proxy = (natVal (Proxy :: Proxy a)) + (natVal (Proxy :: Proxy b))

--ratVal' :: (KnownNat a) => Proxy (Ratio a b) -> Integer
--ratVal' Proxy = natVal (Proxy :: Proxy a)

data Interval a b = Interval


main :: IO ()
main = someFunc
