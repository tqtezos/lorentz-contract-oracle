{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields #-}

module Data.Functor.WithCounter where

import Prelude hiding ((>>), swap)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show(..))
import Text.Read (Read(..))

import Lorentz hiding (get)
import Michelson.Text

-- | A value with a counter
data WithCounter a = WithCounter
  { counter :: !Natural
  , value   :: !a
  }
  deriving  (Generic)
  deriving  (Generic1)
  deriving  (Functor)
  deriving  (Foldable)
  deriving  (Traversable)

deriving instance Read a => Read (WithCounter a)

deriving instance Show a => Show (WithCounter a)

deriving instance IsoValue a => IsoValue (WithCounter a)

-- | Wrap `WithCounter`
toWithCounter :: forall a s. Natural & a & s :-> WithCounter a & s
toWithCounter = do
  pair
  coerce_ @(Natural, a) @(WithCounter a)

-- | Unwrap `WithCounter`
unWithCounter :: forall a s. WithCounter a & s :-> (Natural, a) & s
unWithCounter = do
  coerce_ @(WithCounter a) @(Natural, a)

-- | Assert the counter matches the given value
assertWithCounter_ :: WithCounter a & Natural & s :-> a & s
assertWithCounter_ = do
  unWithCounter
  unpair
  swap
  dip $ do
    assertEq $ mkMTextUnsafe "unequal counters"

-- | Increment the counter
incrementCounter :: WithCounter a & s :-> WithCounter a & s
incrementCounter = do
  unWithCounter
  unpair
  push (1 :: Natural)
  add
  toWithCounter

