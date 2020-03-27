{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields #-}

module Data.Functor.WithCounter where

import Prelude hiding ((>>), swap)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show(..))
import Text.Read (Read(..))

import Lorentz hiding (get)
import Michelson.Text

import Control.Monad.Trans.Writer.Strict
import qualified Options.Applicative as Opt

import Lorentz.Contracts.Parse (parseNatural)

-- | A value with a `Natural` counter
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

-- | Convert `WithCounter` to @`Writer` (`Sum` `Natural`)@.
--
-- Used to define the `Monad` instance
toWriter :: WithCounter a -> Writer (Sum Natural) a
toWriter WithCounter {..} =
  writer (value, Sum counter)

-- | Convert @`Writer` (`Sum` `Natural`)@ to `WithCounter`.
--
-- Used to define the `Monad` instance
unWriter :: Writer (Sum Natural) a -> WithCounter a
unWriter =
  (\(value, Sum counter) ->
    WithCounter counter value
  ) . runWriter

instance Applicative WithCounter where
  pure = unWriter . pure

  fs <*> xs =
    unWriter $
    toWriter fs <*> toWriter xs

instance Monad WithCounter where
  xs >>= f =
    unWriter $
    toWriter xs >>= toWriter . f

-- | Parse a `Natural` counter and value, given a `Opt.Parser` for the value
parseWithCounter :: Opt.Parser a -> Opt.Parser (WithCounter a)
parseWithCounter p =
  WithCounter <$>
    parseNatural "counter" <*>
    p

-- | Wrap `WithCounter`
toWithCounter :: forall a s. Natural & a & s :-> WithCounter a & s
toWithCounter = do
  pair
  forcedCoerce_ @(Natural, a) @(WithCounter a)

-- | Unwrap `WithCounter`
unWithCounter :: forall a s. WithCounter a & s :-> (Natural, a) & s
unWithCounter = do
  forcedCoerce_ @(WithCounter a) @(Natural, a)

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

