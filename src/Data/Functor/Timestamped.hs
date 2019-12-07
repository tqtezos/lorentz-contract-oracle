{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-orphans #-}

module Data.Functor.Timestamped where

import Prelude hiding ((>>), get, swap)
import qualified Control.Monad as Monad
import GHC.Generics (Generic, Generic1)
import Text.Show (Show(..))
import Text.Read (Read(..))
import Text.ParserCombinators.ReadPrec (look, get)

import Lorentz hiding (get)
import Michelson.Text

import Data.Aeson (eitherDecode)

-- | Zero or more, but drop the results
many_ :: Alternative f => f a -> f ()
many_ v = many_v
  where
    many_v = some_v <|> pure ()
    some_v = v *> many_v

instance Read Timestamp where
  readPrec = look Monad.>>= \inputStr ->
    case eitherDecode $ fromString inputStr of
      Left err -> fail err
      Right result ->
        many_ get *> return result

-- | A value with a `Timestamp`
data Timestamped a = Timestamped
  { timestamp   :: Timestamp
  , timeStamped :: a
  }
  deriving  (Generic)
  deriving  (Generic1)
  deriving  (Functor)
  deriving  (Foldable)
  deriving  (Traversable)

deriving instance Read a => Read (Timestamped a)

deriving instance Show a => Show (Timestamped a)

deriving instance IsoValue a => IsoValue (Timestamped a)

-- | Wrap `Timestamped`
toTimestamped :: forall a s. Timestamp & a & s :-> Timestamped a & s
toTimestamped = do
  pair
  coerce_ @(Timestamp, a) @(Timestamped a)

-- | Unwrap `Timestamped`
unTimestamped :: forall a s. Timestamped a & s :-> (Timestamp, a) & s
unTimestamped = do
  coerce_ @(Timestamped a) @(Timestamp, a)

-- | `unTimestamped` `>>` `car`
getTimestamp :: Timestamped a & s :-> Timestamp & s
getTimestamp = do
  unTimestamped
  car

-- | Assert timestamp is strictly before now
assertBeforeNow_ :: Timestamped a & s :-> s
assertBeforeNow_ = do
  getTimestamp
  now
  assertLe $ mkMTextUnsafe "in future"

-- | `assertBeforeNow_`, but keep the `Timestamped` value
assertBeforeNow :: Timestamped a & s :-> Timestamped a & s
assertBeforeNow = do
  dup
  dip assertBeforeNow_

-- | Assert the `Timestamped` value at the head of the stack is strictly newer
assertNewer_ :: Timestamped a & Timestamped a' & s :-> s
assertNewer_ = do
  getTimestamp
  dip getTimestamp
  assertLe $ mkMTextUnsafe "not newer"

-- | Duplicate the first two values on the stack
dupPair :: a & b & s :-> a & b & a & b & s
dupPair = do
  dip dup
  dup
  dip swap

-- | `assertNewer_`, but keep the `Timestamped` values
assertNewer :: Timestamped a & Timestamped a' & s :-> Timestamped a & Timestamped a' & s
assertNewer = do
  dupPair
  assertNewer_

