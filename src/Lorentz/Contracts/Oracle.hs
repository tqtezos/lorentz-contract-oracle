{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Oracle where

import Prelude hiding ((>>), drop, swap)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show(..))

import Lorentz hiding (get)
import Michelson.Text

import Data.Functor.Timestamped

-- | Assert sender is the given address or fail with an error
assertAdmin_ :: Address & s :-> s
assertAdmin_ = do
  sender
  assertEq $ mkMTextUnsafe "only admin may update"

-- | `assertAdmin_`, but preserve the stack
assertAdmin :: Address & s :-> Address & s
assertAdmin = do
  dup
  dip assertAdmin_

data Parameter a
  = GetValue
      { viewValue :: View () a
      }
  | UpdateValue
      { newValue :: a
      }
  | UpdateAdmin
      { newAdmin :: Address
      }
  deriving  (Generic)
  deriving  (Generic1)

deriving instance Show a => Show (Parameter a)

deriving instance IsoValue a => IsoValue (Parameter a)

instance (IsoValue a, HasTypeAnn a) => ParameterHasEntryPoints (Parameter a) where
  type ParameterEntryPointsDerivation (Parameter a) = EpdPlain

-- | Wrap `UpdateValue`
toUpdateValue :: forall a s. KnownValue a => a & s :-> Parameter a & s
toUpdateValue = do
  left
  right
  forcedCoerce_ @(Either (View () a) (Either a Address)) @(Parameter a)

-- | Wrap `UpdateAdmin`
toUpdateAdmin :: forall a s. KnownValue a => Address & s :-> Parameter a & s
toUpdateAdmin = do
  right
  right
  forcedCoerce_ @(Either (View () a) (Either a Address)) @(Parameter a)

data Storage a =
  Storage
    { currentValue :: a
    , admin :: Address
    }
  deriving  (Generic)
  deriving  (Generic1)
  deriving  (Functor)
  deriving  (Foldable)
  deriving  (Traversable)

deriving instance Show a => Show (Storage a)

deriving instance IsoValue a => IsoValue (Storage a)

-- | Wrap `Storage`
toStorage :: a & Address & s :-> Storage a & s
toStorage = do
  pair
  forcedCoerce_

-- | Unwrap `Storage`
unStorage :: Storage a & s :-> (a, Address) & s
unStorage = forcedCoerce_

oracleContract ::
     forall a. (NiceParameter a)
  => (forall s. a & a & s :-> a & a & s) -- ^ new_value, previous_value
  -> ContractCode (Parameter a) (Storage a)
oracleContract check = do
  unpair
  caseT @(Parameter a)
    ( #cGetValue /-> getValue
    , #cUpdateValue /-> updateValue check
    , #cUpdateAdmin /-> updateAdmin
    )

getValue ::
     forall a. (NiceParameter a)
  => Entrypoint (View () a) (Storage a)
getValue = do
  view_ $ do
    cdr
    unStorage
    car

updateValue ::
     forall a. ()
  => (forall s. a & a & s :-> a & a & s)
  -> Entrypoint a (Storage a)
updateValue check = do
  dip $ do
    unStorage
    unpair
  check
  dip $ do
    drop
    assertAdmin
  toStorage
  nil
  pair

updateAdmin ::
  Entrypoint Address (Storage a)
updateAdmin = do
  dip $ do
    unStorage
    unpair
    dip assertAdmin_
  swap
  toStorage
  nil
  pair

--

uncheckedOracleContract ::
     forall a. NiceParameter a
  => ContractCode (Parameter a) (Storage a)
uncheckedOracleContract =
  oracleContract nop

-- `assertBeforeNow` can fail because of the semantics of `now`,
-- i.e. that it returns (roughly) the timestamp of the _last_ block.
timestampedOracleContract ::
     forall a. NiceParameter a
  => ContractCode (Parameter (Timestamped a)) (Storage (Timestamped a))
timestampedOracleContract =
  oracleContract $ do
    -- assertBeforeNow
    assertNewer

