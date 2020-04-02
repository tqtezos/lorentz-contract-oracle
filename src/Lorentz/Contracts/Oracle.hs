{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Oracle where

import Prelude hiding ((>>), drop, swap)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show(..))

import Lorentz hiding (get)
import Michelson.Text

import Data.Functor.Timestamped
-- import Lorentz.Contracts.Util ()

-- type Entrypoint param store = '[ param, store] :-> ContractOut store

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


-- instance Functor (View a) where
--   fmap _ = coerce

-- instance Foldable (View a) where
--   foldr _ = const

-- instance Traversable (View a) where
--   traverse _ = pure . coerce

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

-- deriving instance Read a => Read (Parameter a)

deriving instance Show a => Show (Parameter a)

deriving instance IsoValue a => IsoValue (Parameter a)

instance ParameterHasEntryPoints (Parameter Natural) where
  type ParameterEntryPointsDerivation (Parameter Natural) = EpdPlain

instance ParameterHasEntryPoints (Parameter (Timestamped Natural)) where
  type ParameterEntryPointsDerivation (Parameter (Timestamped Natural)) = EpdPlain


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

-- deriving instance Read a => Read (Storage a)

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
  -> Contract (Parameter a) (Storage a)
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
  => Contract (Parameter a) (Storage a)
uncheckedOracleContract =
  oracleContract nop

-- `assertBeforeNow` can fail because of the semantics of `now`,
-- i.e. that it returns (roughly) the timestamp of the _last_ block.
timestampedOracleContract ::
     forall a. NiceParameter a
  => Contract (Parameter (Timestamped a)) (Storage (Timestamped a))
timestampedOracleContract =
  oracleContract $ do
    -- assertBeforeNow
    assertNewer

