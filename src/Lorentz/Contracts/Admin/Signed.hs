{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields #-}

module Lorentz.Contracts.Admin.Signed where

import GHC.Generics (Generic, Generic1)
import Text.Show (Show(..))
import Text.Read (Read(..))
import Data.Functor
import Data.Foldable
import Data.Traversable

import Lorentz

import Lorentz.Contracts.Util ()

import Data.Functor.Signed
import Data.Functor.WithCounter
import Lorentz.Contracts.Oracle (Entrypoint)
import qualified Lorentz.Contracts.Oracle as Oracle
import qualified Lorentz.Contracts.Admin.Simple as Simple

data Parameter' a
  = UpdateValue
      { newValue :: a
      }
  | UpdateOracleAdmin
      { newOracleAdmin :: Address
      }
  | UpdateOracleContract
      { newOracleContract :: Address
      }
  | UpdateAdmin
      { newAdmin :: PublicKey
      }
  deriving  (Generic)
  deriving  (Generic1)
  deriving  (Functor)
  deriving  (Foldable)
  deriving  (Traversable)

deriving instance Read a => Read (Parameter' a)

deriving instance Show a => Show (Parameter' a)

deriving instance IsoValue a => IsoValue (Parameter' a)

data Storage' =
  Storage'
    { admin          :: PublicKey
    , oracleContract :: Address
    }
  deriving  (Generic)
  deriving  (Read)
  deriving  (Show)
  deriving  (IsoValue)

-- | Wrap `Storage'`
toStorage' :: PublicKey & Address & s :-> Storage' & s
toStorage' = do
  pair
  coerce_

-- | Unwrap `Storage'`
unStorage' :: Storage' & s :-> (PublicKey, Address) & s
unStorage' = coerce_

type Parameter a = Signed (WithCounter (Parameter' a))

type Storage = WithCounter Storage'

-- | Wrap `Storage`
toStorage :: Natural & PublicKey & Address & s :-> Storage & s
toStorage = do
  dip pair
  pair
  coerce_

-- | Unwrap `Storage`
unStorage :: Storage & s :-> (Natural, (PublicKey, Address)) & s
unStorage = coerce_

signedAdminContract ::
     forall a. (IsoValue a, KnownValue a, NoOperation a, NoBigMap a)
  => Contract (Parameter a) Storage
signedAdminContract = do
  unpair
  -- ensure signed
  dip $ do
    dup
    unStorage
    unpair
    swap
    car
  dup
  dip $ do
    swap
    assertSigned_
  unSigned
  cdr
  -- check and increment the counter
  assertWithCounter_
  dip incrementCounter
  -- case match on parameters
  caseT @(Parameter' a)
    ( #cUpdateValue /-> updateValue
    , #cUpdateOracleAdmin /-> updateOracleAdmin (Proxy @a)
    , #cUpdateOracleContract /-> updateOracleContract
    , #cUpdateAdmin /-> updateAdmin
    )

-- | Call the `Oracle.oracleContract`, using the `Address` in `Storage`
callOracleWithStorage ::
     forall a t s. (KnownValue a, NoOperation a, NoBigMap a)
  => (forall s'. t & s' :-> Oracle.Parameter a & s')
  -> t & Storage & s :-> ([Operation], Storage) & s
callOracleWithStorage toParam = do
  dip $ do
    dup
    unStorage
    cdr
    cdr
  toParam
  Simple.callOracle
  pair

-- | Call `Oracle.UpdateValue`
updateValue ::
     forall a. (KnownValue a, NoOperation a, NoBigMap a)
  => Entrypoint a Storage
updateValue = callOracleWithStorage Oracle.toUpdateValue

-- | Call `Oracle.UpdateAdmin`
updateOracleAdmin :: forall a. (KnownValue a, NoOperation a, NoBigMap a)
  => Proxy a
  -> Entrypoint Address Storage
updateOracleAdmin _ = callOracleWithStorage @a Oracle.toUpdateAdmin

-- | Update the Oracle contract `Address`
updateOracleContract ::
  Entrypoint Address Storage
updateOracleContract = do
  swap
  unStorage
  unpair
  dip car
  toStorage
  nil
  pair

-- | Update the admin `PublicKey`
updateAdmin ::
  Entrypoint PublicKey Storage
updateAdmin = do
  swap
  unStorage
  unpair
  dip $ do
    cdr
    swap
  toStorage
  nil
  pair

