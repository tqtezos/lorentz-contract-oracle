{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields #-}

module Lorentz.Contracts.Admin.Simple where

import GHC.Generics (Generic, Generic1)
import Text.Show (Show(..))
import Text.Read (Read(..))
-- import Data.Typeable
import Data.Functor
import Data.Foldable
import Data.Traversable
-- import Data.Coerce
-- import Data.Function (const)
import Prelude (Enum(..))

import Lorentz
import Michelson.Text

import Lorentz.Contracts.Util ()

import Lorentz.Contracts.Oracle (Entrypoint)
import qualified Lorentz.Contracts.Oracle as Oracle

data Parameter a
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
      { newAdmin :: Address
      }
  deriving  (Generic)
  deriving  (Generic1)
  deriving  (Functor)
  deriving  (Foldable)
  deriving  (Traversable)

deriving instance Read a => Read (Parameter a)

deriving instance Show a => Show (Parameter a)

deriving instance IsoValue a => IsoValue (Parameter a)

data Storage =
  Storage
    { oracleContract :: Address
    , admin :: Address
    }
  deriving  (Generic)
  deriving  (Read)
  deriving  (Show)
  deriving  (IsoValue)

-- | Wrap `Storage`
toStorage :: Address & Address & s :-> Storage & s
toStorage = do
  pair
  coerce_

-- | Unwrap `Storage`
unStorage :: Storage & s :-> (Address, Address) & s
unStorage = coerce_

simpleAdminContract ::
     forall a. (IsoValue a, KnownValue a, NoOperation a, NoBigMap a)
  => Contract (Parameter a) Storage
simpleAdminContract = do
  unpair
  caseT @(Parameter a)
    ( #cUpdateValue /-> updateValue
    , #cUpdateOracleAdmin /-> updateOracleAdmin (Proxy @a)
    , #cUpdateOracleContract /-> updateOracleContract
    , #cUpdateAdmin /-> updateAdmin
    )

-- | Call the `Oracle.oracleContract`
callOracle :: forall a s. (KnownValue a, NoOperation a, NoBigMap a)
  => Oracle.Parameter a & Address & s :-> [Operation] & s
callOracle = do
  dip $ do
    contract @(Oracle.Parameter a)
    assertSome $ mkMTextUnsafe "not Oracle"
    push (toEnum 0 :: Mutez)
  transferTokens
  dip nil
  cons

-- | Call the `Oracle.oracleContract`, using the `Address` in `Storage`
callOracleWithStorage :: (KnownValue a, NoOperation a, NoBigMap a)
  => (forall s'. t & s' :-> Oracle.Parameter a & s')
  -> t & Storage & s :-> ([Operation], Storage) & s
callOracleWithStorage toParam = do
  dip $ do
    unStorage
    unpair
    dip $ do
      Oracle.assertAdmin
    dup
    dip toStorage
  toParam
  callOracle
  pair

-- | Call `Oracle.UpdateValue`
updateValue :: forall a. (KnownValue a, NoOperation a, NoBigMap a)
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
  dip $ do
    unStorage
    cdr
    Oracle.assertAdmin
  toStorage
  nil
  pair

-- | Update the admin `Address`
updateAdmin ::
  Entrypoint Address Storage
updateAdmin = do
  dip $ do
    unStorage
    unpair
    dip Oracle.assertAdmin_
  toStorage
  nil
  pair

