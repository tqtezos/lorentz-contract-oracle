{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields -Wno-orphans #-}

module Data.Functor.Signed where

import GHC.Generics (Generic, Generic1)
import Prelude hiding ((>>), get)
import Text.ParserCombinators.ReadPrec (look, get)
import Text.Read (Read(..))
import Text.Show (Show(..))
import qualified Control.Monad as Monad

import Lorentz hiding (get)
import Michelson.Text
import Tezos.Crypto hiding (checkSignature)
import Michelson.Interpret.Pack
import Michelson.Typed.Scope

import Data.Aeson (eitherDecode)

-- | Zero or more, but throw out the results
many_ :: Alternative f => f a -> f ()
many_ v = many_v
  where
    many_v = some_v <|> pure ()
    some_v = v *> many_v

instance Read Signature where
  readPrec = look Monad.>>= \inputStr ->
    case eitherDecode $ fromString inputStr of
      Left err -> fail err
      Right result ->
        many_ get *> return result

-- | A signed value
data Signed a = Signed
  { signature   :: !Signature
  , signedValue :: !a
  }
  deriving  (Generic)
  deriving  (Generic1)
  deriving  (Functor)
  deriving  (Foldable)
  deriving  (Traversable)

deriving instance Read a => Read (Signed a)

deriving instance Show a => Show (Signed a)

deriving instance IsoValue a => IsoValue (Signed a)

-- | Wrap `Signed`
toSigned :: forall a s. Signature & a & s :-> Signed a & s
toSigned = do
  pair
  forcedCoerce_ @(Signature, a) @(Signed a)

-- | Unwrap `Signed`
unSigned :: forall a s. Signed a & s :-> (Signature, a) & s
unSigned = do
  forcedCoerce_ @(Signed a) @(Signature, a)

-- | `checkSignature` for `Signed`
assertSigned_ :: (KnownValue a, NoOperation a, NoBigMap a)
  => PublicKey & Signed a & s :-> s
assertSigned_ = do
  dip $ do
    unSigned
    unpair
    dip pack
  checkSignature
  assert $ mkMTextUnsafe "invalid signature"

-- | Sign a value with the given `SecretKey`
signValue :: forall a. (IsoValue a, KnownValue a, NoOperation a, NoBigMap a)
  => SecretKey
  -> a
  -> Signed a
signValue secretKey x =
  forbiddenBigMap @(ToT a) $
  forbiddenOp @(ToT a) $
  Signed
    { signature   = sign secretKey (packValue' @(ToT a) $ toVal x)
    , signedValue = x
    }

