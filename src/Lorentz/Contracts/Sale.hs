{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-imports -Wno-orphans -Wno-partial-fields -Wno-deprecations -Wno-unused-do-bind #-}

module Lorentz.Contracts.Sale where

import Lorentz
import Michelson.Text
import Michelson.Typed.Arith
import Tezos.Core
import qualified Lorentz.Contracts.ManagedLedger.Babylon as Babylon
import qualified Lorentz.Contracts.ManagedLedger.Types as Babylon

import qualified Lorentz.Contracts.Tunnel as Tunnel


-- | Stub (`undefined`) instance
instance TypeHasDoc Babylon.Parameter where
  typeDocName _ = "Babylon parameter"
  typeDocMdReference _ = customTypeDocMdReference ("Babylon.Parameter", undefined) []
  typeDocMdDescription = "<Babylon.Parameter description>"
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

type Entrypoint param store = '[ param, store ] :-> ContractOut store

-- | FA1.2.1 address type
-- type TokenAddr = ContractAddr Babylon.Parameter
type TokenAddr = Address

-- | "held" is the number of tokens we're holding that we're
-- willing to trade for "wanted", the number of tokens we want
type Price = ("held" :! Natural, "wanted" :! Natural)

toPrice :: forall s. Natural : Natural : s :-> Price : s
toPrice = do
  pair
  coerce_

fromPrice :: forall s. Price : s :-> Natural : Natural : s
fromPrice = do
  coerce_
  unpair

data Parameter =
    Purchase !Price
  | UpdatePrice !Price
  | GetPrice !(View () Price)
  | GetHeldToken !(View () TokenAddr)
  | GetWallet !(View () Address)
  | GetWantedToken !(View () TokenAddr)
  deriving stock Generic
  deriving anyclass IsoValue

-- | The balance is this contract's allowance on the wallet on the held token contract
data Storage = Storage
  { adminAddress :: !Address
  , heldToken :: !TokenAddr
  , wallet :: !Address
  , wantedToken :: !TokenAddr
  , price :: !Price
  } deriving stock Generic
    deriving anyclass IsoValue

instance HasFieldOfType Storage name field =>
         StoreHasField Storage name field where
  storeFieldOps = storeFieldOpsADT

-- | Contstraint for `Storage`
type StorageC store = StorageContains store
  [ "adminAddress" := Address
  , "heldToken" := TokenAddr
  , "wallet" := Address
  , "wantedToken" := TokenAddr
  , "price" := Price
  ]

-- | Ensure is admin before continuing
withAuthorizedAdmin :: forall st a b.
  StorageC st => st : a :-> st : b -> st : a :-> st : b
withAuthorizedAdmin f = do
  stGetField #adminAddress; sender; eq
  if_ f (failCustom_ #senderIsNotAdmin)

-- | Fixed price offer contract:
-- Will accept any trade of held token for wanted token, up to size of pool (in wallet)
--
-- - Deposit tokens
-- - Update price
-- - Get price, contracts (amount left can be gotten from held token contract)
-- - Purchase up to amount left for current price
saleContract :: Contract Parameter Storage
saleContract = contractName "Managed Ledger Sale" $ do
  unpair
  entryCase @Parameter (Proxy @PlainEntryPointsKind)
    ( #cPurchase /-> purchase
    , #cUpdatePrice /-> updatePrice
    , #cGetPrice /-> getPrice
    , #cGetHeldToken /-> getHeldToken
    , #cGetWallet /-> getWallet
    , #cGetWantedToken /-> getWantedToken
    )

mkView :: forall a r s. a : ContractAddr r : s :-> View a r : s
mkView = do
  pair
  coerce_ @(a, ContractAddr r) @(View a r)

-- | Transfer from the wallet holding the @held@ token
transferFrom :: forall s. Address : Address : Natural : Address : s :-> Operation : s
transferFrom = do
  dip pair
  pair
  coerce_ @(Address, (Address, Natural)) @Babylon.TransferParams
  wrap_ #cTransfer
  dip $ do
    push $ unsafeMkMutez 0
    dip $ do
      contract @Babylon.Parameter
      ifNone
        (failUnexpected (mkMTextUnsafe "expected Babylon Parameter"))
        nop
  transferTokens

-- | check equality to price in storage or fail
checkPrice :: forall st s. StorageC st => Price : st : s :-> Price : st : s
checkPrice = do
  dip $ do
    stGetField #price
  dup
  dip $ do
    fromPrice
    swap
    dip $ do
      dip $ do
        fromPrice
      assertEq (mkMTextUnsafe "unexpected wanted price")
    assertEq (mkMTextUnsafe "unexpected held price")

-- type Price = ("held" :! Natural, "wanted" :! Natural)
-- | Purchase using the transferred tokens
purchase :: forall st. StorageC st => Entrypoint Price st
purchase = do
  dip $ do
    stGetField #wallet
  swap
  dip $ do
    checkPrice
    fromPrice
    sender
  pair
  -- [(wallet, sender), heldPrice, wantedPrice]
  stackType @[(Address, Address), Natural, Natural, st]
  dup
  dip $ do
    dip $ dip $ do
      dip $ do
        stGetField #heldToken
        -- _
        dip $ do
          stGetField #wantedToken
      swap
    -- [(wallet, sender), heldPrice, heldToken, wantedPrice, wantedToken, store]
    stackType @[(Address, Address), Natural, Address, Natural, Address, st]
    unpair
    transferFrom
  swap
  dip $ do
    stackType @[(Address, Address), Natural, Address, st]
    unpair
    swap
    -- [sender, wallet, wantedPrice, wantedToken, store]
    transferFrom
    dip nil
    cons
  cons
  pair

-- | Update the `Price`
updatePrice :: forall st. StorageC st => Entrypoint Price st
updatePrice = do
  swap
  withAuthorizedAdmin $ do
    swap
    stSetField #price
  nil
  pair

-- | Get the current `Price`
getPrice :: forall st. StorageC st => Entrypoint (View () Price) st
getPrice = do
  view_ (do cdr; stToField #price)

-- | Get the held token's contract address
getHeldToken :: forall st. StorageC st => Entrypoint (View () TokenAddr) st
getHeldToken = do
  view_ (do cdr; stToField #heldToken)

-- | Get the held token wallet address
getWallet :: forall st. StorageC st => Entrypoint (View () Address) st
getWallet = do
  view_ (do cdr; stToField #wallet)

-- | Get the wanted token's contract address
getWantedToken :: forall st. StorageC st => Entrypoint (View () TokenAddr) st
getWantedToken = do
  view_ (do cdr; stToField #wantedToken)

