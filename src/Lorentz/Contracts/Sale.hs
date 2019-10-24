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

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-imports -Wno-orphans -Wno-partial-fields -Wno-deprecations #-}

module Lorentz.Contracts.Sale where

import Lorentz
import Michelson.Text
import Tezos.Core
import qualified Lorentz.Contracts.ManagedLedger.Babylon as Babylon
import qualified Lorentz.Contracts.ManagedLedger.Types as Babylon

import qualified Lorentz.Contracts.Tunnel as Tunnel


-- TODO:
-- - return tokens when wrong price
-- - not sure that the number of tokens being traded is actually checked to be
--    sufficient (i.e. after checking that the other is a multiple)

-- | Stub instance
instance TypeHasDoc Babylon.Parameter where
  typeDocName _ = "Babylon parameter"
  typeDocMdReference _ = customTypeDocMdReference ("Babylon.Parameter", undefined) []
  typeDocMdDescription = "<Babylon.Parameter description>"
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

type Entrypoint param store = '[ param, store ] :-> ContractOut store

-- | FA1.2.1 address type
type TokenAddr = ContractAddr Babylon.Parameter

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

-- | Set the tunnels
type SetTunnels = ("allowanceTunnel" :! ContractAddr Natural, "balanceTunnel" :! ContractAddr Natural)

data Parameter =
    Purchase !Price
  | WaitForAllowance !Natural
  | WaitForBalance !Natural
  | UpdatePrice !Price
  | GetPrice !(View () Price)
  | GetHeldToken !(View () TokenAddr)
  | GetWallet !(View () Address)
  | GetWantedToken !(View () TokenAddr)
  | SetTunnels !SetTunnels
  deriving stock Generic
  deriving anyclass IsoValue

-- | The state when waiting for balance
type WaitForBalanceS = ("callerSource" :! Address, "allowanceBalance" :! Natural)

toWaitForBalanceS :: forall s. Address : Natural : s :-> WaitForBalanceS : s
toWaitForBalanceS = do
  pair
  coerce_

fromWaitForBalanceS :: forall s. WaitForBalanceS : s :-> Address : Natural : s
fromWaitForBalanceS = do
  coerce_
  unpair

-- | `Ready` if not in the middle of a callback
--
-- `WaitingForAllowance` when waiting for `getAllowance`, etc.
data CallbackState =
    Ready
  | WaitingForAllowance { callerSource :: !Address }
  | WaitingForBalance !WaitForBalanceS
  deriving stock Generic
  deriving anyclass IsoValue

-- | The balance is this contract's allowance on the wallet on the held token contract
data Storage = Storage
  { adminAddress :: !Address
  , heldToken :: !TokenAddr
  , wallet :: !Address
  , allowanceTunnel :: !(Maybe (ContractAddr Natural))
  , wantedToken :: !TokenAddr
  , balanceTunnel :: !(Maybe (ContractAddr Natural))
  , price :: !Price
  , callbackState :: !CallbackState
  } deriving stock Generic
    deriving anyclass IsoValue


-- | @initialStorage adminAddress' heldToken' wallet' wantedToken' price'@
initialStorage :: Address -> TokenAddr -> Address -> TokenAddr -> Price -> Storage
initialStorage adminAddress' heldToken' wallet' wantedToken' price' =
  Storage
    adminAddress'
    heldToken'
    wallet'
    Nothing
    wantedToken'
    Nothing
    price'
    Ready

-- | @allowanceTunnelStorage heldToken' saleAddress'@
allowanceTunnelStorage :: TokenAddr -> ContractAddr Parameter -> Tunnel.Storage Natural Parameter
allowanceTunnelStorage (ContractAddr heldToken') saleAddress' =
  Tunnel.Storage
    heldToken'
    saleAddress'
    (wrap_ #cWaitForAllowance)

-- | @balanceTunnelStorage wantedToken' saleAddress'@
balanceTunnelStorage :: TokenAddr -> ContractAddr Parameter -> Tunnel.Storage Natural Parameter
balanceTunnelStorage (ContractAddr wantedToken') saleAddress' =
  Tunnel.Storage
    wantedToken'
    saleAddress'
    (wrap_ #cWaitForBalance)


instance HasFieldOfType Storage name field =>
         StoreHasField Storage name field where
  storeFieldOps = storeFieldOpsADT

-- | Contstraint for `Storage`
type StorageC store = StorageContains store
  [ "adminAddress" := Address
  , "heldToken" := TokenAddr
  , "wallet" := Address
  , "allowanceTunnel" := Maybe (ContractAddr Natural)
  , "wantedToken" := TokenAddr
  , "balanceTunnel" := Maybe (ContractAddr Natural)
  , "price" := Price
  , "callbackState" := CallbackState
  ]

-- | Ensure is admin before continuing
withAuthorizedAdmin :: forall st a b.
  StorageC st => st : a :-> st : b -> st : a :-> st : b
withAuthorizedAdmin f = do
  -- doc $ DRequireRole "administrator"
  stGetField #adminAddress; sender; eq
  if_ f (failCustom_ #senderIsNotAdmin)

-- | Ensure tunnels set before continuing
assertTunnels :: forall st s. StorageC st => st : s :-> st : s
assertTunnels = do
  stGetField #allowanceTunnel
  ifNone
    (failUnexpected (mkMTextUnsafe "no allowance proxy"))
    (do
      drop
      stGetField #balanceTunnel
      ifNone
        (failUnexpected (mkMTextUnsafe "no balance proxy"))
        drop
    )

-- | Unsafely get allowance tunnel: will fail if unset
unsafeAllowanceTunnel :: forall st s. StorageC st => st : s :-> ContractAddr Natural : st : s
unsafeAllowanceTunnel = do
  stGetField #allowanceTunnel
  ifNone
    (failUnexpected (mkMTextUnsafe "no allowance proxy"))
    nop

-- | Unsafely get balance tunnel: will fail if unset
unsafeBalanceTunnel :: forall st s. StorageC st => st : s :-> ContractAddr Natural : st : s
unsafeBalanceTunnel = do
  stGetField #balanceTunnel
  ifNone
    (failUnexpected (mkMTextUnsafe "no balance proxy"))
    nop

-- | Ensure the `sender` is the allowance tunnel
assertAllowanceTunnel :: forall st s. StorageC st => st : s :-> st : s
assertAllowanceTunnel = do
  unsafeAllowanceTunnel
  address
  sender
  assertEq (mkMTextUnsafe "expected allowance proxy")

-- | Ensure the `sender` is the balance tunnel
assertBalanceTunnel :: forall st s. StorageC st => st : s :-> st : s
assertBalanceTunnel = do
  unsafeBalanceTunnel
  address
  sender
  assertEq (mkMTextUnsafe "expected balance proxy")

-- | Assert in the `Ready` state
assertReady :: forall st s. StorageC st => st : s :-> st : s
assertReady = do
  stGetField #callbackState
  caseT
    ( #cReady /-> nop
    , #cWaitingForAllowance /-> failUnexpected (mkMTextUnsafe "waiting for allowance")
    , #cWaitingForBalance /-> failUnexpected (mkMTextUnsafe "waiting for balance")
    )

-- | Assert in the `WaitingForAllowance` state
assertWaitingForAllowance :: forall st s. StorageC st => st : s :-> Address : st : s
assertWaitingForAllowance = do
  stGetField #callbackState
  caseT
    ( #cReady /-> failUnexpected (mkMTextUnsafe "got allowance but ready")
    , #cWaitingForAllowance /-> do
        dup
        dip $ do
          source
          assertEq (mkMTextUnsafe "unexpected source")
          assertAllowanceTunnel
    , #cWaitingForBalance /-> failUnexpected (mkMTextUnsafe "got allowance but waiting for balance")
    )

-- | Assert in the `WaitingForBalance` state
assertWaitingForBalance :: forall st s. StorageC st => st : s :-> WaitForBalanceS : st : s
assertWaitingForBalance = do
  stGetField #callbackState
  caseT
    ( #cReady /-> failUnexpected (mkMTextUnsafe "got balance but ready")
    , #cWaitingForAllowance /-> failUnexpected (mkMTextUnsafe "got balance but waiting for allowance")
    , #cWaitingForBalance /-> do
        dup
        dip $ do
          fromWaitForBalanceS
          dip drop
          source
          assertEq (mkMTextUnsafe "unexpected source")
          assertBalanceTunnel
    )

-- | Enter the `Ready` state
ready_ :: forall st s. StorageC st => st : s :-> st : s
ready_ = do
  push Ready
  stSetField #callbackState

-- | Enter the `WaitingForAllowance` state
waitForAllowance_ :: forall st s. StorageC st => Address : st : s :-> st : s
waitForAllowance_ = do
  wrap_ #cWaitingForAllowance
  stSetField #callbackState

-- | Enter the `WaitingForBalance` state
waitForBalance_ :: forall st s. StorageC st => WaitForBalanceS : st : s :-> st : s
waitForBalance_ = do
  wrap_ #cWaitingForBalance
  stSetField #callbackState


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
    , #cWaitForAllowance /-> waitForAllowance
    , #cWaitForBalance /-> waitForBalance
    , #cUpdatePrice /-> updatePrice
    , #cGetPrice /-> getPrice
    , #cGetHeldToken /-> getHeldToken
    , #cGetWallet /-> getWallet
    , #cGetWantedToken /-> getWantedToken
    , #cSetTunnels /-> setTunnels
    )

mkView :: forall a r s. a : ContractAddr r : s :-> View a r : s
mkView = do
  pair
  coerce_ @(a, ContractAddr r) @(View a r)

-- | Call @getAllowance@ on the @held@ token contract
getAllowance_ :: forall st s. StorageC st => st : s :-> Operation : st : s
getAllowance_ = do
  self
  address
  dip $ do
    stGetField #wallet
    dip $ do
      unsafeAllowanceTunnel
  pair
  coerce_
  mkView @Babylon.GetAllowanceParams @Natural
  wrap_ #cGetAllowance
  dip $ do
    push $ unsafeMkMutez 0
    dip $ do
      stGetField #heldToken
  transferTokens

-- | Call @getBalance@ on the @wanted@ token contract
getBalance_ :: forall st s. StorageC st => st : s :-> Operation : st : s
getBalance_ = do
  self
  address
  dip $ do
    unsafeBalanceTunnel
  mkView @Address @Natural
  wrap_ #cGetBalance
  dip $ do
    push $ unsafeMkMutez 0
    dip $ do
      stGetField #wantedToken
  transferTokens

-- | Transfer from the wallet holding the @held@ token
transferFromWallet :: forall st s. StorageC st => Address : Natural : st : s :-> Operation : st : s
transferFromWallet = do
  dip $ do
    dip $ do
      stGetField #wallet
    swap
    pair
  pair
  coerce_ @(Address, (Address, Natural)) @Babylon.TransferParams
  wrap_ #cTransfer
  dip $ do
    push $ unsafeMkMutez 0
    dip $ do
      stGetField #heldToken
  transferTokens

-- | Transfer from self, holding the @wanted@ token
transferFromSelf :: forall st s. StorageC st => Address : Natural : st : s :-> Operation : st : s
transferFromSelf = do
  dip $ do
    dip $ do
      self
      address
    swap
    pair
  pair
  coerce_ @(Address, (Address, Natural)) @Babylon.TransferParams
  wrap_ #cTransfer
  dip $ do
    push $ unsafeMkMutez 0
    dip $ do
      stGetField #wantedToken
  transferTokens

-- | check equality to price in storage or fail
--
-- TODO: this needs to be called after the @WaitFor..@ callbacks
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

-- | If balances don't match price, return tokens to purchaser
-- otherwise, rescale number of purchased (held) tokens to buying amount
--
-- @
--  [cash in hand, tokens to buy, purchaser]
-- @
checkPurchasePrice :: forall st. StorageC st => [Natural, Natural, Address, st] :-> [Either Operation (Natural, Natural, Address), st]
checkPurchasePrice = do
  swap
  dip $ do
    dup
    dip $ do
      -- [cash in hand, purchaser, st]
      dip $ do
        swap
        stGetField #price
      -- [cash in hand, price, st, purchaser]
      calculatePurchasedTokens
      -- [purchased tokens, st, purchaser]
    -- [cash in hand, purchased tokens, st, purchaser]
    swap
    -- [purchased tokens, cash in hand, st, purchaser]
  -- [tokens to buy, purchased tokens, cash in hand, st, purchaser]
  swap
  -- [purchased tokens, tokens to buy, cash in hand, st, purchaser]
  ifNone
    (do -- refund cash in hand
      -- [tokens to buy, cash in hand, st, purchaser]
      drop
      dip swap
      swap
      transferFromSelf
      left
    )
    (do -- ensure enough (le) tokens (else refund); return new tokens to buy
      -- [purchased tokens, tokens to buy, cash in hand, st, purchaser]
      (push False)
      if_ -- ifLe
        (do
          -- [purchased tokens, tokens to buy, cash in hand, st, purchaser]
          dip $ do
            drop
            dip swap
          -- [purchased tokens, cash in hand, purchaser, st]
          dip pair
          pair
          coerce_ @(Natural, (Natural, Address)) @(Natural, Natural, Address)
          right
        )
        (do -- process refund
          drop
          drop
          dip swap
          swap
          transferFromSelf
          left
        )
    )

-- | Calculate how many tokens were purchased, or fail
--
-- @
--  [cash in hand, price, s]
-- @
calculatePurchasedTokens :: forall s. Natural : Price : s :-> Maybe Natural : s
calculatePurchasedTokens = do
  dip $ do
    fromPrice
    swap
  ediv
  ifNone
    (do
      drop
      none
    )
    (do
      unpair
      swap
      -- [mod, div, held number]
      -- stackType @[Natural, Natural, Natural, s]
      push 0
      ifEq -- if (mod sentTokens wantedTokenPrice == 0)
        (do
          mul
          some
        )
        (do
          drop
          drop
          none
        )
    )

-- | Purchase using the transferred tokens
purchase :: forall st. StorageC st => Entrypoint Price st
purchase = do
  checkPrice
  dip $ do
    assertTunnels
    assertReady
  drop
  getAllowance_
  dip $ do
    source
    waitForAllowance_
    nil
  cons
  pair

-- | Wait for the @getAllowance@ callback
waitForAllowance :: forall st. StorageC st => Entrypoint Natural st
waitForAllowance = do
  dip $ do
    assertTunnels
    assertWaitingForAllowance
  swap
  toWaitForBalanceS
  dip $ do
    getBalance_
    dip nil
    cons
    swap
  waitForBalance_
  swap
  pair

-- | Wait for the @getBalance@ callback
waitForBalance :: forall st. StorageC st => Entrypoint Natural st
waitForBalance = do
  dip $ do
    assertTunnels
    assertWaitingForBalance
    fromWaitForBalanceS
    swap
  -- [tokens bought with, bought tokens, purchaser address, storage]
  -- stackType @[Natural, Natural, Address, st]
  checkPurchasePrice
  ifLeft
    (do -- refund operation
      dip nil
      cons
    )
    (do
      coerce_ @(Natural, Natural, Address) @(Natural, (Natural, Address))
      unpair
      dip unpair
      dip $ do
        swap
        -- [purchaser address, bought tokens, storage]
        stackType @[Address, Natural, st]
        transferFromWallet
        swap
        stackType @[st, Operation]
      -- stackType @[Natural, st, Operation]
      dip $ do
        stGetField #wallet
      swap
      transferFromSelf
      dip $ do
        swap
        dip nil
        cons
      cons
    )
  dip ready_
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

-- | Set the tunnel contracts
setTunnels :: forall st. StorageC st => Entrypoint SetTunnels st
setTunnels = do
  swap
  withAuthorizedAdmin $ do
    swap
    coerce_ @SetTunnels @(ContractAddr Natural, ContractAddr Natural)
    unpair
    dip swap
    some
    stSetField #allowanceTunnel
    swap
    some
    stSetField #balanceTunnel
  nil
  pair

