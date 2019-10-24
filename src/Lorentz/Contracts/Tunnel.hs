{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Lorentz.Contracts.Tunnel where

import Lorentz
import Michelson.Text
import Tezos.Core

-- | Tunnel storage:
-- - Ensure `senderAddress` matches `sender`
-- - Send only to `targetAddress`
-- - Wrap the parameter for `targetAddress` using `wrapParameter`
data Storage a cp = Storage
  { senderAddress :: Address
  , targetAddress :: ContractAddr cp
  , wrapParameter :: Lambda a cp
  } deriving stock Generic
    deriving anyclass IsoValue

instance HasFieldOfType (Storage a cp) name field =>
         StoreHasField (Storage a cp) name field where
  storeFieldOps = storeFieldOpsADT

-- | A contract that forwards its parameter to the `targetAddress` using the
-- `wrapParameter` `Lambda` to encode the new argument type
--
-- Fails when `sender` is not `senderAddress`.
tunnelContract :: forall a cp. (KnownValue cp, NoOperation cp, NoBigMap cp) => Contract a (Storage a cp)
tunnelContract = do
  unpair
  dip $ do
    stGetField #senderAddress
    sender
    assertEq $ mkMTextUnsafe "unexpected sender"
    stGetField #wrapParameter
  exec
  dip $ do
    stGetField #targetAddress
    push $ unsafeMkMutez 0
  transferTokens
  dip nil
  cons
  pair

