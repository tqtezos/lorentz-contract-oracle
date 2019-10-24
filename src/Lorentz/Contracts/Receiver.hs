
module Lorentz.Contracts.Receiver () where


-- data Receiver (rs :: [(Symbol, *, *)]) (cp :: *) (st :: *) where


-- runReceiverM :: ReceiverM rs (Contract cp st) -> Contract (Parameter cp) (Storage st)

-- data ReceiverT where
--   ReceiverT :: forall (a :: *) (r :: *) (ar :: *) (s :: [*]) (cp :: *) (st :: *). ReceiverT

-- data Receiver (rt :: ReceiverT) where
--   Receiver
--     :: forall (a :: *) (r :: *) (ar :: *) (s :: [*]) (cp :: *) (st :: *).
--       { unReceiver :: (forall s'. View a r : s' :-> ar : s')
--                    -> (View a r : ContractAddr ar : s :-> r : s)
--                    -> Contract cp st
--       } -> Receiver ('ReceiverT name a r ar s cp st)

-- type family ReceiverType name a r ar s cp st :: ReceiverT where
--   ReceiverType name a r ar s cp st = 'ReceiverT name (ToT a) (ToT r) (ToT ar) (ToT s) (ToT cp) (ToT st)


-- type MyReceivers =
--   [ ReceiverType "getBalance" Address Natural Babylon.Parameter Cxt1 cp st
--   , ReceiverType "getAllowance" Address Natural Babylon.Parameter Cxt2 cp st
--   ]

-- withReceiver :: HasReceiver rt rs => ReceiverM rs (Receiver rt -> a) -> ReceiverM rs a

-- -- wrapReceiver :: ReceiverM rs (Receiver rt -> a) -> ReceiverM (rt ': rs) a



-- (forall a r View a r : ContractAddr cp : s :-> r : s -> Contract a b)

-- callCC :: ((a -> m b) -> m a) -> m a

-- runReceiverContract :: ReceiverContract rs cp st
--   -> ([Contract], Contract (Receives rs cp) st)

-- callView :: forall a r cp s.
--      (forall s'. View a r : s' :-> cp : s')
--   -- -> (r : s :-> t)
--   -> (View a r : ContractAddr cp : s :-> r : s -> Contract a b)
--   -> Contract a b

-- data ReceiverContract (rs :: [*]) (cp :: *) (st :: *) where
--   ReceiverContract ::
--     -> Receivers rs st -- entrypoints for each of rs
--     -> Contract cp st
--     -> ReceiverContract rs cp st


-- The idea is that we can define all sorts of callbacks to the contract:
-- - each definition includes: calling a view parameter of another contract, somehow returning to the current state when it calls back to self:
--   * potentially, could set up proxies that store those values.. so then you'd call proxy with leftovers, then callback, then proxy receives, then proxy calls you with all
-- - to run it, we output a series of proxy contracts, one for each callback we want to receive

-- CallBackFor name cxt a r
--   View a r
--   name of callback
--   cxt is leftover types

-- Could also store cxt in self? seems better, but then have to manage different users' inputs?

