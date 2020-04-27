
module Language.Haskell.TH.Orphans where

import Language.Haskell.TH.Syntax

instance Lift (Q Exp) where
  lift = id

