{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields #-}

module Lorentz.Contracts.Admin.Simple.CmdLnArgs where

import Control.Applicative
import Data.Maybe
import Prelude (FilePath, IO)

import Lorentz hiding (get)
import Michelson.Typed.Scope
import Michelson.Typed.Sing
import Michelson.Typed.T
import Util.IO

import Data.Constraint
import Data.Singletons
import qualified Data.Text.Lazy.IO as TL
import qualified Options.Applicative as Opt

import Lorentz.Contracts.Parse
import Lorentz.Contracts.SomeContractParam
import Lorentz.Contracts.Util ()

import qualified Lorentz.Contracts.Admin.Simple as Simple
import qualified Lorentz.Contracts.Oracle.CmdLnArgs as OracleCmdLnArgs

data CmdLnArgs
  = Print (SomeSing T) (Maybe FilePath) Bool
  | Init
      { oracleContract :: Address
      , admin :: Address
      }
  | UpdateValue
      { newValue :: SomeContractParam
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

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ printSubCmd
  , initSubCmd
  , updateValueSubCmd
  , updateOracleAdminSubCmd
  , updateOracleContractSubCmd
  , updateAdminSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    printSubCmd =
      mkCommandParser "print"
      (Print <$>
       OracleCmdLnArgs.parseSomeT "value" <*>
       outputOptions <*>
       onelineOption)
      "Dump the Oracle contract in form of Michelson code"

    initSubCmd =
      mkCommandParser "init"
      (Init <$>
        parseAddress "oracleContract" <*>
        parseAddress "admin"
      )
      "Initial storage for the Oracle contract manager"

    updateValueSubCmd =
      mkCommandParser "update-value"
      (UpdateValue <$>
       OracleCmdLnArgs.parseSomeContractParam "newValue")
      "update value"

    updateOracleAdminSubCmd =
      mkCommandParser "update-admin"
      (UpdateOracleAdmin <$> parseAddress "newOracleAdmin")
      "update oracle admin"

    updateOracleContractSubCmd =
      mkCommandParser "update-admin"
      (UpdateOracleContract <$> parseAddress "newOracleContract")
      "update oracle admin"

    updateAdminSubCmd =
      mkCommandParser "update-admin"
      (UpdateAdmin <$> parseAddress "newAdmin")
      "update admin"

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Simple Admin contract CLI interface"
  ]

runCmdLnArgs :: CmdLnArgs -> IO ()
runCmdLnArgs = \case
  Print (SomeSing (st :: Sing t)) mOutput forceOneLine ->
    withDict (OracleCmdLnArgs.singIT st) $
    withDict (OracleCmdLnArgs.singTypeableT st) $
    OracleCmdLnArgs.assertOpAbsense @t $
    OracleCmdLnArgs.assertBigMapAbsense @t $
    case bigMapConstrained (STPair st (STc SCAddress)) of
      Nothing -> error "big map constraint not met for given type"
      Just Dict ->
        maybe TL.putStrLn writeFileUtf8 mOutput $
        printLorentzContract forceOneLine lcwDumb (Simple.simpleAdminContract @(Value t))
  Init {..} ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Simple.Storage oracleContract admin
  UpdateValue {..} ->
    fromSomeContractParam newValue $ \newValue' ->
      TL.putStrLn . printLorentzValue forceSingleLine $
      Simple.UpdateValue newValue'
  UpdateOracleAdmin {..} ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Simple.UpdateOracleAdmin @() newOracleAdmin
  UpdateOracleContract {..} ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Simple.UpdateOracleContract @() newOracleContract
  UpdateAdmin {..} ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Simple.UpdateAdmin @() newAdmin
  where
    forceSingleLine = True

