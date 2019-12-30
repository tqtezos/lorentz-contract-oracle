{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields #-}

module Lorentz.Contracts.Admin.Signed.CmdLnArgs where

import Control.Applicative
import Data.Maybe
import Data.Typeable
import Data.Proxy
import Data.Function
import Data.String
import Data.List ((++))
import Prelude (FilePath, IO, Monad)
import Data.Functor

import Lorentz hiding (get)
import Michelson.Typed.Scope
import Michelson.Typed.Sing
import Michelson.Typed.T
import Tezos.Crypto (SecretKey, toPublic)
import Util.IO

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Constraint
import Data.Singletons
import qualified Data.Text.Lazy.IO as TL
import qualified Options.Applicative as Opt

import Lorentz.Contracts.Parse
import Lorentz.Contracts.SomeContractParam
import Lorentz.Contracts.Util ()

import qualified Lorentz.Contracts.Admin.Signed as Signed
import qualified Lorentz.Contracts.Oracle.CmdLnArgs as OracleCmdLnArgs
import Data.Functor.Signed (signValue)
import Data.Functor.WithCounter (WithCounter(..), parseWithCounter)

data CmdLnArgs
  = Print (SomeSing T) (Maybe FilePath) Bool
  | Init
      { admin          :: PublicKey
      , oracleContract :: Address
      }
  | UpdateValue
      { newValue    :: SomeContractParam
      , signWithKey :: MaybeT WithCounter SecretKey
      }
  | UpdateOracleAdmin
      { newOracleAdmin :: Address
      , signWithKey    :: MaybeT WithCounter SecretKey
      }
  | UpdateOracleContract
      { newOracleContract :: Address
      , signWithKey       :: MaybeT WithCounter SecretKey
      }
  | UpdateAdmin
      { newAdmin :: Either PublicKey (WithCounter SecretKey)
      -- , signWithKey :: SecretKey
      }

-- | Parse an `PublicKey` argument, given its field name
parsePublicKey :: String -> Opt.Parser PublicKey
parsePublicKey name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "key"
    , Opt.help $ "PublicKey of the " ++ name ++ "."
    ]

parseMaybeT :: Monad f => Opt.Parser (f a) -> Opt.Parser (MaybeT f a)
parseMaybeT p =
  fmap lift p <|> pure empty

parseMaybeSignArg :: Opt.Parser (MaybeT WithCounter SecretKey -> CmdLnArgs) -> Opt.Parser CmdLnArgs
parseMaybeSignArg =
  flip (<*>) $
  parseMaybeT $
    (parseWithCounter parseSecretKey)

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
        parsePublicKey "admin" <*>
        parseAddress "oracleContract"
      )
      "Initial storage for the Oracle contract manager"

    updateValueSubCmd =
      mkCommandParser "update-value"
      (parseMaybeSignArg $ UpdateValue <$>
       OracleCmdLnArgs.parseSomeContractParam "newValue")
      "update value"

    updateOracleAdminSubCmd =
      mkCommandParser "update-admin"
      (parseMaybeSignArg $ UpdateOracleAdmin <$> parseAddress "newOracleAdmin")
      "update oracle admin"

    updateOracleContractSubCmd =
      mkCommandParser "update-admin"
      (parseMaybeSignArg $ UpdateOracleContract <$> parseAddress "newOracleContract")
      "update oracle admin"

    updateAdminSubCmd =
      mkCommandParser "update-admin"
      ((UpdateAdmin . Left <$> parsePublicKey "newAdmin") <|>
       (UpdateAdmin . Right . pure <$> parseSecretKey)
      )
      "update admin"

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Signed Admin contract CLI interface"
  ]

maybeSignArg :: (Typeable (ToT v), IsoValue v, SingI (ToT v), HasNoOp (ToT v), HasNoBigMap (ToT v)) =>
  MaybeT WithCounter SecretKey -> v -> IO ()
maybeSignArg signWithKey' xs =
  case runMaybeT signWithKey' of
    WithCounter _ Nothing ->
      TL.putStrLn . printLorentzValue forceSingleLine $
      xs
    WithCounter counter' (Just secretKey') ->
      TL.putStrLn . printLorentzValue forceSingleLine $
      signValue secretKey' $
      WithCounter counter' $
      xs
  where
    forceSingleLine = True

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
        printLorentzContract forceOneLine lcwDumb (Signed.signedAdminContract @(Value t))
  Init {..} ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    flip asProxyTypeOf (Proxy @Signed.Storage) $
    pure $
    Signed.Storage' admin oracleContract
  UpdateValue {..} ->
    fromSomeContractParam newValue $ \newValue' ->
      maybeSignArg signWithKey $
      Signed.UpdateValue newValue'
  UpdateOracleAdmin {..} ->
    maybeSignArg signWithKey $
    Signed.UpdateOracleAdmin @() newOracleAdmin
  UpdateOracleContract {..} ->
    maybeSignArg signWithKey $
    Signed.UpdateOracleContract @() newOracleContract
  UpdateAdmin {..} ->
    case newAdmin of
      Left newAdminPublicKey ->
        TL.putStrLn . printLorentzValue forceSingleLine $
        Signed.UpdateAdmin @() newAdminPublicKey
      Right (WithCounter counter' newAdminSecretKey) ->
        TL.putStrLn . printLorentzValue forceSingleLine $
        signValue newAdminSecretKey $
        WithCounter counter' $
        Signed.UpdateAdmin @() $
        toPublic newAdminSecretKey
  where
    forceSingleLine = True

