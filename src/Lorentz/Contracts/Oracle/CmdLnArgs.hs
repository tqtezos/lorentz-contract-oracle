{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields #-}

module Lorentz.Contracts.Oracle.CmdLnArgs where

import Control.Applicative
import Text.Show (Show(..))
import Data.List
import Data.Either
import Data.Function (id)
import Prelude (FilePath, IO)
import Data.String (String)
import Data.Maybe
import Data.Typeable

import Lorentz hiding (get)
import Michelson.Typed.T
import Michelson.Typed.Annotation
import Michelson.Typed.Scope
import Michelson.Typed.Sing
import qualified Michelson.Untyped.Type as U
import Michelson.Parser
import Util.IO

import qualified Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Constraint
import Data.Singletons

import Lorentz.Contracts.Util ()
import Lorentz.Contracts.SomeContractParam
import Lorentz.Contracts.Parse
import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G

import qualified Lorentz.Contracts.Oracle as Oracle

assertOpAbsense :: forall (t :: T) a. SingI t => (HasNoOp t => a) -> a
assertOpAbsense f =
  case opAbsense (sing @t) of
    Nothing -> error "assertOpAbsense"
    Just Dict -> forbiddenOp @t f

assertBigMapAbsense :: forall (t :: T) a. SingI t => (HasNoBigMap t => a) -> a
assertBigMapAbsense f =
  case bigMapAbsense (sing @t) of
    Nothing -> error "assertBigMapAbsense"
    Just Dict -> forbiddenBigMap @t f

singTypeableCT :: forall (t :: CT). Sing t -> Dict (Typeable t)
singTypeableCT SCInt = Dict
singTypeableCT SCNat = Dict
singTypeableCT SCString = Dict
singTypeableCT SCBytes = Dict
singTypeableCT SCMutez = Dict
singTypeableCT SCBool = Dict
singTypeableCT SCKeyHash = Dict
singTypeableCT SCTimestamp = Dict
singTypeableCT SCAddress = Dict

singTypeableT :: forall (t :: T). Sing t -> Dict (Typeable t)
singTypeableT (STc ct) =
  withDict (singTypeableCT ct) $
  Dict
singTypeableT STKey = Dict
singTypeableT STUnit = Dict
singTypeableT STSignature = Dict
-- singTypeableT STChainId = Dict
singTypeableT (STOption st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STList st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STSet st) =
  withDict (singTypeableCT st) $
  Dict
singTypeableT STOperation  = Dict
singTypeableT (STContract st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STPair st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STOr st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STLambda st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STBigMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict

singICT :: forall (t :: CT). Sing t -> Dict (SingI t)
singICT SCInt = Dict
singICT SCNat = Dict
singICT SCString = Dict
singICT SCBytes = Dict
singICT SCMutez = Dict
singICT SCBool = Dict
singICT SCKeyHash = Dict
singICT SCTimestamp = Dict
singICT SCAddress = Dict

singIT :: forall (t :: T). Sing t -> Dict (SingI t)
singIT (STc ct) =
  withDict (singICT ct) $
  Dict
singIT STKey = Dict
singIT STUnit = Dict
singIT STSignature = Dict
-- singIT STChainId = Dict
singIT (STOption st) =
  withDict (singIT st) $
  Dict
singIT (STList st) =
  withDict (singIT st) $
  Dict
singIT (STSet st) =
  withDict (singICT st) $
  Dict
singIT STOperation  = Dict
singIT (STContract st) =
  withDict (singIT st) $
  Dict
singIT (STPair st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STOr st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STLambda st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict
singIT (STBigMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict

data CmdLnArgs
  = Print (SomeSing T) (Maybe FilePath) Bool
  | PrintTimeStamped (SomeSing T) (Maybe FilePath) Bool
  | Init
      { currentValue :: SomeContractParam
      , admin :: Address
      }
  | GetValue
      { callbackContract :: Address
      }
  | UpdateValue
      { newValue :: SomeContractParam
      }
  | UpdateAdmin
      { newAdmin :: Address
      }


unExplicitType :: U.Type -> U.T
unExplicitType =
  \case
    U.Type t _ -> t
    U.TypeParameter -> error "U.TypeParameter"
    U.TypeStorage -> error "U.TypeStorage"

fromUntypedComparable :: U.Comparable -> CT
fromUntypedComparable (U.Comparable ct _) = ct

fromUntypedT' :: U.Type -> T
fromUntypedT' = fromUntypedT . unExplicitType

fromUntypedT :: U.T -> T
fromUntypedT (U.Tc ct) = Tc ct
fromUntypedT U.TKey = TKey
fromUntypedT U.TUnit = TUnit
fromUntypedT U.TSignature = TSignature
fromUntypedT (U.TOption _ x) = TOption $ fromUntypedT' x
fromUntypedT (U.TList x) = TList $ fromUntypedT' x
fromUntypedT (U.TSet ct) = TSet $ fromUntypedComparable ct
fromUntypedT U.TOperation = TOperation
fromUntypedT (U.TContract x) = TContract $ fromUntypedT' x
fromUntypedT (U.TPair _ _ x y) = TPair (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TOr _ _ x y) = TOr (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TLambda x y) = TLambda (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TMap ct x) = TMap (fromUntypedComparable ct) $ fromUntypedT' x
fromUntypedT (U.TBigMap ct x) = TBigMap (fromUntypedComparable ct) $ fromUntypedT' x

parseSomeT :: String -> Opt.Parser (SomeSing T)
parseSomeT name =
  (\typeStr ->
    let parsedType = parseNoEnv
          explicitType
          name
          typeStr
     in let type' = either (error . T.pack . show) unExplicitType parsedType
     in withSomeSingT (fromUntypedT type') SomeSing
  ) <$>
  Opt.strOption @Text
    (mconcat
      [ Opt.long $ name ++ "Type"
      , Opt.metavar "Michelson Type"
      , Opt.help $ "The Michelson Type of " ++ name
      ])

parseSomeContractParam :: String -> Opt.Parser SomeContractParam
parseSomeContractParam name =
  (\(SomeSing (st :: Sing t)) paramStr ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    let parsedParam = parseNoEnv
          (G.parseTypeCheckValue @t)
          name
          paramStr
     in let param = either (error . T.pack . show) id parsedParam
     in SomeContractParam param (st, NStar) (Dict, Dict)
  ) <$>
  parseSomeT name <*>
  Opt.strOption @Text
    (mconcat
      [ Opt.long name
      , Opt.metavar "Michelson Value"
      , Opt.help $ "The Michelson Value: " ++ name
      ])

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ printSubCmd
  , printTimestampedSubCmd
  , initSubCmd
  , getValueSubCmd
  , updateValueSubCmd
  , updateAdminSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    printSubCmd =
      mkCommandParser "print"
      (Print <$> parseSomeT "value" <*> outputOptions <*> onelineOption)
      "Dump the Oracle contract in form of Michelson code"

    printTimestampedSubCmd =
      mkCommandParser "print-timestamped"
      (PrintTimeStamped <$> parseSomeT "value" <*> outputOptions <*> onelineOption)
      "Dump the Timestamped Oracle contract in form of Michelson code"

    initSubCmd =
      mkCommandParser "init"
      (Init <$>
        parseSomeContractParam "initialValue" <*>
        parseAddress "admin"
      )
      "Initial storage for the Oracle contract"

    getValueSubCmd =
      mkCommandParser "get-value"
      (GetValue <$> parseAddress "callbackContract")
      "get value"

    updateValueSubCmd =
      mkCommandParser "update-value"
      (UpdateValue <$> parseSomeContractParam "newValue")
      "update value"

    updateAdminSubCmd =
      mkCommandParser "update-admin"
      (UpdateAdmin <$> parseAddress "admin")
      "update admin"

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Oracle contract CLI interface"
  ]

runCmdLnArgs :: CmdLnArgs -> IO ()
runCmdLnArgs = \case
  Print (SomeSing (st :: Sing t)) mOutput forceOneLine ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    case bigMapConstrained (STPair st (STc SCAddress)) of
      Nothing -> error "big map constraint not met for given type"
      Just Dict ->
        maybe TL.putStrLn writeFileUtf8 mOutput $
        printLorentzContract forceOneLine lcwDumb (Oracle.uncheckedOracleContract @(Value t))
  PrintTimeStamped (SomeSing (st :: Sing t)) mOutput forceOneLine ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine lcwDumb (Oracle.timestampedOracleContract @(Value t))
  Init {..} ->
    fromSomeContractParam currentValue $ \currentValue' ->
      TL.putStrLn . printLorentzValue forceSingleLine $
      Oracle.Storage currentValue' admin
  GetValue {..} ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Oracle.GetValue @() $
    View () $
    ContractAddr callbackContract
  UpdateValue {..} ->
    fromSomeContractParam newValue $ \newValue' ->
      TL.putStrLn . printLorentzValue forceSingleLine $
      Oracle.UpdateValue newValue'
  UpdateAdmin {..} ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Oracle.UpdateAdmin @() newAdmin
  where
    forceSingleLine = True

