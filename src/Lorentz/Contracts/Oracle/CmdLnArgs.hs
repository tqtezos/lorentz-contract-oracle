{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Oracle.CmdLnArgs where

import Data.Char
import GHC.Generics
import Control.Applicative
import Control.Monad
import Text.Show (Show(..))
import Data.List
import Data.Either
import Data.Function (id, flip, const)
import Prelude (FilePath, IO, runReaderT)
import Data.String (String)
import Data.Maybe
import Data.Typeable
import Text.ParserCombinators.ReadP (ReadP)
import Text.Read
import qualified Text.ParserCombinators.ReadP as P

import Lorentz
import Michelson.Macro
import Michelson.Parser
import Michelson.TypeCheck.Instr
import Michelson.TypeCheck.TypeCheck
import Michelson.Typed.Annotation
import Michelson.Typed.Instr
import Michelson.Typed.Scope
import Michelson.Typed.Sing
import Michelson.Typed.T
import Michelson.Typed.Value
import Michelson.Typed.Haskell.Value
import qualified Michelson.Untyped.Type as U
import Util.IO
import qualified Tezos.Address as Tezos

import qualified Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Constraint
import Data.Singletons
import Text.Megaparsec (eof)

import qualified Lorentz.Contracts.Oracle as Oracle


instance IsoValue (Value' Instr t) where
  type ToT (Value' Instr t) = t
  toVal = id
  fromVal = id

-- | No `Notes`
instance SingI t => HasTypeAnn (Value' Instr t) where
  getTypeAnn = starNotes

-- | Parse something between the two given `Char`'s
betweenChars :: Char -> Char -> ReadP a -> ReadP a
betweenChars beforeChar afterChar =
  P.char beforeChar `P.between` P.char afterChar

-- | Parse something in parentheses
inParensP :: ReadP a -> ReadP a
inParensP = '(' `betweenChars` ')'

-- | Parse something in double-quotes: @"[something]"@
inQuotesP :: ReadP a -> ReadP a
inQuotesP = '"' `betweenChars` '"'

-- | Attempt to parse with given modifier, otherwise parse without
maybeLiftP :: (ReadP a -> ReadP a) -> ReadP a -> ReadP a
maybeLiftP liftP = liftM2 (<|>) liftP id

-- | Attempt to parse `inParensP`, else parse without
maybeInParensP :: ReadP a -> ReadP a
maybeInParensP = maybeLiftP inParensP

-- | Attempt to parse `inQuotesP`, else parse without
maybeInQuotesP :: ReadP a -> ReadP a
maybeInQuotesP = maybeLiftP inQuotesP

-- | Read an `Address`, inside or outside of @""@'s
readAddressP :: ReadP Address
readAddressP =
      maybeInParensP . maybeInQuotesP $ do
        ensureAddressPrefix
        addressStr <- P.munch1 isAlphaNum
        case Tezos.parseAddress $ T.pack addressStr of
          Left err -> fail $ show err
          Right address' -> return address'
  where
    ensureAddressPrefix =
      (do {('t':'z':'1':_) <- P.look; return ()}) <|>
      (do {('K':'T':'1':_) <- P.look; return ()})

instance Read Address where
  readPrec = readP_to_Prec $ const readAddressP


-- | Parse an `Address` argument, given its field name
parseAddress :: String -> Opt.Parser Address
parseAddress name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "ADDRESS"
    , Opt.help $ "Address of the " ++ name ++ "."
    ]

-- | Parse whether to output on one line
onelineOption :: Opt.Parser Bool
onelineOption = Opt.switch (
  Opt.long "oneline" <>
  Opt.help "Force single line output")

-- | Parse the output `FilePath`
outputOptions :: Opt.Parser (Maybe FilePath)
outputOptions = optional . Opt.strOption $ mconcat
  [ Opt.short 'o'
  , Opt.long "output"
  , Opt.metavar "FILEPATH"
  , Opt.help "File to use as output. If not specified, stdout is used."
  ]


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

assertNestedBigMapsAbsense :: forall (t :: T) a. SingI t => (HasNoNestedBigMaps t => a) -> a
assertNestedBigMapsAbsense f =
  case nestedBigMapsAbsense (sing @t) of
    Nothing -> error "assertNestedBigMapsAbsense"
    Just Dict -> forbiddenNestedBigMaps @t f

assertContractAbsense :: forall (t :: T) a. SingI t => (HasNoContract t => a) -> a
assertContractAbsense f =
  case contractTypeAbsense (sing @t) of
    Nothing -> error "assertContractAbsense"
    Just Dict -> forbiddenContractType @t f


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
singTypeableT STChainId = Dict
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
singIT STChainId = Dict
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

fromUntypedComparable :: U.Comparable -> CT
fromUntypedComparable (U.Comparable ct _) = ct

fromUntypedT' :: U.Type -> T
fromUntypedT' = fromUntypedT . unExplicitType

fromUntypedT :: U.T -> T
fromUntypedT (U.Tc ct) = Tc ct
fromUntypedT U.TKey = TKey
fromUntypedT U.TUnit = TUnit
fromUntypedT U.TChainId = TChainId
fromUntypedT U.TSignature = TSignature
fromUntypedT (U.TOption x) = TOption $ fromUntypedT' x
fromUntypedT (U.TList x) = TList $ fromUntypedT' x
fromUntypedT (U.TSet ct) = TSet $ fromUntypedComparable ct
fromUntypedT U.TOperation = TOperation
fromUntypedT (U.TContract x) = TContract $ fromUntypedT' x
fromUntypedT (U.TPair _ _ x y) = TPair (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TOr _ _ x y) = TOr (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TLambda x y) = TLambda (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TMap ct x) = TMap (fromUntypedComparable ct) $ fromUntypedT' x
fromUntypedT (U.TBigMap ct x) = TBigMap (fromUntypedComparable ct) $ fromUntypedT' x

-- | Parse some `T`
parseSomeT :: String -> Opt.Parser (SomeSing T)
parseSomeT name =
  (\typeStr ->
    let parsedType = parseNoEnv
          type_
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

-- | A contract parameter with some type
data SomeContractParam where
  SomeContractParam
    :: (SingI t, Typeable t)
    => Value t
    -> (Sing t, Notes t)
    -> (Dict (HasNoOp t), Dict (HasNoBigMap t))
    -> SomeContractParam

-- | Consume `SomeContractParam`
fromSomeContractParam ::
     SomeContractParam
  -> (forall t. (SingI t, Typeable t, HasNoOp t, HasNoBigMap t) =>
                  Value t -> r)
  -> r
fromSomeContractParam (SomeContractParam xs (_, _) (Dict, Dict)) f = f xs

-- | Parse and typecheck a Michelson value
parseTypeCheckValue ::
     forall t. (SingI t)
  => Parser (Value t)
parseTypeCheckValue =
  (>>= either (fail . show) return) $
  runTypeCheckIsolated . flip runReaderT def . typeCheckValue . expandValue <$>
  (value <* eof)

parseSomeContractParam :: String -> Opt.Parser SomeContractParam
parseSomeContractParam name =
  (\(SomeSing (st :: Sing t)) paramStr ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    let parsedParam = parseNoEnv
          (parseTypeCheckValue @t)
          name
          paramStr
     in let param = either (error . T.pack . show) id parsedParam
     in SomeContractParam param (st, starNotes) (Dict, Dict)
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
    assertNestedBigMapsAbsense @t $
    assertContractAbsense @t $
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine (Oracle.uncheckedOracleContract @(Value t))
  PrintTimeStamped (SomeSing (st :: Sing t)) mOutput forceOneLine ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    assertNestedBigMapsAbsense @t $
    assertContractAbsense @t $
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine (Oracle.timestampedOracleContract @(Value t))
  Init {..} ->
    fromSomeContractParam currentValue $ \currentValue' ->
      TL.putStrLn . printLorentzValue forceSingleLine $
      Oracle.Storage currentValue' admin
  GetValue {..} ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Oracle.GetValue @() $
    mkView () $ callingDefTAddress @() $ toTAddress callbackContract
  UpdateValue {..} ->
    fromSomeContractParam newValue $ \newValue' ->
      TL.putStrLn . printLorentzValue forceSingleLine $
      Oracle.UpdateValue newValue'
  UpdateAdmin {..} ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Oracle.UpdateAdmin @() newAdmin
  where
    forceSingleLine = True

