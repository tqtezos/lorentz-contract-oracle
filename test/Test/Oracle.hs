{-# LANGUAGE NoApplicativeDo #-} -- test_UpdateValue's TH fails with ApplicativeDo: https://gitlab.haskell.org/ghc/ghc/issues/14471

{-# OPTIONS -Wno-missing-export-lists #-}

module Test.Oracle where

import Data.Typeable

import Test.Hspec (Expectation)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Orphans ()
import Text.Megaparsec (parse)
import qualified Data.Text.Lazy as TL
import qualified Options.Applicative as Opt

import Michelson.Parser
import Michelson.TypeCheck
import qualified Michelson.TypeCheck as TypeCheck
import Michelson.Typed.Annotation
import Michelson.Typed.Instr (FullContract(..))
import Michelson.Typed.EntryPoints
import Michelson.Macro
import Michelson.Typed.Value

import Lorentz hiding (concat)
import Lorentz.Test
import Util.IO

import qualified Lorentz.Contracts.Oracle as Oracle
import qualified Lorentz.Contracts.Oracle.CmdLnArgs as CmdLnArgs


testCmdLnParser :: Opt.Parser TL.Text
testCmdLnParser =
  CmdLnArgs.runCmdLnArgs (const id) <$>
  CmdLnArgs.argParser

assertCmdLnParser :: TL.Text -> [String] -> TestTree
assertCmdLnParser expectedOut args = testCase ("assertCmdLnParser: " ++ show args) $ do
  result' <- Opt.handleParseResult $ Opt.execParserPure (Opt.prefs mempty) (Opt.info testCmdLnParser Opt.fullDesc) args
  assertEqual "parse results" expectedOut result'

test_runCmdLnArgs :: TestTree
test_runCmdLnArgs = testGroup "Parse and run CmdLnArgs"
  [ assertCmdLnParser (printLorentzContract dontForceOneLine (Oracle.uncheckedOracleContract @(Value (ToT Natural)))) ["print", "--valueType", "nat"]
  , assertCmdLnParser "Pair 3 \"tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr\"" ["init", "--initialValueType", "nat", "--initialValue", "3", "--admin", "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr"]
  , assertCmdLnParser "Left (Pair Unit \"KT1ShW17HERZgjUTxSxSc4W3tuWrfLCqBmEi\")" ["get-value", "--callbackContract", "KT1ShW17HERZgjUTxSxSc4W3tuWrfLCqBmEi"]
  , assertCmdLnParser "Right (Left 42)" ["update-value", "--newValueType", "nat", "--newValue", "42"]
  ]
  where
    dontForceOneLine = False


expectTypedContract :: forall cp st m. (NiceParameterFull cp, NiceStorage st, MonadFail m) => TypeCheck.SomeContract -> m (ContractCode cp st)
expectTypedContract =
  \case
    TypeCheck.SomeContract (FullContract fcCode' (_ :: ParamNotes cp') (_ :: Notes st')) ->
      case (eqT @(ToT cp) @cp', eqT @(ToT st) @st') of
        (Just Refl, Just Refl) -> return $ LorentzInstr $ RfNormal fcCode'
        eqs -> fail $ mconcat ["expectTypedContract: ", show eqs]

parseTypedContract :: forall cp st m. (NiceParameterFull cp, NiceStorage st, MonadFail m) => String -> Text -> m (ContractCode cp st)
parseTypedContract name =
  either
  (fail . ("parseTypedContract: "++) . show)
  ( either (fail . ("parseTypedContract: "++) . show) (expectTypedContract @cp @st) . typeCheckContract mempty . expandContract) .
  parse program name

assertCompiledContract :: forall cp st. (NiceParameterFull cp, NiceStorage st) => FilePath -> ContractCode cp st -> TestTree
assertCompiledContract contractPath contractCode = testCase ("assertCompiledContract: " ++ contractPath) $ do
  actualContractSrc <- readFileUtf8 contractPath >>= parseTypedContract @cp @st contractPath
  assertEqual "contracts" actualContractSrc (Lorentz.cast Lorentz.>> contractCode)

test_CompileContract :: TestTree
test_CompileContract = testGroup "Compile Contract" $
  [ assertCompiledContract "unchecked_nat_oracle.tz" $ Oracle.uncheckedOracleContract @Natural
  , assertCompiledContract "nat_oracle.tz" $ Oracle.timestampedOracleContract @Natural
  ]


withOracleContract :: forall a. (HasTypeAnn a, NiceParameterFull a, NiceStorage a)
  => a
  -> Address
  -> (TAddress (Oracle.Parameter a) -> IntegrationalScenario)
  -> Expectation
withOracleContract currentValue' admin' callback =
  integrationalTestExpectation $ do
    oracleContract' <-
      lOriginate
        (Oracle.uncheckedOracleContract @a)
        "Oracle Contract"
        (Oracle.Storage
           currentValue'
           admin')
        (toMutez 0)
    callback oracleContract'

test_UpdateValue :: TestTree
test_UpdateValue = testGroup "UpdateValue" $ concat $
  $(listE ((\(storedValue, newValue) -> [|
    [ testCase ("Update value as non-admin:" ++ show ($(storedValue), $(newValue))) $ do
        withOracleContract
          $(storedValue)
          genesisAddress1 $ \oracleContract' -> do
            lCallDef oracleContract' $ Oracle.UpdateValue $(newValue)
            consumer <- lOriginateEmpty contractConsumer "consumer"
            lCallDef oracleContract' $ Oracle.GetValue (mkView () consumer)
            validate . Left $
              lExpectMichelsonFailed (const True) oracleContract'
    , testCase ("Update value as admin:" ++ show ($(storedValue), $(newValue))) $ do
        withOracleContract
          $(storedValue)
          genesisAddress $ \oracleContract' -> do
            lCallDef oracleContract' $ Oracle.UpdateValue $(newValue)
            consumer <- lOriginateEmpty contractConsumer "consumer"
            lCallDef oracleContract' $ Oracle.GetValue (mkView () consumer)
            validate . Right $
              lExpectViewConsumerStorage consumer [$(newValue)]
    ] |]) <$>
    [ ( [e|0::Natural|], [e|1::Natural|] )
    , ( [e|4::Natural|], [e|4::Natural|] )
    , ( [e|0::Integer|], [e|42::Integer|] )
    , ( [e|False|], [e|True|] )
    , ( [e|[mt|hi|]|], [e|[mt|hello world|]|] )
    , ( [e|[]::[Natural]|], [e|[1,2,3]::[Natural]|] )
    ]
    ))

test_GetValue :: TestTree
test_GetValue = testGroup "GetValue" $ concat $
  $(listE ((\(storedValue) -> [|
    [ testCase ("Get value as non-admin: " ++ show $(storedValue)) $ do
        withOracleContract
          $(storedValue)
          genesisAddress1 $ \oracleContract' -> do
            consumer <- lOriginateEmpty contractConsumer "consumer"
            lCallDef oracleContract' $ Oracle.GetValue (mkView () consumer)
            validate . Right $
              lExpectViewConsumerStorage consumer [$(storedValue)]
    , testCase ("Get value as admin: " ++ show $(storedValue)) $ do
        withOracleContract
          $(storedValue)
          genesisAddress $ \oracleContract' -> do
            consumer <- lOriginateEmpty contractConsumer "consumer"
            lCallDef oracleContract' $ Oracle.GetValue (mkView () consumer)
            validate . Right $
              lExpectViewConsumerStorage consumer [$(storedValue)]
    ] |]) <$>
    [ [e|0::Natural|]
    , [e|4::Natural|]
    , [e|42::Integer|]
    , [e|True|]
    , [e|[mt|hello world|]|]
    , [e|[1,2,3]::[Natural]|]
    ]
    ))

test_UpdateAdmin :: TestTree
test_UpdateAdmin = testGroup "UpdateAdmin"
  [ testCase "Update admin as non-admin" $ do
      withOracleContract
        True
        genesisAddress1 $ \oracleContract' -> do
          lCall oracleContract' $ Oracle.UpdateAdmin genesisAddress2
          validate . Left $
            lExpectMichelsonFailed (const True) oracleContract'
  , testCase "Update admin as admin" $ do
      withOracleContract
        True
        genesisAddress $ \oracleContract' -> do
          lCall oracleContract' $ Oracle.UpdateAdmin genesisAddress2
          validate . Right $ lExpectStorageUpdate @(Oracle.Storage Bool) oracleContract' $ \st ->
            if Oracle.admin st == genesisAddress2
               then return ()
               else Left $ CustomValidationError "Admin was not updated"
  ]

