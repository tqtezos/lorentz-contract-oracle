{-# LANGUAGE NoApplicativeDo #-} -- test_UpdateValue's TH fails with ApplicativeDo: https://gitlab.haskell.org/ghc/ghc/issues/14471

module Test.Oracle where

import Test.Hspec (Expectation)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Orphans ()

import Lorentz hiding (concat)
import Lorentz.Test

import qualified Lorentz.Contracts.Oracle as Oracle

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

