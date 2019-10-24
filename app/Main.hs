{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -Wno-partial-fields -Wno-orphans -Wno-missing-export-lists #-}

module Main where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Char
import Data.List
import Data.String
import System.IO
import Text.Read
import Text.Show
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as P
import Prelude (const, id, maybe, die, displayException, catchAny)

import Lorentz
import qualified Tezos.Address as Tezos
import Michelson.Typed.Scope
import Util.IO
import Michelson.Printer

import Named
import Util.Named
import qualified Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Singletons
import Text.PrettyPrint.ANSI.Leijen.Internal (Doc, linebreak)

import Lorentz.Contracts.Sale (Price, TokenAddr)
import Lorentz.Contracts.Sale (saleContract)
import qualified Lorentz.Contracts.Sale as Sale
import Lorentz.Contracts.Tunnel (tunnelContract)

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

-- | Convert to a `Value`, untype, and render
showValue :: (IsoValue t, SingI (ToT t), HasNoOp (ToT t)) => t -> TL.Text
showValue = printTypedValue False . toVal

data CmdLnArgs
  = Print (Maybe FilePath) Bool
  | PrintTunnel (Maybe FilePath) Bool
  | Init
      { adminAddress :: !Address
      , heldToken :: !TokenAddr
      , wallet :: !Address
      , wantedToken :: !TokenAddr
      , price :: !Price
      }
  | InitAllowance
      { heldToken :: !TokenAddr
      , saleAddress :: !(ContractAddr Sale.Parameter)
      }
  | InitBalance
      { wantedToken :: !TokenAddr
      , saleAddress :: !(ContractAddr Sale.Parameter)
      }
  | Purchase { price :: Price }
  | UpdatePrice { price :: Price }
  | GetPrice { callback :: Address }
  | GetHeldToken { callback :: Address }
  | GetWallet { callback :: Address }
  | GetWantedToken { callback :: Address }
  | SetTunnels { allowanceTunnel :: ContractAddr Natural, balanceTunnel :: ContractAddr Natural }


-- | Parse a natural number argument, given its field name
parseNatural :: String -> Opt.Parser Natural
parseNatural name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "NATURAL"
    , Opt.help $ "Natural number representing " ++ name ++ "."
    ]

-- | Parse an `Address` argument, given its field name
parseAddress :: String -> Opt.Parser Address
parseAddress name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "ADDRESS"
    , Opt.help $ "Address of the " ++ name ++ "."
    ]

parseContractAddr :: String -> Opt.Parser (ContractAddr cp)
parseContractAddr = fmap ContractAddr <$> parseAddress

-- | Parse a `Bool` (optional) argument, given its field name
parseBool :: String -> Opt.Parser Bool
parseBool name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "BOOL"
    , Opt.help $
      "Bool representing whether the contract is initially " ++ name ++ "."
    ]

outputOptions :: Opt.Parser (Maybe FilePath)
outputOptions = optional . Opt.strOption $ mconcat
  [ Opt.short 'o'
  , Opt.long "output"
  , Opt.metavar "FILEPATH"
  , Opt.help "File to use as output. If not specified, stdout is used."
  ]

onelineOption :: Opt.Parser Bool
onelineOption = Opt.switch (
  Opt.long "oneline" <>
  Opt.help "Force single line output")

parsePrice :: Opt.Parser Price
parsePrice =
  (\x y -> (Name .! x, Name .! y)) <$>
  parseNatural "held-price" <*>
  parseNatural "wanted-price"

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.subparser $ mconcat
  [ printSubCmd
  , printTunnelSubCmd
  , initSubCmd
  , initAllowanceSubCmd
  , initBalanceSubCmd
  , purchaseSubCmd
  , updatePriceSubCmd
  , getPriceSubCmd
  , getHeldTokenSubCmd
  , getWalletSubCmd
  , getWantedTokenSubCmd
  , setTunnelsSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    printSubCmd =
      mkCommandParser "print"
      (Print <$> outputOptions <*> onelineOption)
      "Dump the sale contract in form of Michelson code"

    printTunnelSubCmd =
      mkCommandParser "print-tunnel"
      (PrintTunnel <$> outputOptions <*> onelineOption)
      "Dump the tunnel contract in form of Michelson code"

    initSubCmd =
      mkCommandParser "init"
      (Init <$>
        parseAddress "admin" <*>
        parseContractAddr "held" <*>
        parseAddress "wallet" <*>
        parseContractAddr "wanted" <*>
        parsePrice
      )
      "Initial storage for the sale contract"

    initAllowanceSubCmd =
      mkCommandParser "init-allowance"
      (InitAllowance <$>
        parseContractAddr "held" <*>
        parseContractAddr "sale"
      )
      "Initial storage for the allowance tunnel contract"

    initBalanceSubCmd =
      mkCommandParser "init-balance"
      (InitBalance <$>
        parseContractAddr "wanted" <*>
        parseContractAddr "sale"
      )
      "Initial storage for the balance tunnel contract"

    purchaseSubCmd =
      mkCommandParser "purchase"
      (Purchase <$> parsePrice)
      "purchase"

    updatePriceSubCmd =
      mkCommandParser "update-price"
      (UpdatePrice <$> parsePrice)
      "update price"

    getPriceSubCmd =
      mkCommandParser "get-price"
      (GetPrice <$> parseAddress "callback")
      "get price"

    getHeldTokenSubCmd =
      mkCommandParser "get-held"
      (GetHeldToken <$> parseAddress "callback")
      "get price"

    getWalletSubCmd =
      mkCommandParser "get-wanted"
      (GetWantedToken <$> parseAddress "callback")
      "get price"

    getWantedTokenSubCmd =
      mkCommandParser "get-wallet"
      (GetWallet <$> parseAddress "callback")
      "get price"

    setTunnelsSubCmd =
      mkCommandParser "set-tunnels"
      (SetTunnels <$>
        parseContractAddr "held" <*>
        parseContractAddr "wanted")
      "set proxies"

programInfo :: Opt.ParserInfo CmdLnArgs
programInfo =
  Opt.info (Opt.helper <*> versionOption <*> argParser) $
  mconcat
    [ Opt.fullDesc
    , Opt.progDesc "Sale contract parameter generation helper"
    , Opt.header "Lorentz tools"
    , Opt.footerDoc usageDoc
    ]
  where
    versionOption =
      Opt.infoOption
        ("lorentz-contract-sale-" <> versionStr)
        (Opt.long "version" <> Opt.help "Show version.")
    versionStr = "0.1.0.0" -- showVersion version

usageDoc :: Maybe Doc
usageDoc =
  Just $
  mconcat
    [ "You can use help for specific COMMAND"
    , linebreak
    , "EXAMPLE:"
    , linebreak
    , "  lorentz-contract-sale ManagedLedger --help"
    , linebreak
    ]

main :: IO ()
main = do
  hSetTranslit stdout
  hSetTranslit stderr

  cmdLnArgs <- Opt.execParser programInfo
  run cmdLnArgs `catchAny` (die . displayException)
  where
    forceSingleLine :: Bool
    forceSingleLine = True

    run :: CmdLnArgs -> IO ()
    run =
      \case
        Print mOutput forceOneLine ->
          maybe TL.putStrLn writeFileUtf8 mOutput $
          printLorentzContract forceOneLine lcwDumb saleContract
        PrintTunnel mOutput forceOneLine ->
          maybe TL.putStrLn writeFileUtf8 mOutput $
          printLorentzContract forceOneLine lcwDumb $
          tunnelContract @Natural @Sale.Parameter
        Init {..} ->
          TL.putStrLn . printLorentzValue forceSingleLine $
          Sale.initialStorage adminAddress heldToken wallet wantedToken price
        InitAllowance {..} ->
          TL.putStrLn . printLorentzValue forceSingleLine $
          Sale.allowanceTunnelStorage heldToken saleAddress
        InitBalance {..} ->
          TL.putStrLn . printLorentzValue forceSingleLine $
          Sale.balanceTunnelStorage wantedToken saleAddress
        Purchase {..} ->
          TL.putStrLn . printLorentzValue forceSingleLine $
          Sale.Purchase price
        UpdatePrice {..} ->
          TL.putStrLn . printLorentzValue forceSingleLine $
          Sale.UpdatePrice price
        GetPrice {..} ->
          TL.putStrLn . printLorentzValue forceSingleLine $
          Sale.GetPrice $
          View () $
          ContractAddr callback
        GetHeldToken {..} ->
          TL.putStrLn . printLorentzValue forceSingleLine $
          Sale.GetHeldToken $
          View () $
          ContractAddr callback
        GetWallet {..} ->
          TL.putStrLn . printLorentzValue forceSingleLine $
          Sale.GetWallet $
          View () $
          ContractAddr callback
        GetWantedToken {..} ->
          TL.putStrLn . printLorentzValue forceSingleLine $
          Sale.GetWantedToken $
          View () $
          ContractAddr callback
        SetTunnels {..} ->
          TL.putStrLn . printLorentzValue forceSingleLine $
          Sale.SetTunnels $
          (Name .! allowanceTunnel, Name .! balanceTunnel)

