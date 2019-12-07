{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -Wno-partial-fields -Wno-orphans -Wno-missing-export-lists #-}

module Main where

import Control.Applicative
import Control.Monad hiding (fail)
import Data.Function
-- import Data.List
-- import Data.String
import System.IO
-- import Text.Read
-- import Text.Show
-- import Text.ParserCombinators.ReadP (ReadP)
-- import qualified Text.ParserCombinators.ReadP as P
import Prelude (die, displayException, catchAny) -- const, id, maybe,

import Lorentz
-- import qualified Tezos.Address as Tezos
import Michelson.Typed.Scope
import Util.IO
import Michelson.Printer
-- import Michelson.Typed.Instr (toFullContract)
-- import Michelson.Optimizer

-- import Named
-- import Util.Named
import qualified Options.Applicative as Opt
-- import qualified Options.Applicative.Common as Opt
-- import qualified Options.Applicative.Types as Opt
-- import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.IO as TL
import Data.Singletons
import Text.PrettyPrint.ANSI.Leijen.Internal (Doc, linebreak)

-- import qualified Lorentz.Contracts.Oracle as Oracle
import qualified Lorentz.Contracts.Oracle.CmdLnArgs as OracleCmdLnArgs
-- import qualified Lorentz.Contracts.Admin.Simple as SimpleAdmin
import qualified Lorentz.Contracts.Admin.Simple.CmdLnArgs as SimpleAdminCmdLnArgs

-- import Lorentz.Contracts.Sale (Price, TokenAddr)
-- import Lorentz.Contracts.Sale (saleContract)
-- import qualified Lorentz.Contracts.Sale as Sale
-- import Lorentz.Contracts.Tunnel (tunnelContract)

-- -- | Parse something between the two given `Char`'s
-- betweenChars :: Char -> Char -> ReadP a -> ReadP a
-- betweenChars beforeChar afterChar =
--   P.char beforeChar `P.between` P.char afterChar
--
-- -- | Parse something in parentheses
-- inParensP :: ReadP a -> ReadP a
-- inParensP = '(' `betweenChars` ')'
--
-- -- | Parse something in double-quotes: @"[something]"@
-- inQuotesP :: ReadP a -> ReadP a
-- inQuotesP = '"' `betweenChars` '"'
--
-- -- | Attempt to parse with given modifier, otherwise parse without
-- maybeLiftP :: (ReadP a -> ReadP a) -> ReadP a -> ReadP a
-- maybeLiftP liftP = liftM2 (<|>) liftP id
--
-- -- | Attempt to parse `inParensP`, else parse without
-- maybeInParensP :: ReadP a -> ReadP a
-- maybeInParensP = maybeLiftP inParensP
--
-- -- | Attempt to parse `inQuotesP`, else parse without
-- maybeInQuotesP :: ReadP a -> ReadP a
-- maybeInQuotesP = maybeLiftP inQuotesP
--
-- -- | Read an `Address`, inside or outside of @""@'s
-- readAddressP :: ReadP Address
-- readAddressP =
--       maybeInParensP . maybeInQuotesP $ do
--         ensureAddressPrefix
--         addressStr <- P.munch1 isAlphaNum
--         case Tezos.parseAddress $ T.pack addressStr of
--           Left err -> fail $ show err
--           Right address' -> return address'
--   where
--     ensureAddressPrefix =
--       (do {('t':'z':'1':_) <- P.look; return ()}) <|>
--       (do {('K':'T':'1':_) <- P.look; return ()})
--
-- instance Read Address where
--   readPrec = readP_to_Prec $ const readAddressP

-- | Convert to a `Value`, untype, and render
showValue :: (IsoValue t, SingI (ToT t), HasNoOp (ToT t)) => t -> TL.Text
showValue = printTypedValue False . toVal

data CmdLnArgs
  = OracleCmdLnArgs { oracleCmdLnArgs :: OracleCmdLnArgs.CmdLnArgs }
  | SimpleAdminCmdLnArgs { simpleAdminCmdLnArgs :: SimpleAdminCmdLnArgs.CmdLnArgs }
  -- = Print (Maybe FilePath) Bool
  -- | Init
  --     { adminAddress :: !Address
  --     , heldToken :: !TokenAddr
  --     , wallet :: !Address
  --     , wantedToken :: !TokenAddr
  --     , price :: !Price
  --     }
  -- | Purchase { price :: Price }
  -- | UpdatePrice { price :: Price }
  -- | GetPrice { callback :: Address }
  -- | GetHeldToken { callback :: Address }
  -- | GetWallet { callback :: Address }
  -- | GetWantedToken { callback :: Address }

-- -- | Parse a natural number argument, given its field name
-- parseNatural :: String -> Opt.Parser Natural
-- parseNatural name =
--   Opt.option Opt.auto $
--   mconcat
--     [ Opt.long name
--     , Opt.metavar "NATURAL"
--     , Opt.help $ "Natural number representing " ++ name ++ "."
--     ]
--
-- -- | Parse an `Address` argument, given its field name
-- parseAddress :: String -> Opt.Parser Address
-- parseAddress name =
--   Opt.option Opt.auto $
--   mconcat
--     [ Opt.long name
--     , Opt.metavar "ADDRESS"
--     , Opt.help $ "Address of the " ++ name ++ "."
--     ]
--
-- parseContractAddr :: String -> Opt.Parser (ContractAddr cp)
-- parseContractAddr = fmap ContractAddr <$> parseAddress
--
-- -- | Parse a `Bool` (optional) argument, given its field name
-- parseBool :: String -> Opt.Parser Bool
-- parseBool name =
--   Opt.option Opt.auto $
--   mconcat
--     [ Opt.long name
--     , Opt.metavar "BOOL"
--     , Opt.help $
--       "Bool representing whether the contract is initially " ++ name ++ "."
--     ]
--
-- outputOptions :: Opt.Parser (Maybe FilePath)
-- outputOptions = optional . Opt.strOption $ mconcat
--   [ Opt.short 'o'
--   , Opt.long "output"
--   , Opt.metavar "FILEPATH"
--   , Opt.help "File to use as output. If not specified, stdout is used."
--   ]
--
-- onelineOption :: Opt.Parser Bool
-- onelineOption = Opt.switch (
--   Opt.long "oneline" <>
--   Opt.help "Force single line output")

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ Opt.command "Oracle" $ fmap OracleCmdLnArgs $ Opt.info OracleCmdLnArgs.argParser OracleCmdLnArgs.infoMod
  , Opt.command "SimpleAdmin" $ fmap SimpleAdminCmdLnArgs $ Opt.info SimpleAdminCmdLnArgs.argParser SimpleAdminCmdLnArgs.infoMod
  ]
  -- [ printSubCmd
  -- , initSubCmd
  -- , purchaseSubCmd
  -- , updatePriceSubCmd
  -- , getPriceSubCmd
  -- , getHeldTokenSubCmd
  -- , getWalletSubCmd
  -- , getWantedTokenSubCmd
  -- ]
  where
    -- mkCommandParser commandName parser desc =
    --   Opt.command commandName $
    --   Opt.info (Opt.helper <*> parser) $
    --   Opt.progDesc desc
    --
    -- printSubCmd =
    --   mkCommandParser "print"
    --   (Print <$> outputOptions <*> onelineOption)
    --   "Dump the sale contract in form of Michelson code"
    --
    -- initSubCmd =
    --   mkCommandParser "init"
    --   (Init <$>
    --     parseAddress "admin" <*>
    --     parseAddress "held" <*>
    --     parseAddress "wallet" <*>
    --     parseAddress "wanted" <*>
    --     parsePrice
    --   )
    --   "Initial storage for the sale contract"
    --
    -- purchaseSubCmd =
    --   mkCommandParser "purchase"
    --   (Purchase <$> parsePrice)
    --   "purchase"
    --
    -- updatePriceSubCmd =
    --   mkCommandParser "update-price"
    --   (UpdatePrice <$> parsePrice)
    --   "update price"
    --
    -- getPriceSubCmd =
    --   mkCommandParser "get-price"
    --   (GetPrice <$> parseAddress "callback")
    --   "get price"
    --
    -- getHeldTokenSubCmd =
    --   mkCommandParser "get-held"
    --   (GetHeldToken <$> parseAddress "callback")
    --   "get price"
    --
    -- getWalletSubCmd =
    --   mkCommandParser "get-wanted"
    --   (GetWantedToken <$> parseAddress "callback")
    --   "get price"
    --
    -- getWantedTokenSubCmd =
    --   mkCommandParser "get-wallet"
    --   (GetWallet <$> parseAddress "callback")
    --   "get price"

programInfo :: Opt.ParserInfo CmdLnArgs
programInfo =
  -- Opt.info (Opt.helper <*> versionOption <*> argParser) $
  Opt.info (Opt.helper <*> argParser) $
  mconcat
    [ Opt.fullDesc
    , Opt.progDesc "Sale contract parameter generation helper"
    , Opt.header "Lorentz tools"
    , Opt.footerDoc usageDoc
    ]
  -- where
  --   versionOption =
  --     Opt.infoOption
  --       ("lorentz-contract-oracle-" <> versionStr)
  --       (Opt.long "version" <> Opt.help "Show version.")
  --   versionStr = "0.1.0.0" -- showVersion version

usageDoc :: Maybe Doc
usageDoc =
  Just $
  mconcat
    [ "You can use help for specific COMMAND"
    , linebreak
    , "EXAMPLE:"
    , linebreak
    , "  lorentz-contract-sale COMMAND --help"
    , linebreak
    ]

-- lcwOptimize
--   :: (NiceParameter cp, NiceStorage st)
--   => LorentzCompilationWay cp st
-- lcwOptimize = LorentzCompilationWay (toFullContract . optimize . compileLorentzContract)

main :: IO ()
main = do
  hSetTranslit stdout
  hSetTranslit stderr

  cmdLnArgs <- Opt.execParser programInfo
  run cmdLnArgs `catchAny` (die . displayException)
  where
    -- forceSingleLine :: Bool
    -- forceSingleLine = True

    run :: CmdLnArgs -> IO ()
    run =
      \case
        OracleCmdLnArgs {..} ->
          OracleCmdLnArgs.runCmdLnArgs oracleCmdLnArgs
        SimpleAdminCmdLnArgs {..} ->
          SimpleAdminCmdLnArgs.runCmdLnArgs simpleAdminCmdLnArgs

