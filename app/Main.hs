module Main
  ( main
  ) where

import Universum

import qualified Data.Map as Map
import Data.Version (showVersion)
import Main.Utf8 (withUtf8)
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)
import Paths_baseDAO (version)
import Lorentz (DGitRevision, GitRepoSettings(..), mkDGitRevision)
import Lorentz.ContractRegistry

import qualified Basic

programInfo :: DGitRevision -> Opt.ParserInfo CmdLnArgs
programInfo gitRev = Opt.info (Opt.helper <*> versionOption <*> argParser contracts gitRev) $
  mconcat
  [ Opt.fullDesc
  , Opt.progDesc "BaseDAO contracts registry"
  , Opt.header "BaseDAO contracts for Michelson"
  , Opt.footerDoc usageDoc
  ]
  where
    versionOption = Opt.infoOption ("baseDAO-" <> showVersion version)
      (Opt.long "version" <> Opt.help "Show version.")

repoSettings :: GitRepoSettings
repoSettings = GitRepoSettings $ \commit ->
  "https://github.com/tqtezos/baseDAO/tree/" <> commit

usageDoc :: Maybe Doc
usageDoc = Just $ mconcat
   [ "You can use help for specific COMMAND", linebreak
   , "EXAMPLE:", linebreak
   , "  baseDAO print --help", linebreak
   ]

contracts :: ContractRegistry
contracts = ContractRegistry $ Map.fromList
  [ "Basic" ?:: ContractInfo
    { ciContract = Basic.measureBoaConstrictor
    , ciIsDocumented = True
    , ciStorageParser = Just (pure Basic.emptyStorage)
    , ciStorageNotes = Nothing
    }
  ]

main :: IO ()
main = withUtf8 $ do
  let gitRev = $(mkDGitRevision) repoSettings
  cmdLnArgs <- Opt.execParser (programInfo gitRev)
  runContractRegistry contracts cmdLnArgs `catchAny` (die . displayException)
