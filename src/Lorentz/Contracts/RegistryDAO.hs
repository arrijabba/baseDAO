-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | A Registry DAO can be used to store key value pair @(k, v)@
module Lorentz.Contracts.RegistryDAO
  ( registryDaoContract
  , knownRegistryDAOMetadata
  , config
  ) where

import Data.Either (fromRight)
import Fmt (pretty)
import Lorentz
import Universum ((*))
import qualified Universum as U

import qualified Lorentz.Contracts.BaseDAO as DAO
import qualified Lorentz.Contracts.BaseDAO.Proposal as DAO
import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import Lorentz.Contracts.RegistryDAO.Doc
import Lorentz.Contracts.RegistryDAO.Types
import Lorentz.Contracts.Spec.TZIP16Interface as TZIP16
import Morley.Metadata

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | To pass a proposal check, a proposal needs to have:
-- @
--   let s = size(pack(proposalMetadata))
--
--   submitTokens == ceFrozenScaleValue * s + ceFrozenExtraValue
--     &&
--   s < ceMaxProposalSize
-- @
registryDaoProposalCheck
  :: forall s k v store.
     ( NicePackedValue (RegistryDaoProposalMetadata k v)
     , IsoRegistryDaoProposalMetadata k v
     , DAO.StorageC store (RegistryDaoContractExtra k v) (RegistryDaoProposalMetadata k v)
     )
  => DAO.ProposeParams (RegistryDaoProposalMetadata k v) : store : s
  :-> Bool : s
registryDaoProposalCheck = do
  getField #ppProposalMetadata
  packRaw
  size; toNamed #proposalSize

  duupX @3
  stToField #sExtra; toFieldNamed #ceMaxProposalSize; dip dup
  if #ceMaxProposalSize >. #proposalSize then do
    fromNamed #proposalSize
    dig @2
    stackType @(store : Natural : DAO.ProposeParams (RegistryDaoProposalMetadata k v) : s)
    stToField #sExtra

    getField #ceFrozenScaleValue; dip swap; mul
    swap; toField #ceFrozenExtraValue; add

    toNamed #requireValue; swap
    toFieldNamed #ppFrozenToken; swap

    if #requireValue ==. #ppFrozenToken then do
      push True
    else do
      -- The frozen tokens from the proposal has to be exactly equal to the require frozen tokens.
      push False
  else do
    -- submitted proposal size is bigger than max proposal size
    dropN @3
    push False

-- | The slash value of a fail proposal is calculated as:
-- @
--   slashValue
--     = ceSlashScaleValue * pProposerFrozenToken / ceSlashDivisionValue
-- @
registryDaoRejectedProposalReturnValue
  :: forall s k v store.
      (IsoRegistryDaoProposalMetadata k v
      , DAO.StorageC store (RegistryDaoContractExtra k v) (RegistryDaoProposalMetadata k v)
      )
  =>  (DAO.Proposal (RegistryDaoProposalMetadata k v)) : store : s
  :-> ("slash_amount" :! Natural) : s
registryDaoRejectedProposalReturnValue = do
  toField #pProposerFrozenToken
  swap
  stToField #sExtra

  getField #ceSlashScaleValue; dip swap; mul
  swap; toField #ceSlashDivisionValue; swap; ediv
  ifSome car $
    push (0 :: Natural)
  toNamed #slash_amount

-- | When a proposal is accepted, we add/update the new registry entry
-- to the storage registry.
-- We also record the corresponding proposal key and the current time
-- of when it is applied.
decisionLambda
  :: forall k v s store.
  ( DAO.StorageC store (RegistryDaoContractExtra k v) (RegistryDaoProposalMetadata k v)
  -- shared
  , IsoRegistryDaoProposalMetadata k v
  , NicePackedValue (RegistryDaoProposalMetadata k v)
  )
  =>  DAO.Proposal (RegistryDaoProposalMetadata k v) : store : s
  :-> List Operation : store : s
decisionLambda = do
  constructT @(DAO.ProposeParams (RegistryDaoProposalMetadata k v))
    ( fieldCtor $ getField #pProposerFrozenToken
    , fieldCtor $ getField #pMetadata
    )
  dip $ getField #pProposer; pair; DAO.toProposalKey
  swap
  toField #pMetadata
  caseT
    ( #cNormalProposalType /-> do
        toField #npDiff
        iter $ do
          stackType @(RegistryUpdate k v : (DAO.ProposalKey (RegistryDaoProposalMetadata k v)) : store : s)
          constructT @(RegistryEntry k v)
            ( fieldCtor $ getField #ruNewValue
            , fieldCtor $ duupX @2
            , fieldCtor $ now
            )
          stackType @(RegistryEntry k v : RegistryUpdate k v : (DAO.ProposalKey (RegistryDaoProposalMetadata k v)) : store : s)
          dip $ dip swap
          dip $ toField #ruKey
          some; swap
          duupX @3; stToField #sExtra
          stackType @((RegistryDaoContractExtra k v) : k : Maybe (RegistryEntry k v) : store : (DAO.ProposalKey (RegistryDaoProposalMetadata k v)) : s)
          dig @2; dig @2; stUpdate #ceRegistry
          stSetField #sExtra
          swap

        drop @(DAO.ProposalKey (RegistryDaoProposalMetadata k v))
    , #cConfigProposalType /-> do
        dip drop
        duupX @2; stToField #sExtra
        duupX @2; toField #cpFrozenScaleValue; ifSome (setField #ceFrozenScaleValue) nop
        duupX @2; toField #cpFrozenExtraValue; ifSome (setField #ceFrozenExtraValue)  nop
        duupX @2; toField #cpSlashScaleValue; ifSome (setField #ceSlashScaleValue) nop
        duupX @2; toField #cpSlashDivisionValue; ifSome (setField #ceSlashDivisionValue) nop
        swap; toField #cpMaxProposalSize; ifSome (setField #ceMaxProposalSize) nop
        stSetField #sExtra
    , #cUpdateReceiversType /-> do
        stackType @(UpdateReceivers ': (DAO.ProposalKey (RegistryDaoProposalMetadata k v)) : store : _)
        dip (drop # stGetField #sExtra # getField #ceProposalReceivers)
        stackType @(UpdateReceivers ': Set Address ': RegistryDaoContractExtra k v ': store : _)

        -- Push a boolean value to indicate if we are adding addresses
        -- or removing them. True to add and False to remove
        caseT
          ( #cAddReceivers /-> push True
          , #cRemoveReceivers /-> push False
          )
        swap
        stackType @([Address] ': Bool ': Set Address ': RegistryDaoContractExtra k v ': store : _)

        -- Now we have the input addresses at the top
        -- The boolean that indicate if we are adding or removing below it
        -- and the current set of addresses, under it.
        iter $ do
          -- We put a copy of the boolean operation indicator on top of the stack
          duupX @2
          dip update
        drop

        stackType @(Set Address ': RegistryDaoContractExtra k v ': store : _)
        setField #ceProposalReceivers
        stSetField #sExtra
    )
  nil

config ::
  ( IsoRegistryDaoProposalMetadata k v
  , NicePackedValue (RegistryDaoProposalMetadata k v)
  )
  => DAO.Config (RegistryDaoContractExtra k v) (RegistryDaoProposalMetadata k v) Empty
config = DAO.defaultConfig
  { DAO.cDaoName = "Registry DAO"
  , DAO.cDaoDescription = registryDaoDoc
  , DAO.cProposalCheck = registryDaoProposalCheck
  , DAO.cRejectedProposalReturnValue = registryDaoRejectedProposalReturnValue
  , DAO.cDecisionLambda = decisionLambda

  , DAO.cMaxVotingPeriod = 60 * 60 * 24 * 30 -- 1 months
  , DAO.cMinVotingPeriod = 1 -- value between 1 second - 1 month

  , DAO.cMaxQuorumThreshold = 1000
  , DAO.cMinQuorumThreshold = 1

  , DAO.cMaxVotes = 1000
  , DAO.cMaxProposals = 500
  }

type RegistryDaoStorage k v = DAO.Storage (RegistryDaoContractExtra k v) (RegistryDaoProposalMetadata k v)

-- | Parts of metadata that can be filled just knowing the contract,
-- without user's input.
knownRegistryDAOMetadata
  :: forall k v. IsoRegistryDaoProposalMetadata k v
  => DAO.MetadataConfig
  -> Metadata (ToT (RegistryDaoStorage k v))
knownRegistryDAOMetadata cnf = let
  settings = DAO.mkMetadataSettings @(RegistryDaoContractExtra k v) @(RegistryDaoProposalMetadata k v) cnf
  baseMetadata = DAO.knownBaseDAOMetadata settings
  baseViews = fromRight [] $ getViews baseMetadata
  in baseMetadata <> (views ((registryDAOViews @k @v) <> baseViews))

registryDAOViews
  :: forall k v. IsoRegistryDaoProposalMetadata k v
  => [TZIP16.View (ToT (RegistryDaoStorage k v))]
registryDAOViews = [proposalReceiversListView @k @v]

compileViewCode_ :: ViewCode st ret -> CompiledViewCode st ret
compileViewCode_ = U.either (U.error . pretty) U.id . compileViewCode

proposalReceiversListView
  :: forall k v. IsoRegistryDaoProposalMetadata k v
  =>  TZIP16.View (ToT (RegistryDaoStorage k v))
proposalReceiversListView = TZIP16.View
  { vName = "get_proposal_receivers_list"
  , vDescription = Just "Get proposal receivers list from storage"
  , vPure = Just True
  , vImplementations =
      [  VIMichelsonStorageView $
          mkMichelsonStorageView @(RegistryDaoStorage k v) @(Set Address) Nothing [] $
            compileViewCode_ $ WithParam @() $ do
              drop
              stToField #sExtra
              stackType @(RegistryDaoContractExtra k v : _)
              toField #ceProposalReceivers
      ]
  }

registryDaoContract ::
  ( DAO.DaoC (RegistryDaoContractExtra k v) (RegistryDaoProposalMetadata k v) Empty

  -- shared
  , IsoRegistryDaoProposalMetadata k v
  ) => Contract
      (DAO.Parameter (RegistryDaoProposalMetadata k v) Empty)
      (DAO.Storage (RegistryDaoContractExtra k v) (RegistryDaoProposalMetadata k v))
registryDaoContract = DAO.baseDaoContract config
