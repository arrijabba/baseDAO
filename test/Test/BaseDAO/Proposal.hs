-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.BaseDAO.Proposal
  ( test_BaseDAO_Proposal
  ) where

import Universum hiding ((>>), drop, compare)

import Lorentz
import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Tezos.Crypto (blake2b)

import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import Test.Common
import Test.BaseDAO.ProposalConfig

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_Proposal :: TestTree
test_BaseDAO_Proposal = testGroup "BaseDAO propose/vote entrypoints tests:"
  [ testGroup "Proposal creator:"
      [ testCase "can propose a valid proposal" $
          nettestTestExpectation validProposal
      , testCase "cannot propose an invalid proposal (rejected)" $
          nettestTestExpectation rejectProposal
      , testCase "cannot propose with insufficient tokens" $
          nettestTestExpectation insufficientTokenProposal
      , testCase "cannot propose a non-unique proposal" $
          nettestTestExpectation nonUniqueProposal
      ]
  , testGroup "Voter:"
      [ testCase "can vote on a valid proposal" $
          nettestTestExpectation voteValidProposal
      , testCase "cannot vote non-existing proposal" $
          nettestTestExpectation voteNonExistingProposal
      , testCase "can vote on multiple proposals" $
          nettestTestExpectation voteMultiProposals
      , testCase "cannot vote if the vote amounts exceeds token balance" $
          nettestTestExpectation insufficientTokenVote
      -- TODO [#30]: Can be tested until delay time is implemented for nettest
      -- , testCase "cannot vote on outdated proposal" $
      --     nettestTestExpectation
      ]
  , testGroup "Admin:"
      [ testCase "can set voting period" $
          nettestTestExpectation setVotingPeriod
      , testCase "can set quorum threshold" $
          nettestTestExpectation setQuorumThreshold
      , testCase "can flush proposals that got accepted" $
          nettestTestExpectation flushAcceptedProposals
      , testCase "can flush proposals that got rejected due to not meeting quorum_threshold" $
          nettestTestExpectation flushRejectProposalQuorum
      , testCase "can flush proposals that got rejected due to negative votes" $
          nettestTestExpectation flushRejectProposalNegativeVotes
      , testCase "flush should not affecting ongoing proposals" $
          nettestTestExpectation flushNotAffectOngoingProposals
      , testCase "flush with bad 'cRejectedProposalReturnValue'" $
          nettestTestExpectation flushWithBadConfig
      -- TODO [#15]: admin burn proposer token and test "flush"

      , testCase "flush and run decision lambda" $
          nettestTestExpectation flushDecisionLambda
      ]
  , testGroup "Other:"
      [ testCase "can call proposalMetadata" $
          nettestTestExpectation proposalMetadata
      ]
  , testGroup "Bounded Value"
      [ testCase "bounded value on proposals" $
          nettestTestExpectation proposalBoundedValue
      , testCase "bounded value on votes" $
          nettestTestExpectation votesBoundedValue
      , testCase "bounded range on quorum_threshold" $
          nettestTestExpectation quorumThresholdBound
      , testCase "bounded range on voting_period" $
          nettestTestExpectation votingPeriodBound
      ]
  ]

validProposal :: (Monad m) => NettestImpl m -> m ()
validProposal = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig config
  let params = DAO.ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = 1
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
  checkTokenBalance (DAO.frozenTokenId) dao owner1 10
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 90
  -- TODO [#31]: Currently proposalId is expected to be knowned (checkInStorage)

  checkIfAProposalExist (makeProposalKey params owner1) dao

rejectProposal :: (Monad m) => NettestImpl m -> m ()
rejectProposal = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig config
  let params = DAO.ProposeParams
        { ppFrozenToken = 9
        , ppProposalMetadata = 1
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
    & expectCustomError_ #fAIL_PROPOSAL_CHECK

insufficientTokenProposal :: (Monad m) => NettestImpl m -> m ()
insufficientTokenProposal = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig config
  let params = DAO.ProposeParams
        { ppFrozenToken = 101
        , ppProposalMetadata = 1
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
    & expectCustomError_ #pROPOSAL_INSUFFICIENT_BALANCE

nonUniqueProposal :: (Monad m) => NettestImpl m -> m ()
nonUniqueProposal = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig config
  _ <- createSampleProposal 1 owner1 dao
  createSampleProposal 1 owner1 dao
    & expectCustomError_ #pROPOSAL_NOT_UNIQUE

voteValidProposal :: (Monad m) => NettestImpl m -> m ()
voteValidProposal = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDaoWithConfig config

  -- | Create sample proposal (first proposal has id = 0)
  key1 <- createSampleProposal 1 owner1 dao
  let params = DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = (key1 :: ByteString)
        }

  callFrom (AddressResolved owner2) dao (Call @"Vote") [params]
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 98
  checkTokenBalance (DAO.frozenTokenId) dao owner2 2
  -- TODO [#31]: check if the vote is updated properly

voteNonExistingProposal :: (Monad m) => NettestImpl m -> m ()
voteNonExistingProposal = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDaoWithConfig config

  -- | Create sample proposal
  _ <- createSampleProposal 1 owner1 dao
  let params = DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = ("\11\12\13" :: ByteString)
        }

  callFrom (AddressResolved owner2) dao (Call @"Vote") [params]
    & expectCustomError_ #pROPOSAL_NOT_EXIST

voteMultiProposals :: (Monad m) => NettestImpl m -> m ()
voteMultiProposals = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDaoWithConfig config

  -- | Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao
  key2 <- createSampleProposal 2 owner1 dao
  let params =
        [ DAO.VoteParam
            { vVoteType = True
            , vVoteAmount = 2
            , vProposalKey = (key1 :: ByteString)
            }
        , DAO.VoteParam
            { vVoteType = False
            , vVoteAmount = 3
            , vProposalKey = (key2 :: ByteString)
            }
        ]

  callFrom (AddressResolved owner2) dao (Call @"Vote") params
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 95
  checkTokenBalance (DAO.frozenTokenId) dao owner2 5
  -- TODO [#31]: check storage if the vote update the proposal properly

insufficientTokenVote :: (Monad m) => NettestImpl m -> m ()
insufficientTokenVote = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDaoWithConfig config

  -- | Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao
  let params =
        [ DAO.VoteParam
            { vVoteType = True
            , vVoteAmount = 51
            , vProposalKey = (key1 :: ByteString)
            }
        , DAO.VoteParam
            { vVoteType = False
            , vVoteAmount = 50
            , vProposalKey = (key1 :: ByteString)
            }
        ]

  callFrom (AddressResolved owner2) dao (Call @"Vote") params
    & expectCustomError_ #vOTING_INSUFFICIENT_BALANCE

setVotingPeriod :: (Monad m) => NettestImpl m -> m ()
setVotingPeriod = uncapsNettest $ do
  ((owner1, _), _, dao, admin) <- originateBaseDaoWithConfig config

  let param = 60 * 60 -- 1 hour

  callFrom (AddressResolved owner1) dao (Call @"Set_voting_period") param
    & expectCustomError_ #nOT_ADMINISTRATOR

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") param
  -- TODO [#31]: checkStorage

setQuorumThreshold :: (Monad m) => NettestImpl m -> m ()
setQuorumThreshold = uncapsNettest $ do
  ((owner1, _), _, dao, admin) <- originateBaseDaoWithConfig config

  let param = 100

  callFrom (AddressResolved owner1) dao (Call @"Set_quorum_threshold") param
    & expectCustomError_ #nOT_ADMINISTRATOR

  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") param
  -- TODO [#31]: checkStorage

proposalMetadata :: (Monad m) => NettestImpl m -> m ()
proposalMetadata = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig config

  key1 <- createSampleProposal 1 owner1 dao

  checkIfAProposalExist (key1 :: ByteString) dao

flushNotAffectOngoingProposals :: (Monad m) => NettestImpl m -> m ()
flushNotAffectOngoingProposals = uncapsNettest $ do
  ((owner1, _), _, dao, admin) <- originateBaseDaoWithConfig config

  callFrom (AddressResolved owner1) dao (Call @"Flush") ()
    & expectCustomError_ #nOT_ADMINISTRATOR

  key1 <- createSampleProposal 1 owner1 dao
  key2 <- createSampleProposal 2 owner1 dao
  callFrom (AddressResolved admin) dao (Call @"Flush") ()
  checkIfAProposalExist (key1 :: ByteString) dao
  checkIfAProposalExist (key2 :: ByteString) dao

flushAcceptedProposals :: (Monad m) => NettestImpl m -> m ()
flushAcceptedProposals = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDaoWithConfig (config {DAO.cForceVotingPeriod = True})

  -- | Accepted Proposals
  key1 <- createSampleProposal 1 owner1 dao
  checkTokenBalance (DAO.frozenTokenId) dao owner1 10

  -- TODO [#30]: Currently this is useless since we force voting period by config
  -- This is due to Nettest not supporting time delay yet, so we cannot `wait`
  -- until the voting period is over.
  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 1
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 1

  let upvote = DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = (key1 :: ByteString)
        }
      downvote = DAO.VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = (key1 :: ByteString)
        }
  callFrom (AddressResolved owner2) dao (Call @"Vote") [upvote, downvote]
  checkTokenBalance (DAO.frozenTokenId) dao owner2 3
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 97

  callFrom (AddressResolved admin) dao (Call @"Flush") ()

  checkIfAProposalExist (key1 :: ByteString) dao
    & expectCustomError_ #pROPOSAL_NOT_EXIST

  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 100 -- proposer

  checkTokenBalance (DAO.frozenTokenId) dao owner2 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 100 -- voter

flushRejectProposalQuorum :: (Monad m) => NettestImpl m -> m ()
flushRejectProposalQuorum =
  uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDaoWithConfig (config {DAO.cForceVotingPeriod = True})

  -- | Rejected Proposal
  key1 <- createSampleProposal 1 owner1 dao
  checkTokenBalance (DAO.frozenTokenId) dao owner1 10

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 1
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 3

  let votes =
        [ DAO.VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = (key1 :: ByteString)
          }
        , DAO.VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = (key1 :: ByteString)
          }
        ]
  callFrom (AddressResolved owner2) dao (Call @"Vote") votes

  callFrom (AddressResolved admin) dao (Call @"Flush") ()

  checkIfAProposalExist (key1 :: ByteString) dao
    & expectCustomError_ #pROPOSAL_NOT_EXIST

  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 95 -- proposer: cRejectedValue reduce frozen token by half
  checkTokenBalance (DAO.frozenTokenId) dao owner2 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 100 -- voter

flushRejectProposalNegativeVotes :: (Monad m) => NettestImpl m -> m ()
flushRejectProposalNegativeVotes = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDaoWithConfig (config {DAO.cForceVotingPeriod = True})

  -- | Rejected Proposal
  key1 <- createSampleProposal 1 owner1 dao
  checkTokenBalance (DAO.frozenTokenId) dao owner1 10

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 1
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 3

  let votes =
        [ DAO.VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = (key1 :: ByteString)
          }
        , DAO.VoteParam
          { vVoteType = False
          , vVoteAmount = 1
          , vProposalKey = (key1 :: ByteString)
          }
        , DAO.VoteParam
          { vVoteType = False
          , vVoteAmount = 1
          , vProposalKey = (key1 :: ByteString)
          }
        ]
  callFrom (AddressResolved owner2) dao (Call @"Vote") votes

  callFrom (AddressResolved admin) dao (Call @"Flush") ()

  checkIfAProposalExist (key1 :: ByteString) dao
    & expectCustomError_ #pROPOSAL_NOT_EXIST

  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 95 -- proposer: cRejectedValue reduce frozen token by half
  checkTokenBalance (DAO.frozenTokenId) dao owner2 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 100 -- voter

flushWithBadConfig :: (Monad m) => NettestImpl m -> m ()
flushWithBadConfig = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDaoWithConfig
                                              (badRejectedValueConfig {DAO.cForceVotingPeriod = True})

  key1 <- createSampleProposal 1 owner1 dao
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 90

  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 2
  let upvote = DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = (key1 :: ByteString)
        }
  callFrom (AddressResolved owner2) dao (Call @"Vote") [upvote]

  callFrom (AddressResolved admin) dao (Call @"Flush") ()

  checkIfAProposalExist (key1 :: ByteString) dao
    & expectCustomError_ #pROPOSAL_NOT_EXIST

  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 90 -- slash all frozen values
  checkTokenBalance (DAO.frozenTokenId) dao owner2 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 100

flushDecisionLambda :: (Monad m) => NettestImpl m -> m ()
flushDecisionLambda = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDaoWithConfig
                                              (decisionLambdaConfig {DAO.cForceVotingPeriod = True})

  key1 <- createSampleProposal 1 owner1 dao

  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 1
  let upvote = DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = (key1 :: ByteString)
        }
  callFrom (AddressResolved owner2) dao (Call @"Vote") [upvote]

  callFrom (AddressResolved admin) dao (Call @"Flush") ()

  -- | Credit the proposer 10 tokens when the proposal is accepted
  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 110

proposalBoundedValue :: (Monad m) => NettestImpl m -> m ()
proposalBoundedValue = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig (config { DAO.cMaxProposals = 1})

  let params = DAO.ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = 1
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
  callFrom (AddressResolved owner1) dao (Call @"Propose") params
    & expectCustomError_ #mAX_PROPOSALS_REACHED

votesBoundedValue :: (Monad m) => NettestImpl m -> m ()
votesBoundedValue = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDaoWithConfig (config { DAO.cMaxVotes = 1})

  key1 <- createSampleProposal 1 owner2 dao
  let upvote = DAO.VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = (key1 :: ByteString)
        }
      downvote= DAO.VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = (key1 :: ByteString)
        }

  callFrom (AddressResolved owner1) dao (Call @"Vote") [downvote]
  callFrom (AddressResolved owner1) dao (Call @"Vote") [upvote]
    & expectCustomError_ #mAX_VOTES_REACHED

quorumThresholdBound :: (Monad m) => NettestImpl m -> m ()
quorumThresholdBound = uncapsNettest $ do
  (_, _, dao, admin) <- originateBaseDaoWithConfig
                                          (config { DAO.cMinQuorumThreshold = 1
                                                  , DAO.cMaxQuorumThreshold = 2
                                                  })
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 1
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 2
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 0
    & expectCustomError_ #oUT_OF_BOUND_QUORUM_THRESHOLD
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 3
    & expectCustomError_ #oUT_OF_BOUND_QUORUM_THRESHOLD

votingPeriodBound :: (Monad m) => NettestImpl m -> m ()
votingPeriodBound = uncapsNettest $ do
  (_, _, dao, admin) <- originateBaseDaoWithConfig
                                          (config { DAO.cMinVotingPeriod = 1
                                                  , DAO.cMaxVotingPeriod = 2
                                                  })
  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 1
  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 2
  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 0
    & expectCustomError_ #oUT_OF_BOUND_VOTING_PERIOD
  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 3
    & expectCustomError_ #oUT_OF_BOUND_VOTING_PERIOD

-------------------------------------------------------------------------------
-- Helper
-------------------------------------------------------------------------------

-- TODO [#30]: see this MR: https://gitlab.com/morley-framework/morley/-/issues/351
-- delayInSeconds :: MonadNettest caps base m => Natural -> m ()
-- delayInSeconds _ = do
--   pure ()

createSampleProposal
  :: MonadNettest caps base m
  => Integer -> Address -> TAddress (DAO.Parameter TestProposalMetadata) -> m ByteString
createSampleProposal counter owner1 dao = do
  let params = DAO.ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = counter
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
  pure $ (makeProposalKey params owner1)

checkIfAProposalExist
  :: MonadNettest caps base m
  => DAO.ProposalKey -> TAddress (DAO.Parameter TestProposalMetadata) -> m ()
checkIfAProposalExist proposalKey dao = do
  owner :: Address <- newAddress "owner"
  consumer <- originateSimple "consumer" [] contractConsumer
  -- | If the proposal exists, there should be no error
  callFrom (AddressResolved owner) dao (Call @"Proposal_metadata") (mkView proposalKey consumer)

makeProposalKey :: DAO.ProposeParams Integer -> Address -> ByteString
makeProposalKey params owner = blake2b $ lPackValue (params, owner)

-- TODO [#31]: See this ISSUES: https://gitlab.com/morley-framework/morley/-/issues/415#note_435327096
-- Check if certain field in storage
-- checkPropertyOfProposal :: _
-- checkPropertyOfProposal = error "undefined"
