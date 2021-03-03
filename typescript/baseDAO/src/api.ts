// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

import { ContractMethod , Signer, TransactionWalletOperation,
         Contract, ContractAbstraction,
         ContractProvider, TezosToolkit, MichelsonMap
       } from '@taquito/taquito';
import { InMemorySigner, importKey } from '@taquito/signer';

import { Parameter } from './generated/Parameter';
import { Startup } from './generated/Startup';
import { CallCustom } from './generated/CallCustom';
import { Accept_ownership } from './generated/Accept_ownership';
import { Burn } from './generated/Burn';
import { Balance_of } from './generated/Balance_of';
import { Confirm_migration } from './generated/Confirm_migration';
import { Drop_proposal } from './generated/Drop_proposal';
import { Flush } from './generated/Flush';
import { Get_total_supply } from './generated/Get_total_supply';
import { GetVotePermitCounter } from './generated/GetVotePermitCounter';
import { Migrate } from './generated/Migrate';
import { Mint } from './generated/Mint';
import { Propose } from './generated/Propose';
import { Set_quorum_threshold } from './generated/Set_quorum_threshold';
import { Set_voting_period } from './generated/Set_voting_period';
import { Transfer } from './generated/Transfer';
import { Transfer_contract_tokens } from './generated/Transfer_contract_tokens';
import { Transfer_ownership } from './generated/Transfer_ownership';
import { Update_operators } from './generated/Update_operators';
import { Vote } from './generated/Vote';

function println(s: string) {
  console.log(s);
}

type Callback = (Contract) => ContractMethod<ContractProvider>

type ExtractMapKey<MapType> = MapType extends Map<infer R, infer V> ? R : null;
type ExtractMapValue<MapType> = MapType extends Map<infer R, infer V> ? V : null;

const unit = ["Unit"]; //https://github.com/ecadlabs/taquito/issues/526

/**
 * Wraps BaseDAO Contract methods
 */
export class BaseDAOContract {
  private contractAddr: string;
  private nodeAddr: string;
  private senderSk: string;
  private contract: undefined | Promise<ContractAbstraction<ContractProvider>>;

  public debug: boolean;

  public lastOperationHash: undefined | string;

  constructor(nodeAddr: string, senderSk: string, contractAddr: string) {
    this.contractAddr = contractAddr;
    this.nodeAddr = nodeAddr;
    this.senderSk = senderSk;
    this.debug = false;
  }

  private initContract(): Promise<ContractAbstraction<ContractProvider>> {
    if (this.contract === undefined) {
      const Tezos = new TezosToolkit(this.nodeAddr);
      Tezos.setProvider({ signer: new InMemorySigner(this.senderSk) });
      this.contract = Tezos.contract.at(this.contractAddr);
    }
    return this.contract;
  }

  public inspectParameter(): void {
    this.initContract().then(contract => {
      const methods = contract.parameterSchema.ExtractSignatures();
      println(JSON.stringify(methods, null, 2));
      });
  }

  private withContract(callback: Callback): Promise<string | void> {

    // Initialize contract if it hasn't been initialized
    return this.initContract().then(callback)
        .then((method:ContractMethod<ContractProvider>) => {
          if (this.debug) {
            let tp = method.toTransferParams();
            println(JSON.stringify(tp, null, 2));
          }
          return method.send();
        })
        .then((op: any): string => {
          println(`Waiting for ${op.hash} to be confirmed...`);
          return op.confirmation(1).then(() => {
            this.lastOperationHash = op.hash;
            return this;
          });
        })
        .catch((error) => println(`Error: ${JSON.stringify(error, null, 2)}`));
  }

  // entrypoint methods
  balance_of(arg: Balance_of): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.balance_of(arg.requests, arg.callback));
  }

  burn(arg: Burn): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.burn(arg.from_, arg.token_id, arg.amount));
  }

  accept_ownership(): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.accept_ownership(unit));
  }

  call_custom(arg: CallCustom): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.callCustom(arg[0], arg[1]));
  }

  confirm_migration(): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.confirm_migration(unit));
  }

  drop_proposal(arg: Drop_proposal): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.drop_proposal(arg));
  }

  flush(arg: Flush): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.flush(arg));
  }

  get_total_supply(arg: Get_total_supply): Promise<string|void> {
    return this.withContract(
      contract =>  contract.methods.get_total_supply(arg.viewParam, arg.viewCallbackTo));
  }

  getVotePermitCounter(arg: GetVotePermitCounter): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.getVotePermitCounter(arg.viewParam, arg.viewCallbackTo));
  }

  migrate(arg: Migrate): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.migrate(arg));
  }

  mint(arg: Mint): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.mint(arg.to_, arg.token_id, arg.amount));
  }

  propose(arg: Propose): Promise<string|void> {
    let proposalMetadata : MichelsonMap<ExtractMapKey<Propose["proposal_metadata"]>, ExtractMapValue<Propose["proposal_metadata"]>> = new MichelsonMap();
    for (let [k, v] of arg.proposal_metadata.entries()) {
      proposalMetadata.set(k, v);
    }

    return this.withContract(
      contract => contract.methods.propose(arg.frozen_token, proposalMetadata));
  }

  set_quorum_threshold(arg: Set_quorum_threshold): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.set_quorum_threshold(arg));
  }

  set_voting_period(arg: Set_voting_period): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.set_voting_period(arg));
  }

  startup(arg: Startup): Promise<string|void> {
    return this.withContract((contract) => {
      if (arg == null) {
        return contract.methods.startup(arg);
      } else {
        return contract.methods.startup(arg[0], arg[1]);
      }
    });
  }

  transfer(arg: Transfer): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.transfer(arg));
  }

  transfer_contract_tokens(arg: Transfer_contract_tokens): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.transfer_contract_tokens(arg.contract_address, arg.params));
  }

  transfer_ownership(arg: Transfer_ownership): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.transfer_ownership(arg));
  }

  update_operators(arg: Update_operators) {

    return this.withContract(
      contract => contract.methods.update_operators(arg));
  }

  vote(arg: Vote) {
    return this.withContract(
      contract => contract.methods.vote(arg));
  }
}
