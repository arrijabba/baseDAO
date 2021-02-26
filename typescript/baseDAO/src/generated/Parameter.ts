import {Startup} from  './Startup';
import {CallCustom} from  './CallCustom';
import {Accept_ownership} from  './Accept_ownership';
import {Burn} from  './Burn';
import {Balance_of} from  './Balance_of';
import {Transfer} from  './Transfer';
import {Update_operators} from  './Update_operators';
import {Confirm_migration} from  './Confirm_migration';
import {Drop_proposal} from  './Drop_proposal';
import {Flush} from  './Flush';
import {GetVotePermitCounter} from  './GetVotePermitCounter';
import {Get_total_supply} from  './Get_total_supply';
import {Migrate} from  './Migrate';
import {Mint} from  './Mint';
import {Propose} from  './Propose';
import {Set_quorum_threshold} from  './Set_quorum_threshold';
import {Set_voting_period} from  './Set_voting_period';
import {Transfer_ownership} from  './Transfer_ownership';
import {Vote} from  './Vote';
import {Transfer_contract_tokens} from  './Transfer_contract_tokens';
export type Parameter =
  | Startup
  | CallCustom
  | Accept_ownership
  | Burn
  | Balance_of
  | Transfer
  | Update_operators
  | Confirm_migration
  | Drop_proposal
  | Flush
  | GetVotePermitCounter
  | Get_total_supply
  | Migrate
  | Mint
  | Propose
  | Set_quorum_threshold
  | Set_voting_period
  | Transfer_ownership
  | Vote
  | Transfer_contract_tokens
