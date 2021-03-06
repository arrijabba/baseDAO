// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "defaults.mligo"
#include "types.mligo"
#include "proposal.mligo"
#include "registryDAO/types.mligo"

// The following include is required so that we have a function with an
// entrypoint type to use with `compile-storage` command to make Michelson
// storage expression.
#include "base_DAO.mligo"

(*
 * Implements logic that differentiates between various proposals, and wraps
 * the relavant data in appropriate constructors.
 *)
let extract_proposal (metadata : proposal_metadata) : proposal_type =
  match Map.find_opt "agoraPostID" metadata with
  | Some (packed_b) ->
      begin
        match ((Bytes.unpack packed_b) : (nat option)) with
        | Some (agora_post_id) ->
            begin
              match Map.find_opt "updates" metadata with
              | Some (packed_b) ->
                  begin
                    match ((Bytes.unpack packed_b) : (registry_diff option)) with
                    | Some (diff) -> Normal_proposal diff
                    | None ->
                      (failwith "UPDATES_UNPACKING_FAILED" : proposal_type)
                  end
              | None -> (failwith "UPDATES_NOT_FOUND" : proposal_type)
            end
        | None -> (failwith "UNPACKING FAILED" : proposal_type)
      end
  | None ->
      begin
        match Map.find_opt "update_receivers" metadata with
          | Some (packed_b) ->
              begin
                match ((Bytes.unpack packed_b) : update_receiver_param option) with
                  | Some upr_param -> Update_receivers_proposal upr_param
                  | None -> (failwith "UNPACKING FAILED" : proposal_type)
              end
          | None -> Configuration_proposal
      end

let require_extra_value (key_name, ce : string * contract_extra) : bytes =
    match Map.find_opt key_name ce with
    | Some (packed_b) -> packed_b
    | None -> (failwith "CONFIG_VALUE_NOT_FOUND" : bytes)

let require_unpacked_nat(packed_b: bytes) : nat =
  match ((Bytes.unpack packed_b) : (nat option)) with
  | Some (v) -> v
  | None -> (failwith "UNPACKING_FAILED" : nat)

let require_unpacked_nat_opt(packed_b: bytes) : nat option =
  match ((Bytes.unpack packed_b) : ((nat option) option)) with
  | Some (v) -> v
  | None -> (failwith "UNPACKING_FAILED" : nat option)

let require_unpacked_registry (packed_b: bytes) : registry =
  match ((Bytes.unpack packed_b) : (registry option)) with
  | Some (v) -> v
  | None -> (failwith "UNPACKING_FAILED" : registry)

let require_unpacked_registry_affected (packed_b: bytes) : registry_affected =
  match ((Bytes.unpack packed_b) : (registry_affected option)) with
  |  Some (v) -> v
  | None -> (failwith "UNPACKING_FAILED" : registry_affected)

let require_unpacked_proposal_receivers (packed_b: bytes) : proposal_receivers =
  match ((Bytes.unpack packed_b) : (proposal_receivers option)) with
  | Some (v) -> v
  | None -> (failwith "UNPACKING_FAILED" : proposal_receivers)

let lookup_config (key_name, ce : string * contract_extra) : nat =
    require_unpacked_nat(require_extra_value(key_name, ce))

let lookup_config_in_proposal (key_name, ce : string * contract_extra) : nat option =
    require_unpacked_nat_opt(require_extra_value(key_name, ce))

let require_config (key_name, ce : string * contract_extra) : nat = lookup_config (key_name, ce)

let apply_diff_registry (diff, registry : registry_diff * registry) : registry =
  let
    foldFn (registry, update: registry * (registry_key * registry_value option)) : registry =
      let registry_key = update.0 in
      let registry_value = update.1 in
      Map.update registry_key registry_value registry
  in List.fold foldFn diff registry

let apply_diff_registry_affected (proposal_key, diff, registry_affected : proposal_key
        * registry_diff * registry_affected) : registry_affected =
  let
    foldFn (registry_affected, update: registry_affected * (registry_key *
      registry_value option)) : registry_affected =
      let registry_key = update.0 in
      Map.update registry_key (Some (proposal_key)) registry_affected
  in List.fold foldFn diff registry_affected

let apply_diff (diff, ce : registry_diff * contract_extra) : contract_extra =
  let registry = require_unpacked_registry(require_extra_value("registry", ce)) in
  let new_registry : registry = apply_diff_registry (diff, registry) in
  Map.update "registry" (Some (Bytes.pack new_registry)) ce

let apply_diff_affected (proposal_key, diff, ce : proposal_key *
    registry_diff * contract_extra) : contract_extra =
  let registry_af = require_unpacked_registry_affected(require_extra_value("registry_affected", ce)) in
  let new_registry_af : registry_affected = apply_diff_registry_affected (proposal_key, diff, registry_af)
  in Map.update "registry_affected" (Some (Bytes.pack new_registry_af)) ce

// Configuration Lambdas

(*
 * Proposal check lambda : returns true if following two checks are true.
 * 1. if proposer has locked frozen_scale_value * s + frozen_extra_value
 * where s = size(pack(proposalMetadata)) and frozen_scale_value and frozen_extra_value
 * are from storage configuration.
 * 2. proposal size < max_proposal_size where max_proposal_size is from stored configuration.
 *)
let registry_DAO_proposal_check (params, extras : propose_params * contract_extra) : bool =
  let proposal_size = Bytes.size(Bytes.pack(params.proposal_metadata)) in
  let frozen_scale_value = require_config ("frozen_scale_value", extras) in
  let frozen_extra_value = require_config ("frozen_extra_value", extras) in
  let max_proposal_size = require_config ("max_proposal_size", extras) in
  let required_token_lock = frozen_scale_value * proposal_size + frozen_extra_value
  in (params.frozen_token = required_token_lock) && (proposal_size < max_proposal_size)

(*
 * Proposal rejection return lambda: returns `slash_scale_value * frozen / slash_division_value`
 * where slash_scale_value and slash_division_value are specified by the DAO creator
 * in configuration and frozen is the amount that was frozen by the proposer.
 *)
let registry_DAO_rejected_proposal_return_value (params, extras : proposal * contract_extra) : nat =
  let slash_scale_value = require_config ("slash_scale_value", extras) in
  let slash_division_value = require_config ("slash_division_value", extras)
  in (slash_scale_value * params.proposer_frozen_token) / slash_division_value

type update_fn = (address * address set) -> proposal_receivers

let update_receivers(current_set, updates, update_fn : proposal_receivers * address list * update_fn) : proposal_receivers =
  let
    foldFn (existing, item : proposal_receivers * address) : proposal_receivers = update_fn (item, existing)
  in List.fold foldFn updates current_set

(*
 * Uses the keys in the proposal metadata to differentiate between configuration
 * proposal and normal proposals. If the map contains `agoraPostID` key, the proposal
 * is considerd as 'normal'. Or else it is considered as carrying a 'configuration'
 * proposal.
 *
 * For 'normal' proposals, the contract_extra is expected to contain the bigmap
 * for registry and bigmap for key update tracking under keys 'registry' and
 * 'registry_affected' keys.
 *
 * For 'configuration' proposal, the code expects all the configuration keys
 * included in the proposal (or it throws an error). The expected keys are
 * "frozen_scale_value", "frozen_extra_value", "max_proposal_size", "slash_scale_value"
 * and "slash_division_value". 'None' values are ignored and 'Some' values are
 * used for updating corresponding configuraton.
 *)
let registry_DAO_decision_lambda (proposal, extras : proposal * contract_extra)
    : operation list * contract_extra =
  let propose_param : propose_params = {
    frozen_token = proposal.proposer_frozen_token;
    proposal_metadata = proposal.metadata
    } in
  let proposal_key = to_proposal_key (propose_param, proposal.proposer) in
  match extract_proposal(proposal.metadata) with
  | Normal_proposal diff ->
      let extras = apply_diff(diff, extras) in
      let extras =
        apply_diff_affected(proposal_key, diff, extras) in
        (([] : operation list), extras)
  | Update_receivers_proposal urp ->
      let current_set = require_unpacked_proposal_receivers(require_extra_value("proposal_receivers", extras)) in
      begin
        let new_set = match urp with
          | Add_receivers receivers ->
              update_receivers(current_set, receivers, (fun (i, c : address * address set) -> Set.add i c))
          | Remove_receivers receivers ->
              update_receivers(current_set, receivers, (fun (i, c : address * address set) -> Set.remove i c))
        in (([] : operation list), Map.update "proposal_receivers" (Some (Bytes.pack new_set)) extras)
      end
  | Configuration_proposal ->
      let m_frozen_scale_value = lookup_config_in_proposal ("frozen_scale_value", proposal.metadata) in
      let m_frozen_extra_value = lookup_config_in_proposal ("frozen_extra_value", proposal.metadata) in
      let m_max_proposal_size = lookup_config_in_proposal ("max_proposal_size", proposal.metadata) in
      let m_slash_scale_value = lookup_config_in_proposal ("slash_scale_value", proposal.metadata) in
      let m_slash_division_value = lookup_config_in_proposal ("slash_division_value", proposal.metadata) in
      let new_ce = match m_frozen_scale_value with
        | Some (frozen_scale_value) ->
            Map.update "frozen_scale_value" (Some (Bytes.pack (frozen_scale_value))) extras
        | None -> extras in

      let new_ce = match m_frozen_extra_value with
        | Some (frozen_extra_value) ->
            Map.update "frozen_extra_value" (Some (Bytes.pack (frozen_extra_value))) new_ce
        | None -> new_ce in

      let new_ce = match m_max_proposal_size with
        | Some (max_proposal_size) ->
            Map.update "max_proposal_size" (Some (Bytes.pack (max_proposal_size))) new_ce
        | None -> new_ce in

      let new_ce = match m_slash_scale_value with
        | Some (slash_scale_value) ->
            Map.update "slash_scale_value" (Some (Bytes.pack (slash_scale_value))) new_ce
        | None -> new_ce in

      let new_ce = match m_slash_division_value with
        | Some (slash_division_value) ->
            Map.update "slash_division_value" (Some (Bytes.pack (slash_division_value))) new_ce
        | None -> new_ce
      in (([] : operation list), new_ce)

let default_registry_DAO_full_storage (admin, token_address, frozen_scale_value, frozen_extra_value, max_proposal_size, slash_scale_value, slash_division_value
    : (address * address * nat * nat * nat * nat * nat)) : full_storage =
  let (startup, (store, config)) = default_full_storage (admin, token_address) in
  let new_storage = { store with
    extra = Map.literal [
          ("registry" , Bytes.pack (Map.empty : registry));
          ("registry_affected" , Bytes.pack (Map.empty : registry_affected));
          ("proposal_receivers" , Bytes.pack (Set.empty : proposal_receivers));
          ("frozen_scale_value" , Bytes.pack frozen_scale_value);
          ("frozen_extra_value" , Bytes.pack frozen_extra_value);
          ("max_proposal_size" , Bytes.pack max_proposal_size);
          ("slash_scale_value" , Bytes.pack slash_scale_value);
          ("slash_division_value" , Bytes.pack slash_division_value);
          ];
  } in
  let new_config = { config with
    proposal_check = registry_DAO_proposal_check;
    rejected_proposal_return_value = registry_DAO_rejected_proposal_return_value;
    decision_lambda = registry_DAO_decision_lambda;
    } in
  (startup, (new_storage, new_config))


// We are not using this right now, but just leaving here in case we might want it
// soon.
let successful_proposal_receiver_view (full_storage : full_storage): proposal_receivers =
  match ((Map.find_opt "proposal_receivers" full_storage.1.0.extra) : bytes option) with
    | Some (packed_b) ->
        begin
          match (Bytes.unpack packed_b : proposal_receivers option) with
          | Some r -> r
          | None -> ((failwith "Unpacking failed") : proposal_receivers)
        end
    | None -> (failwith "'proposal_receivers' key not found" : proposal_receivers)
