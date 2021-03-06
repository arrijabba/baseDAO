// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Permit.hs module

// Complete parameter with common signature data and pack them into bytes that
// will be signed in permit.
//
// This slightly differs from the Haskell version in that it already
// returns packed data; LIGO does not let us return polymorphic DataToSign.
[@inline]
let vote_param_to_signed_data (param, ctrt_address, store : vote_param * address * storage)
    : bytes * storage =
  ( Bytes.pack
    ( (Tezos.chain_id, ctrt_address)
    , (store.permits_counter, param)
    )
  , { store with permits_counter = store.permits_counter + 1n }
  )

// Verify permit and return its author.
[@inline]
let checked_permit_sender (permit, data_to_sign : permit * bytes): address =
  if Crypto.check permit.key permit.signature data_to_sign
  then Tezos.address (Tezos.implicit_account (Crypto.hash_key (permit.key)))
  else ([%Michelson ({| { FAILWITH } |} : string * bytes -> address)]
        ("MISSIGNED", data_to_sign) : address)

// Check that permit is signed by its author, and return the author
// and the parameter to work with.
[@inline]
let verify_permit_vote
  (permit, vote_param, ctrt_address, store : permit * vote_param * address * storage)
    : (vote_param * address * storage) =
  let (data_to_sign, store) = vote_param_to_signed_data (vote_param, ctrt_address, store) in
  let permit_sender = checked_permit_sender (permit, data_to_sign) in
  (vote_param, permit_sender, store)

// Check that permit is signed by its author, and return the data
// carried under permit protection.
[@inline]
let verify_permit_protected_vote
  (permited, ctrt_address, store : vote_param_permited * address * storage)
    : vote_param * address * storage =
  match permited.permit with
    None -> (permited.argument, Tezos.sender, store)
  | Some permit -> verify_permit_vote (permit, permited.argument, ctrt_address, store)

let get_vote_permit_counter (param, store : vote_permit_counter_param * storage) : return =
  ( Tezos.transaction store.permits_counter 0mutez param.callback
    :: ([] : operation list)
  , store
  )

let get_total_supply (param, store : get_total_supply_param * storage) : return =
  let result = match Big_map.find_opt param.token_id store.total_supply with
      None ->
        (failwith("FA2_TOKEN_UNDEFINED") : nat)
    | Some v -> v in
  ( Tezos.transaction result 0mutez param.callback
    :: ([] : operation list)
  , store
  )
