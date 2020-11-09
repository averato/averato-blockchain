%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc Consensus module for common tests - used when we are only interested in the state transitions
%%% @end
%%% -------------------------------------------------------------------
-module(aec_consensus_common_tests).

-export([]).

-ifdef(TEST).
-behavior(aec_consensus).

-define(TAG, 1337).

%% API
-export([ can_be_turned_off/0
        , assert_config/1
        , dirty_validate_key_header/0
        , start/1
        , stop/0
        , is_providing_extra_http_endpoints/0
        , client_request/1
        , extra_from_header/1
        , recent_cache_n/0
        , recent_cache_trim_header/1
        , keyblocks_for_target_calc/0
        , keyblock_create_adjust_target/2
        , dirty_validate_block_pre_conductor/1
        , dirty_validate_key_header/1
        , genesis_block_with_state/0
        , genesis_height/0
        , genesis_header/0
        , genesis_state/0
        , genesis_target/0
        , key_header_for_sealing/1
        , validate_key_header_seal/2
        , generate_key_header_seal/5
        , set_key_block_seal/2
        , nonce_for_sealing/1
        , next_nonce_for_sealing/2
        , trim_sealing_nonce/2
        , default_target/0
        , assert_key_target_range/1
        , key_header_difficulty/1 ]).

can_be_turned_off() -> false.
assert_config(_Config) -> ok.
start(_Config) -> ok.
stop() -> ok.

dirty_validate_key_header() -> error(todo).

is_providing_extra_http_endpoints() -> false.
client_request(_) -> error(unsupported).

extra_from_header(_) ->
    #{consensus => ?MODULE}.

recent_cache_n() -> 1.
recent_cache_trim_header(_) -> {}.

keyblocks_for_target_calc() -> 0.
keyblock_create_adjust_target(Block, []) -> {ok, Block}.

dirty_validate_block_pre_conductor(_) -> ok.
dirty_validate_key_header(_) -> error(todo).

genesis_block_with_state() -> error(todo).
genesis_height() -> error(todo).
genesis_header() -> error(todo).
genesis_state() -> error(todo).

genesis_target() -> ?TAG.

key_header_for_sealing(Header) ->
    aec_headers:root_hash(Header).

validate_key_header_seal(_Header, _Protocol) ->
    ok.

generate_key_header_seal(_, _, ?TAG, _, _) ->
    { continue_mining, {ok, ?TAG} }.

set_key_block_seal(Block, ?TAG) ->
    aec_blocks:set_nonce_and_pow(Block, ?TAG, ?TAG).

nonce_for_sealing(_Header) ->
    ?TAG.

next_nonce_for_sealing(?TAG, _) ->
    ?TAG.

trim_sealing_nonce(?TAG, _) ->
    ?TAG.

default_target() ->
    ?TAG.

assert_key_target_range(?TAG) ->
    ok.

key_header_difficulty(_) ->
    ?TAG.
-endif.
