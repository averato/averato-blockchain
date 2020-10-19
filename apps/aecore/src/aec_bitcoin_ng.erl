%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc BitcoinNG consensus module
%%% @end
%%% -------------------------------------------------------------------
-module(aec_bitcoin_ng).
-behavior(aec_consensus).

%% API
-export([ can_be_turned_off/0
        , dirty_validate_key_header/0
        , prepare_start/0
        , start/0
        , stop/0
        , is_providing_extra_http_endpoints/0
        , extra_from_header/1
        , recent_cache_n/0
        , recent_cache_trim_header/1
        , dirty_validate_key_header/1
        , genesis_block_with_state/0
        , genesis_height/0
        , genesis_hash/0
        , genesis_state/0
        , validate_key_seal/1]).

can_be_turned_off() -> true.
dirty_validate_key_header() -> error(todo).
prepare_start() -> error(todo).
start() -> error(todo).
stop() -> error(todo).

is_providing_extra_http_endpoints() -> false.
extra_from_header(_) ->
    #{consensus => ?MODULE}.

recent_cache_n() -> 10.
recent_cache_trim_header(_) -> {}.

dirty_validate_key_header(_) -> error(todo).

genesis_block_with_state() -> error(todo).
genesis_height() -> error(todo).
genesis_hash() -> error(todo).
genesis_state() -> error(todo).

validate_key_seal(Header) ->
    %% Zero nonce and pow_evidence before hashing, as this is how the mined block
    %% got hashed.
    Nonce = aec_headers:nonce(Header),
    Evd = aec_headers:pow(Header),
    Target = aec_headers:target(Header),
    Header1 = aec_headers:set_nonce_and_pow(Header, 0, no_value),
    HeaderBinary = aec_headers:serialize_to_binary(Header1),
    case aec_mining:verify(HeaderBinary, Nonce, Evd, Target) of
        true ->
            ok;
        false ->
            {error, incorrect_pow}
    end.
