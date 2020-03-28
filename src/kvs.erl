-module(kvs).
-author("Radek Szymczyszyn <lavrin@gmail.com>").

-export([
         from_proplist/1,
         get/2, get/3,
         proplist_to_map/1

         %% TODO:
         %filter/2,
         %find/2,
         %fold/3,
         %is_key/2,
         %iterator/1,
         %keys/1,
         %map/2,
         %merge/2,
         %new/0,
         %next/1,
         %put/3,
         %remove/2,
         %size/1,
         %take/2,
         %to_list/1,
         %update/3,
         %update_with/3, update_with/4,
         %values/1,
         %with/2,
         %without/2
        ]).

-export_type([t/0,
              keywords/0]).

-type t() :: [{term(), term()}].
%% The default `kvs' type - a list of key-value pairs.

-type keywords() :: [{atom(), term()}].
%% An Elixir-compatible keyword list.
%% A list of pairs in which the first item (the key) is an atom.

%% @doc Unfolds a `Proplist' into a list of key-value pairs.
%%
%% Properties which are not pairs are substituted with pairs
%% where the second element of each is `true'.
%%
%% The call fails with a `function_clause' if `Proplist' is not a list.
%% @since 0.1.0
-spec from_proplist(proplists:proplist()) -> t().
from_proplist(Proplist) when is_list(Proplist) ->
    proplists:unfold(Proplist).

%% @doc Returns the `Value' associated with `Key' if `KVs' contains `Key'.
%%
%% The call fails with a `{badkey, Key}' exception if no value is associated with `Key',
%% or with a `function_clause' if `KVs' is not a list.
%%
%% Example:
%%
%% ```
%% > kvs:get(a, [{a, 3}]).
%% 3
%% > kvs:get(z, [{a, 3}]).
%% ** exception error: {badkey,z}
%%      in function  kvs:get/2
%%         called as kvs:get(z,[{a,3}])
%% '''
%% @since 0.1.0
-spec get(Key, KVs) -> Value when
      Key :: term(),
      KVs :: t(),
      Value :: term().
get(Key, KVs) when is_list(KVs) ->
    case lists:keyfind(Key, 1, KVs) of
        false -> erlang:error({badkey, Key}, [Key, KVs]);
        {Key, Value} -> Value
    end.

%% @doc Returns `Value' associated with `Key' if `KVs' contains `Key'.
%% If no value is associated with `Key', `Default' is returned.
%%
%% The call fails with a `function_clause' if `KVs' is not a list.
%%
%% Example:
%%
%% ```
%% > kvs:get(a, [{a, "Just a"}], "Default value").
%% "Just a"
%% > kvs:get(z, [{a, "Just a"}], "Default value").
%% "Default value"
%% '''
%% @since 0.1.0
-spec get(Key, KVs, Default) -> Value | Default when
      Key :: term(),
      KVs :: t(),
      Default :: term(),
      Value :: term().
get(Key, KVs, Default) when is_list(KVs) ->
    case lists:keyfind(Key, 1, KVs) of
        false -> Default;
        {Key, Value} -> Value
    end.

%% @doc Returns a map built from `Proplist'.
%%
%% The proplist is first unfolded, i.e. properties which are not pairs
%% are substituted with pairs where the second element of each is `true'.
%%
%% The call fails with a `function_clause' if `Proplist' is not a list.
%% @since 0.1.0
-spec proplist_to_map(proplists:proplist()) -> map().
proplist_to_map(Proplist) when is_list(Proplist) ->
    maps:from_list(from_proplist(Proplist)).
