-module(kvs).

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

-type t() :: [{term(), term()}].

-spec from_proplist(proplists:proplist()) -> t().
from_proplist(Proplist) ->
    proplists:unfold(Proplist).

-spec get(Key, KVs) -> Value when
      Key :: term(),
      KVs :: t(),
      Value :: term().
get(Key, KVs) ->
    case lists:keyfind(Key, 1, KVs) of
        false -> erlang:error({badkey, Key}, [Key, KVs]);
        {Key, Value} -> Value
    end.

-spec get(Key, KVs, Default) -> Value when
      Key :: term(),
      KVs :: t(),
      Default :: term(),
      Value :: term().
get(Key, KVs, Default) ->
    case lists:keyfind(Key, 1, KVs) of
        false -> Default;
        {Key, Value} -> Value
    end.

-spec proplist_to_map(proplists:proplist()) -> map().
proplist_to_map(Proplist) ->
    maps:from_list(from_proplist(Proplist)).
