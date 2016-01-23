-module(algolia_index).

-export([add_object/2, update_object/2, search/2, search/3, get_settings/1, set_settings/2]).
-export([partial_update_object/2, delete_object/2, get_object/2, get_object/3, delete/1, clear/1]).

-include("client.hrl").
-include("index.hrl").

-type(response() :: {ok, map()} | {error, any()}).

-spec(add_object/2 :: (#algolia_index{}, map()) -> response()).
add_object(Index, Object) ->
  IndexName = Index#algolia_index.index_name,
  case maps:get(<<"objectID">>, Object, false) of
    false ->
      Path = lists:flatten(io_lib:format("/1/indexes/~s", [IndexName])),
      Method = post;
    ObjectID ->
      Path = lists:flatten(io_lib:format("/1/indexes/~s/~s", [IndexName, ObjectID])),
      Method = put
  end,
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({write, Method, Path, Object}).

-spec(update_object/2 :: (#algolia_index{}, map()) -> response()).
update_object(Index, Object) ->
  IndexName = Index#algolia_index.index_name,
  ObjectID = maps:get(<<"objectID">>, Object),
  Path = lists:flatten(io_lib:format("/1/indexes/~s/~s", [IndexName, ObjectID])),
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({write, put, Path, Object}).

-spec(partial_update_object/2 :: (#algolia_index{}, map()) -> response()).
partial_update_object(Index, Object) ->
  IndexName = Index#algolia_index.index_name,
  ObjectID = maps:get(<<"objectID">>, Object),
  Path = lists:flatten(io_lib:format("/1/indexes/~s/~s/partial", [IndexName, ObjectID])),
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({write, post, Path, Object}).

-spec(delete_object/2 :: (#algolia_index{}, binary()) -> response()).
delete_object(Index, ObjectID) ->
  IndexName = Index#algolia_index.index_name,
  Path = lists:flatten(io_lib:format("/1/indexes/~s/~s", [IndexName, ObjectID])),
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({write, delete, Path}).

-spec(search/2 :: (#algolia_index{}, binary()) -> response()).
search(Index, Query) ->
  search(Index, Query, #{}).

-spec(search/3 :: (#algolia_index{}, binary(), map()) -> response()).
search(Index, Query, AdditionalParams) ->
  IndexName = Index#algolia_index.index_name,
  Path = lists:flatten(io_lib:format("/1/indexes/~s/query", [IndexName])),
  QueryWithParams = maps:put(<<"query">>, Query, AdditionalParams),
  Params = build_query_params(QueryWithParams),
  Body = #{
    <<"params">> => list_to_binary(Params)
  },
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({read, post, Path, Body}).

-spec(get_object/2 :: (#algolia_index{}, string()) -> response()).
get_object(Index, ObjectID) ->
  get_object(Index, ObjectID, <<"">>).

-spec(get_object/3 :: (#algolia_index{}, string(), string()) -> response()).
get_object(Index, ObjectID, Attribute) ->
  IndexName = Index#algolia_index.index_name,
  case Attribute of
    <<"">> ->
      Path = lists:flatten(io_lib:format("/1/indexes/~s/~s", [IndexName, ObjectID]));
    Attribute ->
      UrlParams = build_query_params(#{<<"attribute">> => Attribute}),
      Path = lists:flatten(io_lib:format("/1/indexes/~s/~s?~s", [IndexName, ObjectID, UrlParams]))
  end,
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({read, get, Path}).

-spec(get_settings/1 :: (#algolia_index{}) -> response()).
get_settings(Index) ->
  IndexName = Index#algolia_index.index_name,
  Path = lists:flatten(io_lib:format("/1/indexes/~s/settings", [IndexName])),
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({read, get, Path}).

-spec(set_settings/2 :: (#algolia_index{}, map()) -> response()).
set_settings(Index, Settings) ->
  IndexName = Index#algolia_index.index_name,
  Path = lists:flatten(io_lib:format("/1/indexes/~s/settings", [IndexName])),
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({write, put, Path, Settings}).

-spec(delete/1 :: (#algolia_index{}) -> response()).
delete(Index) ->
  IndexName = Index#algolia_index.index_name,
  Path = lists:flatten(io_lib:format("/1/indexes/~s", [IndexName])),
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({write, delete, Path}).

-spec(clear/1 :: (#algolia_index{}) -> response()).
clear(Index) ->
  IndexName = Index#algolia_index.index_name,
  Path = lists:flatten(io_lib:format("/1/indexes/~s/clear", [IndexName])),
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({write, post, Path}).

build_query_params(Params) ->
  maps:fold(fun format_query_string/3, [], Params).

format_query_string(Key, Val, Acc) ->
  case Acc of
    [] ->
      Query = io_lib:format("~s=~s", [
        http_uri:encode(binary_to_list(Key)),
        http_uri:encode(format_query_value(Val))
      ]);
    _ ->
      Query = io_lib:format("~s&~s=~s", [
        Acc,
        http_uri:encode(binary_to_list(Key)),
        http_uri:encode(format_query_value(Val))
      ])
  end,
  lists:flatten(Query).

format_query_value(Value) when is_list(Value) ->
  Value;
format_query_value(Value) when is_binary(Value) ->
  binary_to_list(Value);
format_query_value(Value) when is_integer(Value) ->
  integer_to_list(Value).
