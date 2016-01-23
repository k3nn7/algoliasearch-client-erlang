-module(algolia_index).

-export([add_object/2, update_object/2, search/2, search/3, get_settings/1, set_settings/2]).
-export([partial_update_object/2, delete_object/2, get_object/2, get_object/3, delete/1, clear/1]).
-export([search_request/3, get_settings_request/1, set_settings_request/2]).
-export([update_object_request/2, partial_update_object_request/2, delete_object_request/2]).
-export([get_object_request/2, get_object_request/3, delete_request/1, clear_request/1]).

-include("client.hrl").
-include("index.hrl").

add_object(Index, Object) ->
  IndexName = http_uri:encode(Index#algolia_index.index_name),
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

update_object(Index, Object) ->
  algolia_transport:handle_response(
    algolia_transport:do_request(update_object_request(Index, Object))).

update_object_request(Index, Object = {ObjectPropList}) ->
  {IndexName, AppId, ApiKey, _, WriteHost} = get_index_options(Index),
  ObjectID = proplists:get_value(<<"objectID">>, ObjectPropList),
  Path = lists:flatten(io_lib:format("/1/indexes/~s/~s", [IndexName, ObjectID])),
  algolia_transport:build_request(put, WriteHost, Path, Object, AppId, ApiKey).

partial_update_object(Index, Object) ->
  algolia_transport:handle_response(
    algolia_transport:do_request(partial_update_object_request(Index, Object))).

partial_update_object_request(Index, Object = {ObjectPropList}) ->
  {IndexName, AppId, ApiKey, _, WriteHost} = get_index_options(Index),
  ObjectID = proplists:get_value(<<"objectID">>, ObjectPropList),
  Path = lists:flatten(io_lib:format("/1/indexes/~s/~s/partial", [IndexName, ObjectID])),
  algolia_transport:build_request(post, WriteHost, Path, Object, AppId, ApiKey).

delete_object(Index, ObjectID) ->
  algolia_transport:handle_response(
    algolia_transport:do_request(delete_object_request(Index, ObjectID))).

delete_object_request(Index, ObjectID) ->
  {IndexName, AppId, ApiKey, _, WriteHost} = get_index_options(Index),
  Path = lists:flatten(io_lib:format("/1/indexes/~s/~s", [IndexName, ObjectID])),
  algolia_transport:build_request(delete, WriteHost, Path, AppId, ApiKey).

search(Index, Query) ->
  search(Index, Query, {[]}).

search(Index, Query, Params) ->
  algolia_transport:handle_response(
    algolia_transport:do_request(
      search_request(Index, Query, Params))).

search_request(Index, Query, AdditionalParams) ->
  {IndexName, AppId, ApiKey, ReadHost, _} = get_index_options(Index),
  Params = build_search_params(Query, AdditionalParams),
  Body = {[
    {<<"params">>, list_to_binary(Params)}
  ]},
  Path = lists:flatten(io_lib:format("/1/indexes/~s/query", [IndexName])),
  algolia_transport:build_request(post, ReadHost, Path, Body, AppId, ApiKey).

get_object(Index, ObjectID) ->
  get_object_request(Index, ObjectID).

get_object(Index, ObjectID, Attribute) ->
  get_object_request(Index, ObjectID, Attribute).

get_object_request(Index, ObjectID) ->
  get_object_request(Index, ObjectID, <<"">>).

get_object_request(Index, ObjectID, Attribute) ->
  {IndexName, AppId, ApiKey, ReadHost, _} = get_index_options(Index),
  case Attribute of
    <<"">> ->
      Path = lists:flatten(io_lib:format("/1/indexes/~s/~s", [IndexName, ObjectID]));
    Attribute ->
      UrlParams = build_query_params([{<<"attribute">>, Attribute}], []),
      Path = lists:flatten(io_lib:format("/1/indexes/~s/~s?~s", [IndexName, ObjectID, UrlParams]))
  end,
  algolia_transport:build_request(get, ReadHost, Path, AppId, ApiKey).

get_settings(Index) ->
  algolia_transport:handle_response(
    algolia_transport:do_request(
      get_settings_request(Index)
    )
  ).

get_settings_request(Index) ->
  {IndexName, AppId, ApiKey, ReadHost, _} = get_index_options(Index),
  Path = lists:flatten(io_lib:format("/1/indexes/~s/settings", [IndexName])),
  algolia_transport:build_request(get, ReadHost, Path, AppId, ApiKey).

set_settings(Index, Settings) ->
  algolia_transport:handle_response(
    algolia_transport:do_request(
      set_settings_request(Index, Settings))).

set_settings_request(Index, Settings) ->
  {IndexName, AppId, ApiKey, _, WriteHost} = get_index_options(Index),
  Path = lists:flatten(io_lib:format("/1/indexes/~s/settings", [IndexName])),
  algolia_transport:build_request(put, WriteHost, Path, Settings, AppId, ApiKey).

delete(Index) ->
  algolia_transport:handle_response(
    algolia_transport:do_request(delete_request(Index))).

delete_request(Index) ->
  {IndexName, AppId, ApiKey, _, WriteHost} = get_index_options(Index),
  Path = lists:flatten(io_lib:format("/1/indexes/~s", [IndexName])),
  algolia_transport:build_request(delete, WriteHost, Path, AppId, ApiKey).

clear(Index) ->
  algolia_transport:handle_response(
    algolia_transport:do_request(clear_request(Index))).

clear_request(Index) ->
  {IndexName, AppId, ApiKey, _, WriteHost} = get_index_options(Index),
  Path = lists:flatten(io_lib:format("/1/indexes/~s/clear", [IndexName])),
  algolia_transport:build_request(post, WriteHost, Path, AppId, ApiKey).

get_index_options(Index) ->
  IndexName = http_uri:encode(Index#algolia_index.index_name),
  Client = Index#algolia_index.client,
  [ReadHost | _] = Client#algolia_client.read_hosts,
  [WriteHost | _] = Client#algolia_client.write_hosts,
  AppId = Client#algolia_client.app_id,
  ApiKey = Client#algolia_client.api_key,
  {IndexName, AppId, ApiKey, ReadHost, WriteHost}.

build_search_params(Query, {Params}) ->
  QueryWithParams = lists:append([
    Params,
    [{<<"query">>, Query}]
  ]),
  build_query_params(QueryWithParams, []).

build_query_params([], Acc) ->
  Acc;
build_query_params([{Key, Val} | Rest], Acc) ->
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
  build_query_params(Rest, lists:flatten(Query)).

format_query_value(Value) when is_list(Value) ->
  Value;
format_query_value(Value) when is_binary(Value) ->
  binary_to_list(Value);
format_query_value(Value) when is_integer(Value) ->
  integer_to_list(Value).
