-module(algolia_index).

-export([add_object/2, update_object/2, search/2, search/3, get_settings/1, set_settings/2]).
-export([partial_update_object/2, delete_object/2, get_object/2, get_object/3, delete/1, clear/1]).
-export([set_settings_request/2]).
-export([delete_request/1, clear_request/1]).

-include("client.hrl").
-include("index.hrl").

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

update_object(Index, Object) ->
  IndexName = Index#algolia_index.index_name,
  ObjectID = maps:get(<<"objectID">>, Object),
  Path = lists:flatten(io_lib:format("/1/indexes/~s/~s", [IndexName, ObjectID])),
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({write, put, Path, Object}).

partial_update_object(Index, Object) ->
  IndexName = Index#algolia_index.index_name,
  ObjectID = maps:get(<<"objectID">>, Object),
  Path = lists:flatten(io_lib:format("/1/indexes/~s/~s/partial", [IndexName, ObjectID])),
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({write, post, Path, Object}).

delete_object(Index, ObjectID) ->
  IndexName = Index#algolia_index.index_name,
  Path = lists:flatten(io_lib:format("/1/indexes/~s/~s", [IndexName, ObjectID])),
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({write, delete, Path}).

search(Index, Query) ->
  search(Index, Query, #{}).

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

get_object(Index, ObjectID) ->
  get_object(Index, ObjectID, <<"">>).

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

get_settings(Index) ->
  IndexName = Index#algolia_index.index_name,
  Path = lists:flatten(io_lib:format("/1/indexes/~s/settings", [IndexName])),
  Transport = Index#algolia_index.client#algolia_client.transport,
  Transport({read, get, Path}).

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
