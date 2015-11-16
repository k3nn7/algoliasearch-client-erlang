-module(algolia_index).

-export([add_object/2, add_object_request/2, search_request/2, search/2]).

add_object(Index, Object) ->
  algolia_transport:do_request(add_object_request(Index, Object)).

add_object_request(Index, Object = {ObjectPropList}) ->
  {IndexName, AppId, ApiKey, _, WriteHost} = get_index_options(Index),
  case proplists:get_value(<<"objectID">>, ObjectPropList, false) of
    false ->
      Path = lists:flatten(io_lib:format("/1/indexes/~s", [IndexName])),
      algolia_transport:build_request(post, WriteHost, Path, Object, AppId, ApiKey);
    ObjectID ->
      Path = lists:flatten(io_lib:format("/1/indexes/~s/~s", [IndexName, ObjectID])),
      algolia_transport:build_request(put, WriteHost, Path, Object, AppId, ApiKey)
  end.

search(Index, Query) ->
  algolia_transport:do_request(search_request(Index, Query)).

search_request(Index, Query) ->
  {IndexName, AppId, ApiKey, ReadHost, _} = get_index_options(Index),
  Params = list_to_binary(io_lib:format("query=~s", [Query])),
  Body = {[
    {<<"params">>, Params}
  ]},
  Path = lists:flatten(io_lib:format("/1/indexes/~s/query", [IndexName])),
  algolia_transport:build_request(post, ReadHost, Path, Body, AppId, ApiKey).

get_index_options(_Index = {algolia_index, IndexOptions}) ->
  IndexName = proplists:get_value(index_name, IndexOptions),
  {algolia_client, ClientOptions} = proplists:get_value(client, IndexOptions),
  [ReadHost| _] = proplists:get_value(read_hosts, ClientOptions),
  [WriteHost | _] = proplists:get_value(write_hosts, ClientOptions),
  AppId = proplists:get_value(app_id, ClientOptions),
  ApiKey = proplists:get_value(api_key, ClientOptions),
  {IndexName, AppId, ApiKey, ReadHost, WriteHost}.

