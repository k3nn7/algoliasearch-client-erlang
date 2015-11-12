-module(algolia_index).

-export([add_object/2]).

add_object(Index = {algolia_index, IndexOptions}, Object) ->
  IndexName = proplists:get_value(index_name, IndexOptions),
  {algolia_client, ClientOptions} = proplists:get_value(client, IndexOptions),
  [WriteHost | _] = proplists:get_value(write_hosts, ClientOptions),
  Path = lists:flatten(io_lib:format("/1/indexes/~s/123", [IndexName])),
  AppId = proplists:get_value(app_id, ClientOptions),
  ApiKey = proplists:get_value(api_key, ClientOptions),
  algolia_transport:build_request(post, WriteHost, Path, AppId, ApiKey).
