-module(algolia_index).

-export([add_object/2]).

add_object(_Index = {algolia_index, IndexOptions}, Object = {ObjectPropList}) ->
  IndexName = proplists:get_value(index_name, IndexOptions),
  {algolia_client, ClientOptions} = proplists:get_value(client, IndexOptions),
  [WriteHost | _] = proplists:get_value(write_hosts, ClientOptions),
  AppId = proplists:get_value(app_id, ClientOptions),
  ApiKey = proplists:get_value(api_key, ClientOptions),

  case proplists:get_value(<<"objectID">>, ObjectPropList, false) of
    false ->
      Path = lists:flatten(io_lib:format("/1/indexes/~s", [IndexName])),
      algolia_transport:build_request(post, WriteHost, Path, Object, AppId, ApiKey);
    ObjectID ->
      Path = lists:flatten(io_lib:format("/1/indexes/~s/~s", [IndexName, ObjectID])),
      algolia_transport:build_request(put, WriteHost, Path, Object, AppId, ApiKey)
  end.
