-module(algolia).

-export([make_client/2, init_index/2, list_indices/1]).

-include("client.hrl").
-include("index.hrl").

-spec(make_client/2 :: (string(), string()) -> #algolia_client{}).
make_client(AppId, ApiKey) ->
  #algolia_client{
    transport = algolia_transport:make_transport(AppId, ApiKey)
  }.

init_index(Client, IndexName) ->
  #algolia_index{
    index_name = http_uri:encode(IndexName),
    client = Client
  }.

-type(response() :: {ok, map()} | {error, any()}).

-spec(list_indices/1 :: (#algolia_client{}) -> response()).
list_indices(Client) ->
  Path = lists:flatten(io_lib:format("/1/indexes", [])),
  Transport = Client#algolia_client.transport,
  Transport({read, get, Path}).
