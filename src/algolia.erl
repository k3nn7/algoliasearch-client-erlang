-module(algolia).

-export([make_client/2, init_index/2, list_indices/1]).

-include("client.hrl").
-include("index.hrl").

make_client(AppId, ApiKey) ->
  #algolia_client{
    transport = algolia_transport:make_transport(AppId, ApiKey)
  }.

init_index(Client, IndexName) ->
  #algolia_index{
    index_name = IndexName,
    client = Client
  }.

list_indices(Client) ->
  Path = lists:flatten(io_lib:format("/1/indexes", [])),
  Transport = Client#algolia_client.transport,
  Transport({read, get, Path}).
