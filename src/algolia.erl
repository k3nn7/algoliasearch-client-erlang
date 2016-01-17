-module(algolia).

-export([make_client/2, init_index/2, list_indices/1]).
-export([list_indices_request/1]).

-define(readHosts, [
  "~s-dsn.algolia.net",
  "~s-1.algolianet.com",
  "~s-2.algolianet.com",
  "~s-3.algolianet.com"
]).
-define(writeHosts, [
  "~s.algolia.net",
  "~s-1.algolianet.com",
  "~s-2.algolianet.com",
  "~s-3.algolianet.com"
]).

make_client(AppId, ApiKey) ->
  ReadHosts = lists:map(
    fun(HostFormat) -> lists:flatten(io_lib:format(HostFormat, [AppId])) end,
    ?readHosts
  ),
  WriteHosts = lists:map(
    fun(HostFormat) -> lists:flatten(io_lib:format(HostFormat, [AppId])) end,
    ?writeHosts
  ),
  {algolia_client, [
    {app_id, AppId},
    {api_key, ApiKey},
    {read_hosts, ReadHosts},
    {write_hosts, WriteHosts}
  ]}.

init_index(Client, IndexName) ->
  {algolia_index, [
    {index_name, IndexName},
    {client, Client}
  ]}.

list_indices(Client) ->
  algolia_transport:handle_response(
    algolia_transport:do_request(list_indices_request(Client))).

list_indices_request(Client) ->
  {AppId, ApiKey, ReadHost, _} = get_client_options(Client),
  Path = lists:flatten(io_lib:format("/1/indexes", [])),
  algolia_transport:build_request(get, ReadHost, Path, AppId, ApiKey).

get_client_options(_Client = {algolia_client, ClientOptions}) ->
  [ReadHost | _] = proplists:get_value(read_hosts, ClientOptions),
  [WriteHost | _] = proplists:get_value(write_hosts, ClientOptions),
  AppId = proplists:get_value(app_id, ClientOptions),
  ApiKey = proplists:get_value(api_key, ClientOptions),
  {AppId, ApiKey, ReadHost, WriteHost}.
