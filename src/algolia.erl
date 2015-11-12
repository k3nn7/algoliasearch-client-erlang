-module(algolia).

-export([make_client/2, init_index/2, build_request/5]).

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

build_request(Method, Host, Path, AppId, ApiKey) ->
  Url = lists:flatten(io_lib:format("https://~s~s", [Host, Path])),
  [
    {method, Method},
    {url, Url},
    {headers, [
      {"Content-Type", "application/json; charset=utf-8"},
      {"X-Algolia-Application-Id", AppId},
      {"X-Algolia-API-Key", ApiKey},
      {"Connection", "keep-alive"},
      {"User-Agent", "Algolia for Erlang"}
    ]}
  ].

init_index(Client, IndexName) ->
  {algolia_index, [
    {index_name, IndexName},
    {client, Client}
  ]}.
