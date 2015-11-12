-module(algolia_transport).

-export([build_request/5]).

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
