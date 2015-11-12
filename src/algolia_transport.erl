-module(algolia_transport).

-export([build_request/6]).

build_request(Method, Host, Path, Body, AppId, ApiKey) ->
  Url = lists:flatten(io_lib:format("https://~s~s", [Host, Path])),
  Headers = build_headers(AppId, ApiKey),
  EncodedBody = jiffy:encode(Body),
  [
    {method, Method},
    {url, Url},
    {body, EncodedBody},
    {headers, Headers}
  ].

build_headers(AppId, ApiKey) ->
  [
    {"Content-Type", "application/json; charset=utf-8"},
    {"X-Algolia-Application-Id", AppId},
    {"X-Algolia-API-Key", ApiKey},
    {"Connection", "keep-alive"},
    {"User-Agent", "Algolia for Erlang"}
  ].
