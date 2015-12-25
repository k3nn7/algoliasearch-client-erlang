-module(algolia_transport).

-export([build_request/6, build_request/5, do_request/1, handle_response/1]).

build_request(Method, Host, Path, AppId, ApiKey) ->
  Url = lists:flatten(io_lib:format("https://~s~s", [Host, Path])),
  Headers = build_headers(AppId, ApiKey),
  [
    {method, Method},
    {url, Url},
    {headers, Headers}
  ].

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

do_request(Request) ->
  Url = proplists:get_value(url, Request),
  Headers = proplists:get_value(headers, Request),
  Method = proplists:get_value(method, Request),
  Body = proplists:get_value(body, Request, false),
  case Body of
    false ->
      ibrowse:send_req(Url, Headers, Method);
    Body ->
      ibrowse:send_req(Url, Headers, Method, Body)
  end.

handle_response({ok, Code, _Headers, Body}) ->
  handle_http_result(list_to_integer(Code), Body).

handle_http_result(Code, Body) when ((Code >= 200) and (Code < 300)) ->
  try jiffy:decode(list_to_binary(Body)) of
    DecodedBody ->
      {ok, DecodedBody}
  catch
    _:_ ->
      {error, invalid_json}
  end;

handle_http_result(_Code, Body) ->
  {error, Body}.
