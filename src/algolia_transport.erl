-module(algolia_transport).

-export([make_transport/2, handle_response/1, make_request_builder/2]).

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

-spec make_transport(string(), string()) -> fun().
make_transport(AppId, ApiKey) ->
  HttpRequestBuilder = make_request_builder(AppId, ApiKey),
  fun(Request) ->
    handle_response(
      do_request(
        HttpRequestBuilder(Request)
      )
    )
  end.

-spec make_request_builder(string(), string()) -> fun().
make_request_builder(AppId, ApiKey) ->
  fun
    ({WhichHost, Method, Path}) ->
      build_request(AppId, ApiKey, WhichHost, Method, Path);
    ({WhichHost, Method, Path, Body}) ->
      build_request(AppId, ApiKey, WhichHost, Method, Path, Body)
  end.

build_request(AppId, ApiKey, WhichHost, Method, Path) ->
  Host = get_host(WhichHost, AppId),
  Url = lists:flatten(io_lib:format("https://~s~s", [Host, Path])),
  Headers = build_headers(AppId, ApiKey),
  [
    {method, Method},
    {url, Url},
    {headers, Headers}
  ].

build_request(AppId, ApiKey, WhichHost, Method, Path, Body) ->
  Request = build_request(AppId, ApiKey, WhichHost, Method, Path),
  EncodedBody = jiffy:encode(Body),
  lists:append(Request, [{body, EncodedBody}]).

get_host(read, AppId) ->
  [Host | _] = lists:map(
    fun(HostFormat) -> lists:flatten(io_lib:format(HostFormat, [AppId])) end,
    ?readHosts
  ),
  Host;
get_host(write, AppId) ->
  [Host | _] = lists:map(
    fun(HostFormat) -> lists:flatten(io_lib:format(HostFormat, [AppId])) end,
    ?writeHosts
  ),
  Host.

build_headers(AppId, ApiKey) ->
  [
    {"Content-Type", "application/json; charset=utf-8"},
    {"X-Algolia-Application-Id", AppId},
    {"X-Algolia-API-Key", ApiKey},
    {"Connection", "keep-alive"},
    {"User-Agent", "Algolia for Erlang"}
  ].

handle_response({ok, Code, _Headers, Body}) ->
  handle_http_result(list_to_integer(Code), Body);
handle_response({error, Reason}) ->
  {error, Reason}.

handle_http_result(Code, Body) when ((Code >= 200) and (Code < 300)) ->
  try jiffy:decode(list_to_binary(Body), [return_maps]) of
    DecodedBody ->
      {ok, DecodedBody}
  catch
    _:_ ->
      {error, invalid_json}
  end;

handle_http_result(_Code, Body) ->
  {error, Body}.

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
