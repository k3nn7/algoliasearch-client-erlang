-module(algolia_new_transport_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

build_read_request_test() ->
  RequestBuilder = algolia_new_transport:make_request_builder("abc", "def"),
  Request = {read, get, "/1/indexes/bar/123"},
  ExpectedBuildedRequest = [
      {method, get},
      {url, "https://abc-dsn.algolia.net/1/indexes/bar/123"},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "abc"},
        {"X-Algolia-API-Key", "def"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
  BuildedRequest = RequestBuilder(Request),
  ?assertEqual(
    ExpectedBuildedRequest,
    BuildedRequest
  ).

build_write_request_test() ->
  RequestBuilder = algolia_new_transport:make_request_builder("abc", "def"),
  Request = {write, post, "/1/indexes/bar/123", #{<<"foo">> => <<"bar">>}},
  ExpectedBuildedRequest = [
    {method, post},
    {url, "https://abc.algolia.net/1/indexes/bar/123"},
    {headers, [
      {"Content-Type", "application/json; charset=utf-8"},
      {"X-Algolia-Application-Id", "abc"},
      {"X-Algolia-API-Key", "def"},
      {"Connection", "keep-alive"},
      {"User-Agent", "Algolia for Erlang"}
    ]},
    {body, <<"{\"foo\":\"bar\"}">>}
  ],
  BuildedRequest = RequestBuilder(Request),
  ?assertEqual(
    ExpectedBuildedRequest,
    BuildedRequest
  ).
