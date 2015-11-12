-module(algolia_transport_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

build_request_test() ->
  RequestBody = {
    [{<<"foo">>, <<"bar">>}]
  },
  ?assertEqual(
    [
      {method, post},
      {url, "https://foo-dsn.algolia.net/1/indexes/bar/123"},
      {body, <<"{\"foo\":\"bar\"}">>},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "abc"},
        {"X-Algolia-API-Key", "def"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_transport:build_request(
      post,
      "foo-dsn.algolia.net",
      "/1/indexes/bar/123",
      RequestBody,
      "abc",
      "def"
    )
  ).
