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

build_request_without_body_test() ->
  ?assertEqual(
    [
      {method, get},
      {url, "https://foo-dsn.algolia.net/1/indexes/bar/123"},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "abc"},
        {"X-Algolia-API-Key", "def"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_transport:build_request(
      get,
      "foo-dsn.algolia.net",
      "/1/indexes/bar/123",
      "abc",
      "def"
    )
  ).


handle_valid_response_test() ->
  HttpResponse = {ok, "200", [], "{\"key\":\"val\"}"},
  ExpectedResult = {ok, {[{<<"key">>, <<"val">>}]}},
  ?assertEqual(
    ExpectedResult,
    algolia_transport:handle_response(HttpResponse)
  ).

handle_invalid_json_response_test() ->
  HttpResponse = {ok, "200", [], "foo-!@"},
  ExpectedResult = {error, invalid_json},
  ?assertEqual(
    ExpectedResult,
    algolia_transport:handle_response(HttpResponse)
  ).

handle_http_error_test() ->
  HttpResponse = {ok, "500", [], "internal error"},
  ExpectedResult = {error, "internal error"},
  ?assertEqual(
    ExpectedResult,
    algolia_transport:handle_response(HttpResponse)
  ).
