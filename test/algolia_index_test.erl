-module(algolia_index_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

add_object_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(
    [
      {method, post},
      {url, "https://foo.algolia.net/1/indexes/baz/123"},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:add_object(Index, "abc")
  ).
