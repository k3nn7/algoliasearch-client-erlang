-module(algolia_index_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

add_object_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "baz"),
  Object = {[
    {<<"content">>, <<"foo bar">>}
  ]},
  ?assertEqual(
    [
      {method, post},
      {url, "https://foo.algolia.net/1/indexes/baz"},
      {body, <<"{\"content\":\"foo bar\"}">>},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:add_object_request(Index, Object)
  ).

add_object_with_given_id_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "baz"),
  Object = {[
    {<<"objectID">>, <<"4321">>},
    {<<"content">>, <<"foo bar">>}
  ]},
  ?assertEqual(
    [
      {method, put},
      {url, "https://foo.algolia.net/1/indexes/baz/4321"},
      {body, <<"{\"objectID\":\"4321\",\"content\":\"foo bar\"}">>},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:add_object_request(Index, Object)
  ).

search_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(
    [
      {method, post},
      {url, "https://foo-dsn.algolia.net/1/indexes/baz/query"},
      {body, <<"{\"params\":{\"query\":\"foo\"}}">>},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:search_request(Index, <<"foo">>)
  ).
