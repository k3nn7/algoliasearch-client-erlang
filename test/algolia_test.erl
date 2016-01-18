-module(algolia_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("client.hrl").
-include("index.hrl").

create_client_test() ->
  ?assertEqual(
    foo_client(),
    algolia:make_client("foo", "bar")
  ).

init_index_test() ->
  Client = algolia:make_client("foo", "bar"),
  ?assertEqual(
    baz_index(),
    algolia:init_index(Client, "baz")
  ).

list_indices_test() ->
  Client = algolia:make_client("foo", "bar"),
  ?assertEqual(
    [
      {method, get},
      {url, "https://foo-dsn.algolia.net/1/indexes"},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia:list_indices_request(Client)
  ).


foo_client() ->
  #algolia_client{
    app_id = "foo",
    api_key = "bar",
    read_hosts = [
      "foo-dsn.algolia.net",
      "foo-1.algolianet.com",
      "foo-2.algolianet.com",
      "foo-3.algolianet.com"
    ],
    write_hosts = [
      "foo.algolia.net",
      "foo-1.algolianet.com",
      "foo-2.algolianet.com",
      "foo-3.algolianet.com"
    ]
  }.

baz_index() ->
  #algolia_index{
    index_name = "baz",
    client = foo_client()
  }.
