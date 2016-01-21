-module(algolia_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("client.hrl").
-include("index.hrl").

create_client_test() ->
  Client = algolia:make_client("foo", "bar"),
  ?assert(is_function(Client#algolia_client.transport)).

init_index_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual("baz", Index#algolia_index.index_name).

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

list_indices_new_test() ->
  ExpectedRequest = {read, get, "/1/indexes"},
  ExpectedResult = {ok, #{<<"items">> => []}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  ?assertEqual(ExpectedResult, algolia:list_indices_new(Client)).

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
