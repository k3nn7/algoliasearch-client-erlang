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
  ExpectedRequest = {read, get, "/1/indexes"},
  ExpectedResult = {ok, #{<<"items">> => []}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  ?assertEqual(ExpectedResult, algolia:list_indices(Client)).
