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
  ExpectedResult = {ok,
    #{<<"items">> => [
      #{<<"createdAt">> => <<"2015-11-15T13:14:22.878Z">>,
        <<"dataSize">> => 221,
        <<"entries">> => 13,
        <<"fileSize">> => 970,
        <<"lastBuildTimeS">> => 0,
        <<"name">> => <<"Erlang">>,
        <<"numberOfPendingTask">> => 0,
        <<"pendingTask">> => false,
        <<"updatedAt">> => <<"2016-01-10T23:11:41.540Z">>}],
      <<"nbPages">> => 1}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  ?assertEqual(ExpectedResult, algolia:list_indices(Client)).
