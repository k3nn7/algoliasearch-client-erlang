-module(algolia_index_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

add_object_test() ->
  Object = #{<<"content">> => <<"foo bar">>},
  ExpectedRequest = {write, post, "/1/indexes/baz", Object},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:add_object(Index, Object)).

add_object_with_given_id_test() ->
  Object = #{
    <<"objectID">> => <<"4321">>,
    <<"content">> => <<"foo bar">>
  },
  ExpectedRequest = {write, put, "/1/indexes/baz/4321", Object},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:add_object(Index, Object)).

update_object_test() ->
  Object = #{
    <<"objectID">> => <<"4321">>,
    <<"content">> => <<"foo bar">>
  },
  ExpectedRequest = {write, put, "/1/indexes/baz/4321", Object},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:update_object(Index, Object)).

partial_update_object_test() ->
  Object = #{
    <<"objectID">> => <<"4321">>,
    <<"content">> => <<"foo bar">>
  },
  ExpectedRequest = {write, post, "/1/indexes/baz/4321/partial", Object},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:partial_update_object(Index, Object)).

delete_object_test() ->
  ExpectedRequest = {write, delete, "/1/indexes/baz/4321"},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:delete_object(Index, <<"4321">>)).

search_test() ->
  RequestBody = #{<<"params">> => <<"query=foo">>},
  ExpectedRequest = {read, post, "/1/indexes/baz/query", RequestBody},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:search(Index, <<"foo">>)).

search_encode_query_test() ->
  RequestBody = #{<<"params">> => <<"query=foo%20bar">>},
  ExpectedRequest = {read, post, "/1/indexes/baz/query", RequestBody},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:search(Index, <<"foo bar">>)).

search_with_additional_parameters_test() ->
  RequestBody = #{<<"params">> => <<"getRankingInfo=1&hitsPerPage=10&query=foo%20bar&queryType=prefixAll">>},
  ExpectedRequest = {read, post, "/1/indexes/baz/query", RequestBody},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(
    ExpectedResult,
    algolia_index:search(Index, <<"foo bar">>, #{
      <<"queryType">> => <<"prefixAll">>,
      <<"hitsPerPage">> => 10,
      <<"getRankingInfo">> => 1
    })
  ).

get_object_test() ->
  ExpectedRequest = {read, get, "/1/indexes/baz/4321"},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:get_object(Index, <<"4321">>)).

get_object_with_attributes_test() ->
  ExpectedRequest = {read, get, "/1/indexes/baz/4321?attribute=name%2Cage"},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:get_object(Index, <<"4321">>, <<"name,age">>)).

get_settings_test() ->
  ExpectedRequest = {read, get, "/1/indexes/baz/settings"},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:get_settings(Index)).

set_settings_test() ->
  RequestBody = #{
    <<"hitsPerPage">> => 50,
    <<"attributesToIndex">> => [<<"name">>, <<"email">>]
  },
  ExpectedRequest = {write, put, "/1/indexes/baz/settings", RequestBody},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:set_settings(Index, RequestBody)).

delete_test() ->
  ExpectedRequest = {write, delete, "/1/indexes/baz"},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:delete(Index)).

clear_test() ->
  ExpectedRequest = {write, post, "/1/indexes/baz/clear"},
  ExpectedResult = {ok, #{<<"objectID">> => <<"213">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:clear(Index)).
