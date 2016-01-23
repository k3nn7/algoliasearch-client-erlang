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
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "abc xyz"),
  ?assertEqual(
    [
      {method, put},
      {url, "https://foo.algolia.net/1/indexes/abc%20xyz/settings"},
      {body, <<"{\"hitsPerPage\":50,\"attributesToIndex\":[\"name\",\"email\"]}">>},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:set_settings_request(Index, {[
      {<<"hitsPerPage">>, 50},
      {<<"attributesToIndex">>, [<<"name">>, <<"email">>]}
    ]})
  ).

delete_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "abc xyz"),
  ?assertEqual(
    [
      {method, delete},
      {url, "https://foo.algolia.net/1/indexes/abc%20xyz"},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:delete_request(Index)
  ).

clear_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "abc xyz"),
  ?assertEqual(
    [
      {method, post},
      {url, "https://foo.algolia.net/1/indexes/abc%20xyz/clear"},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:clear_request(Index)
  ).
