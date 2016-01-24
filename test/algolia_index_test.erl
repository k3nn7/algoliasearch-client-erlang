-module(algolia_index_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

add_object_test() ->
  Object = #{<<"content">> => <<"foo bar">>},
  ExpectedRequest = {write, post, "/1/indexes/baz", Object},
  ExpectedResult = {ok,
    #{<<"createdAt">> => <<"2016-01-24T08:34:47.700Z">>,
      <<"objectID">> => <<"129196290">>,
      <<"taskID">> => 699175850}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:add_object(Index, Object)).

add_object_with_given_id_test() ->
  Object = #{
    <<"objectID">> => <<"4321 1234">>,
    <<"content">> => <<"foo bar">>
  },
  ExpectedRequest = {write, put, "/1/indexes/baz/4321%201234", Object},
  ExpectedResult = {ok,
    #{<<"createdAt">> => <<"2016-01-24T08:34:47.700Z">>,
      <<"objectID">> => <<"4321 1234">>,
      <<"taskID">> => 699175850}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:add_object(Index, Object)).

update_object_test() ->
  Object = #{
    <<"objectID">> => <<"4321 1234">>,
    <<"content">> => <<"foo bar">>
  },
  ExpectedRequest = {write, put, "/1/indexes/baz/4321%201234", Object},
  ExpectedResult = {ok,
    #{<<"objectID">> => <<"4321 1234">>,
      <<"taskID">> => 699180670,
      <<"updatedAt">> => <<"2016-01-24T08:37:05.242Z">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:update_object(Index, Object)).

update_object_with_escaped_id_test() ->
  Object = #{
    <<"objectID">> => <<"foo bar">>,
    <<"content">> => <<"foo bar">>
  },
  ExpectedRequest = {write, put, "/1/indexes/baz/foo%20bar", Object},
  ExpectedResult = {ok,
    #{<<"objectID">> => <<"4321">>,
      <<"taskID">> => 699180670,
      <<"updatedAt">> => <<"2016-01-24T08:37:05.242Z">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:update_object(Index, Object)).

partial_update_object_test() ->
  Object = #{
    <<"objectID">> => <<"4321 1234">>,
    <<"content">> => <<"foo bar">>
  },
  ExpectedRequest = {write, post, "/1/indexes/baz/4321%201234/partial", Object},
  ExpectedResult = {ok,
    #{<<"objectID">> => <<"4321 1234">>,
      <<"taskID">> => 699180670,
      <<"updatedAt">> => <<"2016-01-24T08:37:05.242Z">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:partial_update_object(Index, Object)).

delete_object_test() ->
  ExpectedRequest = {write, delete, "/1/indexes/baz/4321%201234"},
  ExpectedResult = {ok,
    #{<<"deletedAt">> => <<"2016-01-24T08:40:40.717Z">>,
      <<"objectID">> => <<"4321 1234">>,
      <<"taskID">> => 1012510111}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:delete_object(Index, <<"4321 1234">>)).

search_test() ->
  RequestBody = #{<<"params">> => <<"query=foo">>},
  ExpectedRequest = {read, post, "/1/indexes/baz/query", RequestBody},
  ExpectedResult = {ok, #{<<"hits">> => [
    #{<<"_highlightResult">> => #{<<"content">> => #{<<"matchLevel">> => <<"full">>,
      <<"matchedWords">> => [<<"foo">>],
      <<"value">> => <<"<em>foo</em>">>}},
      <<"content">> => <<"foo">>,
      <<"objectID">> => <<"53383650">>}],
    <<"hitsPerPage">> => 20,
    <<"nbHits">> => 1,
    <<"nbPages">> => 1,
    <<"page">> => 0,
    <<"params">> => <<"query=foo">>,
    <<"processingTimeMS">> => 1,
    <<"query">> => <<"foo">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:search(Index, <<"foo">>)).

search_encode_query_test() ->
  RequestBody = #{<<"params">> => <<"query=foo%20bar">>},
  ExpectedRequest = {read, post, "/1/indexes/baz/query", RequestBody},
  ExpectedResult = {ok, #{<<"hits">> => [
    #{<<"_highlightResult">> => #{<<"content">> => #{<<"matchLevel">> => <<"full">>,
      <<"matchedWords">> => [<<"foo">>, <<"bar">>],
      <<"value">> => <<"<em>foo</em> <em>bar</em>">>}},
      <<"content">> => <<"foo bar">>,
      <<"objectID">> => <<"129196290">>}],
    <<"hitsPerPage">> => 20,
    <<"nbHits">> => 1,
    <<"nbPages">> => 1,
    <<"page">> => 0,
    <<"params">> => <<"query=foo%20bar">>,
    <<"processingTimeMS">> => 1,
    <<"query">> => <<"foo bar">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:search(Index, <<"foo bar">>)).

search_with_additional_parameters_test() ->
  RequestBody = #{<<"params">> => <<"getRankingInfo=1&hitsPerPage=10&query=foo%20bar&queryType=prefixAll">>},
  ExpectedRequest = {read, post, "/1/indexes/baz/query", RequestBody},
  ExpectedResult = {ok, #{<<"hits">> => [
    #{<<"_highlightResult">> => #{<<"content">> => #{<<"matchLevel">> => <<"full">>,
      <<"matchedWords">> => [<<"foo">>, <<"bar">>],
      <<"value">> => <<"<em>foo</em> <em>bar</em>">>}},
      <<"content">> => <<"foo bar">>,
      <<"objectID">> => <<"129196290">>}],
    <<"hitsPerPage">> => 20,
    <<"nbHits">> => 1,
    <<"nbPages">> => 1,
    <<"page">> => 0,
    <<"params">> => <<"query=foo%20bar">>,
    <<"processingTimeMS">> => 1,
    <<"query">> => <<"foo bar">>}},
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
  ExpectedRequest = {read, get, "/1/indexes/baz/4321%201234"},
  ExpectedResult = {ok,
    #{<<"content">> => <<"foo bar">>, <<"objectID">> => <<"4321 1234">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:get_object(Index, <<"4321 1234">>)).

get_object_with_attributes_test() ->
  ExpectedRequest = {read, get, "/1/indexes/baz/4321%201234?attribute=name%2Cage"},
  ExpectedResult = {ok,
    #{<<"content">> => <<"foo bar">>, <<"objectID">> => <<"4321 1234">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:get_object(Index, <<"4321 1234">>, <<"name,age">>)).

get_settings_test() ->
  ExpectedRequest = {read, get, "/1/indexes/baz/settings"},
  ExpectedResult = {ok,
    #{<<"attributeForDistinct">> => null,
      <<"snippetEllipsisText">> => <<>>,
      <<"unretrievableAttributes">> => null}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:get_settings(Index)).

set_settings_test() ->
  RequestBody = #{
    <<"hitsPerPage">> => 50,
    <<"attributesToIndex">> => [<<"name">>, <<"email">>]
  },
  ExpectedRequest = {write, put, "/1/indexes/baz/settings", RequestBody},
  ExpectedResult = {ok,
    #{<<"taskID">> => 699197950, <<"updatedAt">> => <<"2016-01-24T08:55:13.504Z">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:set_settings(Index, RequestBody)).

delete_test() ->
  ExpectedRequest = {write, delete, "/1/indexes/baz"},
  ExpectedResult = {ok,
    #{<<"deletedAt">> => <<"2016-01-24T08:57:49.752Z">>, <<"taskID">> => 699201240}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:delete(Index)).

delete_escaped_index_name_test() ->
  ExpectedRequest = {write, delete, "/1/indexes/foo%20bar"},
  ExpectedResult = {ok,
    #{<<"deletedAt">> => <<"2016-01-24T08:57:49.752Z">>, <<"taskID">> => 699201240}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "foo bar"),
  ?assertEqual(ExpectedResult, algolia_index:delete(Index)).

clear_test() ->
  ExpectedRequest = {write, post, "/1/indexes/baz/clear"},
  ExpectedResult = {ok,
    #{<<"taskID">> => 699197950, <<"updatedAt">> => <<"2016-01-24T08:55:13.504Z">>}},
  Client = algolia_mock_client:make(ExpectedRequest, ExpectedResult),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(ExpectedResult, algolia_index:clear(Index)).
