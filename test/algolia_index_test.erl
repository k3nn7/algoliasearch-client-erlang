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

update_object_test() ->
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
    algolia_index:update_object_request(Index, Object)
  ).

partial_update_object_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "baz"),
  Object = {[
    {<<"objectID">>, <<"4321">>},
    {<<"content">>, <<"foo bar">>}
  ]},
  ?assertEqual(
    [
      {method, post},
      {url, "https://foo.algolia.net/1/indexes/baz/4321/partial"},
      {body, <<"{\"objectID\":\"4321\",\"content\":\"foo bar\"}">>},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:partial_update_object_request(Index, Object)
  ).

delete_object_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "baz"),
  ObjectId = <<"12345">>,
  ?assertEqual(
    [
      {method, delete},
      {url, "https://foo.algolia.net/1/indexes/baz/12345"},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:delete_object_request(Index, ObjectId)
  ).

search_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(
    [
      {method, post},
      {url, "https://foo-dsn.algolia.net/1/indexes/baz/query"},
      {body, <<"{\"params\":\"query=foo\"}">>},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:search_request(Index, <<"foo">>, {[]})
  ).

search_other_query_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(
    [
      {method, post},
      {url, "https://foo-dsn.algolia.net/1/indexes/baz/query"},
      {body, <<"{\"params\":\"query=foo%20bar\"}">>},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:search_request(Index, <<"foo bar">>, {[]})
  ).

search_with_additional_parameters_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(
    [
      {method, post},
      {url, "https://foo-dsn.algolia.net/1/indexes/baz/query"},
      {body, <<"{\"params\":\"queryType=prefixAll&hitsPerPage=10&getRankingInfo=1&query=foo\"}">>},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:search_request(Index, <<"foo">>, {[
      {<<"queryType">>, <<"prefixAll">>},
      {<<"hitsPerPage">>, 10},
      {<<"getRankingInfo">>, 1}
    ]})
  ).

get_object_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(
    [
      {method, get},
      {url, "https://foo-dsn.algolia.net/1/indexes/baz/4321"},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:get_object_request(Index, <<"4321">>)
  ).

get_object_with_attributes_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "baz"),
  ?assertEqual(
    [
      {method, get},
      {url, "https://foo-dsn.algolia.net/1/indexes/baz/4321?attribute=name%2Cage"},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:get_object_request(Index, <<"4321">>, <<"name,age">>)
  ).

get_settings_test() ->
  Client = algolia:make_client("foo", "bar"),
  Index = algolia:init_index(Client, "abc xyz"),
  ?assertEqual(
    [
      {method, get},
      {url, "https://foo-dsn.algolia.net/1/indexes/abc%20xyz/settings"},
      {headers, [
        {"Content-Type", "application/json; charset=utf-8"},
        {"X-Algolia-Application-Id", "foo"},
        {"X-Algolia-API-Key", "bar"},
        {"Connection", "keep-alive"},
        {"User-Agent", "Algolia for Erlang"}
      ]}
    ],
    algolia_index:get_settings_request(Index)
  ).

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
