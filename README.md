# Algolia Search API Client for Erlang

[![Build Status](https://travis-ci.org/k3nn7/algoliasearch-client-erlang.svg)](https://travis-ci.org/k3nn7/algoliasearch-client-erlang)

Setup
-----

Add to `deps` section in your `rebar.config`:
```erlang
{algolia, ".*", {git, "git://github.com/k3nn7/algoliasearch-client-erlang.git", "017332f00e4471adb5b3d5b4e064d2513e15bf02"}}
```

And run:
```bash
$ rebar get-deps
```

Quick Start
-----------
This library uses `ibrowse` and `ssl` applications so start them first:
```
1> ibrowse:start().
{ok,<0.52.0>}
2> ssl:start().
ok
```

Adding objects to index:

```erlang
Object = {[{<<"objectID">>, <<"1234">>}, {<<"content">>, <<"foo bar">>}]},
Client = algolia:make_client("YourApplicationID", "YourAPIKey"),
Index = algolia:init_index(Client, "IndexName"),
algolia_index:add_object(Index, Object).
```

Updating existing object:
```erlang
Object = {[{<<"objectID">>, <<"1234">>}, {<<"content">>, <<"foo bar">>}]},
Client = algolia:make_client("YourApplicationID", "YourAPIKey"),
Index = algolia:init_index(Client, "IndexName"),
algolia_index:update_object(Index, Object).
```

Partially update existing object:
```erlang
Object = {[{<<"objectID">>, <<"1234">>}, {<<"content">>, <<"foo bar">>}]},
Client = algolia:make_client("YourApplicationID", "YourAPIKey"),
Index = algolia:init_index(Client, "IndexName"),
algolia_index:update_object(Index, Object).
```

Querying index:

```erlang
Results = algolia_index:search(Index, <<"foo">>).
{ok,{[{<<"hits">>,
       [{[{<<"content">>,<<"foo">>},
          {<<"objectID">>,<<"53383650">>},
          {<<"_highlightResult">>,
           {[{<<"content">>,
              {[{<<"value">>,<<"<em>foo</em>">>},
                {<<"matchLevel">>,<<"full">>},
                {<<"matchedWords">>,[<<"foo">>]}]}}]}}]},
        {[{<<"content">>,<<"foo">>},
          {<<"objectID">>,<<"1234">>},
          {<<"_highlightResult">>,
           {[{<<"content">>,
              {[{<<"value">>,<<"<em>foo</em>">>},
                {<<"matchLevel">>,<<"full">>},
                {<<"matchedWords">>,[<<"foo">>]}]}}]}}]}]},
      {<<"nbHits">>,2},
      {<<"page">>,0},
      {<<"nbPages">>,1},
      {<<"hitsPerPage">>,20},
      {<<"processingTimeMS">>,1},
      {<<"query">>,<<"foo">>},
      {<<"params">>,<<"query=foo">>}]}}
```

Query index with additional parameters:
```erlang
Results = algolia_index:search(Index, <<"foo">>, {[
      {<<"queryType">>, <<"prefixAll">>},
      {<<"hitsPerPage">>, 10},
      {<<"getRankingInfo">>, 1}

]}).
```

Delete an object:
```erlang
Client = algolia:make_client("YourApplicationID", "YourAPIKey"),
Index = algolia:init_index(Client, "IndexName"),
ObjectID = <<"1234">>,
algolia_index:delete_object(Index, ObjectID).
```

Get an object:
```erlang
Client = algolia:make_client("YourApplicationID", "YourAPIKey"),
Index = algolia:init_index(Client, "IndexName"),
ObjectID = <<"1234">>,
% Retrieves all attributes
algolia_index:get_object(Index, ObjectID).
% Retrieves name and age
algolia_index:get_object(Index, ObjectID, <<"name,age">>).
```

Getting index settings:
```erlang
Client = algolia:make_client("YourApplicationID", "YourAPIKey"),
Index = algolia:init_index(Client, "IndexName"),
Settings = algolia_index:get_settings(Index).
{ok,{[{<<"minWordSizefor1Typo">>,4},
      {<<"minWordSizefor2Typos">>,8},
      {<<"hitsPerPage">>,20},
      {<<"maxValuesPerFacet">>,100},
      {<<"attributesToIndex">>,null},
      {<<"numericAttributesToIndex">>,null},
      {<<"attributesToRetrieve">>,null},
      {<<"unretrievableAttributes">>,null},
      {<<"optionalWords">>,null},
      {<<"attributesForFaceting">>,null},
      {<<"attributesToSnippet">>,null},
      {<<"attributesToHighlight">>,null},
      {<<"attributeForDistinct">>,null},
      {<<"ranking">>,
       [<<"typo">>,<<"geo">>,<<"words">>,<<"proximity">>,<<"attribute">>,
        <<"exact">>,<<"custom">>]},
      {<<"customRanking">>,[<<"desc(content)">>]},
      {<<"separatorsToIndex">>,<<>>},
      {<<"removeWordsIfNoResults">>,<<"none">>},
      {<<"queryType">>,<<"prefixLast">>},
      {<<"highlightPreTag">>,<<"<em>">>},
      {<<"highlightPostTag">>,<<"</em>">>}]}}
```

Change index settings:
```erlang
Client = algolia:make_client("YourApplicationID", "YourAPIKey"),
Index = algolia:init_index(Client, "IndexName"),
Settings = {[{<<"customRanking">>, [<<"desc(content)">>]}]},
algolia_index:set_settings(Index, Settings).
```

List indices
```erlang
Client = algolia:make_client("YourApplicationID", "YourAPIKey"),
algolia:list_indices(Client)
```

Testing
-------
```bash
$ rebar eu
```
