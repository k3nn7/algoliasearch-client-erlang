# Algolia Search API Client for Erlang

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

Testing
-------
```bash
$ rebar eu
```
