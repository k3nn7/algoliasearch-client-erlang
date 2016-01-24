# Algolia Search API Client for Erlang

[![Build Status](https://travis-ci.org/k3nn7/algoliasearch-client-erlang.svg)](https://travis-ci.org/k3nn7/algoliasearch-client-erlang)

Setup
-----

Add to `deps` section in your `rebar.config`:
```erlang
{algolia, ".*", {git, "git://github.com/k3nn7/algoliasearch-client-erlang.git", {tag, "0.1.0"}}}
```

And run:
```bash
$ rebar get-deps
```

This library uses `ibrowse` and `ssl` applications so start them first:
```erlang
1> ibrowse:start().
{ok,<0.52.0>}
2> ssl:start().
ok
```

After that initialize client with you ApplicationID and API-Key:

```erlang
Client = algolia:make_client("YourApplicationID", "YourAPIKey").
```


Quick Start
-----------

Use follow code to add object to `example` index:

```erlang
1> Index = algolia:init_index(Client, "example").
2> Object = #{<<"content">> => <<"foo">>}.
3> algolia_index:add_object(Index, Object).
4> {ok, #{<<"createdAt">> => <<"2016-01-24T08:34:47.700Z">>,
  <<"objectID">> => <<"129196290">>,
  <<"taskID">> => 699175850}}

```

Now you can query this index:

```erlang
5> algolia_index:search(Index, <<"foo">>).
6> {ok, #{<<"hits">> => [
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
    <<"query">> => <<"foo">>}}
```

Testing
-------
```bash
$ rebar eu
```
