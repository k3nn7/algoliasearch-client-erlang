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
{ok, #{<<"createdAt">> => <<"2016-01-24T08:34:47.700Z">>,
  <<"objectID">> => <<"129196290">>,
  <<"taskID">> => 699175850}}

```

Now you can query this index:

```erlang
4> algolia_index:search(Index, <<"foo">>).
{ok, #{<<"hits">> => [
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

Commands reference
------------------

It is just an overview of functions that this library provides. For more detailed description of parameters that you can use please see official [Algolia REST API Reference](https://www.algolia.com/doc/rest).

### Add new object to the index

Example with automatic `objectID` assignment:

```erlang
1> Object = #{<<"content">> => <<"foo">>}.
2> algolia_index:add_object(Index, Object).
```

Add object with given `objectID`:

```erlang
1> Object = #{<<"objectID">> => <<"1234">>, <<"content">> => <<"foo">>}.
2> algolia_index:add_object(Index, Object).
```

### Update an existing object in the index

Replace all object attributes:

```erlang
1> Object = #{<<"objectID">> => <<"1234">>, <<"content">> => <<"foo">>}.
2> algolia_index:update_object(Index, Object).
```

Update some object attributes (partial update):

```erlang
1> Object = #{<<"objectID">> => <<"1234">>, <<"content">> => <<"foo">>}.
2> algolia_index:partial_update(Index, Object).
```

Apply operation on object attributes:

```erlang
1> Object = #{
  <<"objectID">> => <<"1234">>,
  <<"price">> => #{
   <<"_operation">> => <<"increment">>,
   <<"value">> => 10
 }}.
2> algolia_index:partial_update(Index, Object).
```

### Search

Search without additional parameters:

```erlang
1> algolia_index:search(Index, <<"foo bar">>).
```

The server response will look like:

```erlang
{ok, #{<<"hits">> => [
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

You can customize your search by providing additional parameters (see official [REST API Reference](https://www.algolia.com/doc/rest#full-text-search-parameters)) for comprehensive list of available parameters.
Example:

```erlang
1> algolia_index:search(
  Index,
  <<"foo bar">>,
  #{<<"hitsPerPage">> => 10}
  ).
```

### Get an object

You can get an object using its `objectID`:

```erlang
1> algolia_index:get_object(Index, <<"4321">>).
```

Optionally you can specify comma separated list of attributes that you want to receive:
```erlang
1> algolia_index:get_object(Index, <<"4321">>, <<"name,age">>).
```

### Delete on object

You can delete object by its `objectID`:

```erlang
1> algolia_index:delete_object(Index, <<"4321">>).
```

### Get index settings

You can receive all index settings by calling:

```erlang
1> algolia_index:get_settings(Index).
```

The server response will look like (fragment, complete list of fields with their description can be bound in official [REST API reference](https://www.algolia.com/doc/rest#get-index-settings)):

```erlang
{ok, #{<<"attributeForDistinct">> => null,
  ...
  <<"snippetEllipsisText">> => <<>>,
  <<"unretrievableAttributes">> => null}},
```

### Set index settings

To change index settings call:

```erlang
1> algolia_index:set_settings(Index, #{
    <<"hitsPerPage">> => 50,
    <<"attributesToIndex">> => [<<"name">>, <<"email">>]
  }).
```

### List indices

You can get list of all your indices together with their associated informations.
Notice that this method as a parameter takes `Client` instead of `Index`:

```erlang
1> algolia:list_indices(Client).
```

### Delete an index

To delete given index call:

```erlang
1> algolia_index:delete(Index).
```

### Clear an index

To delete index contents (index settings won't be removed'):

```erlang
1> algolia_index:clear(Index).
```

Testing
-------
```bash
$ rebar eu
```
