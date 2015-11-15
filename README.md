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
```

Testing
-------
```bash
$ rebar eu
```

To do (iteration #1)
----
- [x] Initializing client
- [x] Initializing request
- [x] Building request
- [x] Making request over HTTP
- [x] Logic for querying index
- [x] Logic for feeding index
- [ ] Logic for parsing response
